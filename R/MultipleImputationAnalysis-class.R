#' @title Multiple imputation analysis using Rubin's combining rules.
#' @description
#' R6 class that runs the full EPIMIGHT heritability and genetic correlation
#' pipeline K times with independent Bernoulli downsample draws, then combines
#' the K meta-analyzed estimates via Rubin's multiple imputation rules.
#'
#' When K = 1, the class degrades to a single-draw analysis (no Rubin
#' combination) and returns the single resample's meta-analysis as-is.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @export
MultipleImputationAnalysis <- R6::R6Class( #nolint
  "MultipleImputationAnalysis",
  inherit = Analysis,
  private = list(
    c1_tte = NULL,
    relationship_kind = NULL,
    K = NULL,
    seed = NULL,
    d1_earliest_onset = NULL,
    d2_earliest_onset = NULL,
    cif_analysis = NULL,
    h2_analysis = NULL,
    gc_analysis = NULL,
    re_d1_c1 = NULL,
    re_d2_c1 = NULL,

    #' Run CIF stratified by born_at_year for one disorder in one cohort.
    run_cif_stratified = function(disorder, tte_df, earliest_onset) {
      status_col <- paste0(disorder, "_failure_status")
      time_col   <- paste0(disorder, "_failure_time")

      tte <- tte_df |>
        rename(
          failure_status = !!as.name(status_col),
          failure_time   = !!as.name(time_col)
        ) |>
        as.data.table()

      private$cif_analysis$run(
        tte            = tte,
        earliest_onset = earliest_onset,
        group_columns  = list("born_at_year")
      )
    },

    #' Run heritability from paired CIF estimates.
    run_h2_stratified = function(c1_estimates, c2_estimates) {
      combined_estimates <- c1_estimates |>
        inner_join(c2_estimates, by = join_by(time, born_at_year)) |>
        rename(
          cohort1_estimates = estimate.x,
          cohort1_cases     = cases.x,
          cohort2_estimates = estimate.y,
          cohort2_cases     = cases.y
        ) |>
        select(time, born_at_year, starts_with("cohort"))

      private$h2_analysis$run(
        relationship_kind = private$relationship_kind,
        estimates         = combined_estimates
      )
    },

    #' Filter to maximum follow-up time per birth year.
    get_tmax_per_year = function(dt) {
      dt |>
        group_by(born_at_year) |>
        filter(time == max(time)) |>
        ungroup() |>
        arrange(born_at_year) |>
        as.data.table()
    },

    #' Run one complete analysis pipeline for resample k.
    #' Uses pre-computed c1 CIFs (private$re_d1_c1, private$re_d2_c1)
    #' to avoid redundant computation — these are identical across resamples.
    run_single_resample = function(k) {
      seed_d1 <- private$seed + k - 1L
      seed_d2 <- private$seed + private$K + k - 1L

      # Work on a copy to avoid mutating the original
      tte <- copy(private$c1_tte)

      # Bernoulli downsample
      tte[, d1_diagnosed_relatives := self$downsample_relatives(
        d1_diagnosed_relatives, d1_n_relatives, seed = seed_d1
      )]
      tte[, d2_diagnosed_relatives := self$downsample_relatives(
        d2_diagnosed_relatives, d2_n_relatives, seed = seed_d2
      )]

      # Split cohorts
      c2 <- tte[d1_diagnosed_relatives > 0]
      c3 <- tte[d2_diagnosed_relatives > 0]

      if (nrow(c2) == 0 || nrow(c3) == 0) {
        return(NULL)
      }

      # CIF: c1 is pre-computed, only c2/c3 vary per resample
      re_d1_c1 <- private$re_d1_c1
      re_d2_c1 <- private$re_d2_c1
      re_d1_c2 <- private$run_cif_stratified("d1", c2, private$d1_earliest_onset)
      re_d1_c3 <- private$run_cif_stratified("d1", c3, private$d1_earliest_onset)
      re_d2_c3 <- private$run_cif_stratified("d2", c3, private$d2_earliest_onset)

      if (is.null(re_d1_c2) || is.null(re_d1_c3) || is.null(re_d2_c3)) {
        return(NULL)
      }

      # Heritability
      h2_d1 <- private$run_h2_stratified(re_d1_c1, re_d1_c2)
      h2_d2 <- private$run_h2_stratified(re_d2_c1, re_d2_c3)

      if (nrow(h2_d1) == 0 || nrow(h2_d2) == 0) {
        return(NULL)
      }

      # tmax per year + meta
      h2_d1_tmax <- private$get_tmax_per_year(
        h2_d1[, .(born_at_year, time, h2, se, l95, u95)]
      )
      h2_d2_tmax <- private$get_tmax_per_year(
        h2_d2[, .(born_at_year, time, h2, se, l95, u95)]
      )

      if (nrow(h2_d1_tmax) == 0 || nrow(h2_d2_tmax) == 0) {
        return(NULL)
      }

      h2_d1_meta <- private$h2_analysis$run_meta(h2_d1_tmax)
      h2_d2_meta <- private$h2_analysis$run_meta(h2_d2_tmax)

      # Assemble GC input
      re_combined <- re_d1_c1[, .(time, born_at_year,
                                   re_d1_c1_estimates = estimate,
                                   re_d1_c1_cases     = cases)] |>
        merge(
          re_d1_c3[, .(time, born_at_year,
                        re_d1_c3_estimates = estimate,
                        re_d1_c3_cases     = cases)],
          by = c("time", "born_at_year")
        ) |>
        merge(
          re_d2_c1[, .(time, born_at_year,
                        re_d2_c1_estimates = estimate,
                        re_d2_c1_cases     = cases)],
          by = c("time", "born_at_year")
        )

      h2_combined <- merge(
        h2_d1[, .(time, born_at_year, h2_d1 = h2)],
        h2_d2[, .(time, born_at_year, h2_d2 = h2)],
        by = c("time", "born_at_year")
      )

      combined_full <- merge(re_combined, h2_combined,
                             by = c("time", "born_at_year"))
      setorder(combined_full, born_at_year, time)

      combined_tmax <- private$get_tmax_per_year(combined_full)

      if (nrow(combined_tmax) == 0) {
        return(NULL)
      }

      # Genetic correlation
      gc_tmax <- private$gc_analysis$run(
        relationship_kind = private$relationship_kind,
        estimates         = combined_tmax
      )

      if (nrow(gc_tmax) == 0) {
        return(NULL)
      }

      gc_meta <- private$gc_analysis$run_meta(gc_tmax)

      # Select GC tmax columns needed for per-year Rubin combination
      gc_tmax_out <- gc_tmax[, .(born_at_year, time, rhh, rhog, se, l95, u95)]

      list(
        h2_d1_meta = h2_d1_meta,
        h2_d2_meta = h2_d2_meta,
        gc_meta    = gc_meta,
        h2_d1_tmax = h2_d1_tmax,
        h2_d2_tmax = h2_d2_tmax,
        gc_tmax    = gc_tmax_out
      )
    },

    #' Combine K per-year estimates via Rubin, then meta-analyze.
    #' @param analysis_obj The analysis object to call run_meta on
    #'   (h2_analysis for heritability, gc_analysis for genetic correlation).
    rubin_then_meta = function(tmax_list, estimate_col, se_col, meta_col,
                               analysis_obj) {
      all_years <- rbindlist(tmax_list, idcol = "resample")
      years <- sort(unique(all_years$born_at_year))

      per_year_rubin <- rbindlist(lapply(years, function(yr) {
        yr_data <- all_years[born_at_year == yr]
        if (nrow(yr_data) < 2L) return(NULL)
        rubin <- self$combine_rubin(yr_data,
                                    estimate_column = estimate_col,
                                    se_column = se_col)
        rubin[, born_at_year := yr]
        rubin
      }))

      if (nrow(per_year_rubin) == 0L) {
        stop("No birth years had >= 2 successful resamples for Rubin combination")
      }

      # Rename for run_meta compatibility: meta_col → h2/rhh, fixed_se → se
      meta_input <- per_year_rubin[, .(born_at_year)]
      meta_input[[meta_col]] <- per_year_rubin$fixed_meta
      meta_input[["se"]] <- per_year_rubin$fixed_se

      meta_result <- analysis_obj$run_meta(meta_input)

      # Add Rubin diagnostics (averaged across birth years)
      meta_result[, `:=`(
        within_var  = mean(per_year_rubin$within_var),
        between_var = mean(per_year_rubin$between_var),
        total_var   = mean(per_year_rubin$total_var),
        b_over_t    = mean(per_year_rubin$b_over_t),
        k_resamples = per_year_rubin$k_resamples[1]
      )]

      meta_result
    }
  ),

  public = list(
    #' @description
    #' Create a new MultipleImputationAnalysis instance.
    #'
    #' @param c1_tte Pre-joined \code{data.table} with both traits' TTE data
    #'   and relative count columns.
    #' @param relationship_kind Relationship kind string (e.g. \code{"FS"}).
    #' @param K Number of MI resamples (default 20).
    #' @param seed Base random seed.
    #' @param d1_earliest_onset Earliest onset age for disorder 1 (default 1).
    #' @param d2_earliest_onset Earliest onset age for disorder 2 (default 1).
    initialize = function(c1_tte, relationship_kind, K = 20L, seed = 42L,
                          d1_earliest_onset = 1L, d2_earliest_onset = 1L) {
      stopifnot(
        is.data.table(c1_tte),
        is.character(relationship_kind),
        length(relationship_kind) == 1L,
        is.numeric(K), K >= 1L,
        is.numeric(seed)
      )

      required_cols <- c("person_id", "born_at_year",
                         "d1_failure_status", "d1_failure_time",
                         "d1_diagnosed_relatives", "d1_n_relatives",
                         "d2_failure_status", "d2_failure_time",
                         "d2_diagnosed_relatives", "d2_n_relatives")
      missing <- setdiff(required_cols, colnames(c1_tte))
      if (length(missing) > 0) {
        stop("c1_tte is missing required columns: ",
             paste(missing, collapse = ", "))
      }

      private$c1_tte            <- c1_tte
      private$relationship_kind <- relationship_kind
      private$K                 <- as.integer(K)
      private$seed              <- as.integer(seed)
      private$d1_earliest_onset <- as.integer(d1_earliest_onset)
      private$d2_earliest_onset <- as.integer(d2_earliest_onset)

      private$cif_analysis <- CumulativeIncidenceAnalysis$new()
      private$h2_analysis  <- HeritabilityAnalysis$new()
      private$gc_analysis  <- GeneticCorrelationAnalysis$new()
    },

    #' @description
    #' Run the multiple imputation analysis.
    #'
    #' Executes the full CIF/h2/GC pipeline K times with independent Bernoulli
    #' downsample draws, then combines estimates via Rubin's rules.
    #' When K = 1, returns the single resample's meta directly.
    #'
    #' @param rubin_level Controls the ordering of Rubin combination and
    #'   birth-year meta-analysis.  \code{"meta"} (default) runs Meta then
    #'   Rubin: each resample produces a scalar meta-analyzed estimate, K
    #'   scalars are combined via Rubin.  \code{"per_year"} runs Rubin then
    #'   Meta: K per-year estimates are combined via Rubin at each birth year,
    #'   then meta-analyzed.  The latter gives meta weights that reflect total
    #'   (sampling + imputation) variance.
    #' @return A named list with elements \code{h2_d1}, \code{h2_d2}, and
    #'   \code{gc}.  Each element is a list with \code{rubin_meta} (1-row
    #'   \code{data.table}) and \code{resample_meta} (K-row \code{data.table}).
    run = function(rubin_level = "meta") {
      stopifnot(rubin_level %in% c("meta", "per_year"))

      # Pre-compute CIF on the full c1 cohort (invariant across resamples)
      private$re_d1_c1 <- private$run_cif_stratified("d1", private$c1_tte,
                                                      private$d1_earliest_onset)
      private$re_d2_c1 <- private$run_cif_stratified("d2", private$c1_tte,
                                                      private$d2_earliest_onset)

      if (is.null(private$re_d1_c1) || is.null(private$re_d2_c1)) {
        stop("CIF on base cohort (c1) returned NULL — no events in data")
      }

      results <- vector("list", private$K)

      for (k in seq_len(private$K)) {
        results[[k]] <- private$run_single_resample(k)
      }

      # Drop failed resamples
      results <- Filter(Negate(is.null), results)
      K_eff <- length(results)

      if (K_eff == 0L) {
        stop("All ", private$K, " resamples failed (empty c2/c3 cohorts)")
      }

      if (K_eff < private$K) {
        message("Warning: ", private$K - K_eff, " of ", private$K,
                " resamples failed and were dropped (K_eff = ", K_eff, ")")
      }

      # Per-resample meta (always collected for diagnostics)
      h2_d1_resamples <- rbindlist(lapply(results, `[[`, "h2_d1_meta"))
      h2_d2_resamples <- rbindlist(lapply(results, `[[`, "h2_d2_meta"))
      gc_resamples    <- rbindlist(lapply(results, `[[`, "gc_meta"))

      if (K_eff == 1L) {
        # Single resample: return meta as-is with NA diagnostics
        add_na_diagnostics <- function(meta) {
          meta[, `:=`(within_var = NA_real_, between_var = NA_real_,
                      total_var = NA_real_, b_over_t = NA_real_,
                      k_resamples = 1L)]
          meta
        }
        h2_d1_rubin <- add_na_diagnostics(copy(h2_d1_resamples))
        h2_d2_rubin <- add_na_diagnostics(copy(h2_d2_resamples))
        gc_rubin    <- add_na_diagnostics(copy(gc_resamples))
      } else if (rubin_level == "meta") {
        # Meta → Rubin: combine K scalar meta estimates
        h2_d1_rubin <- self$combine_rubin(h2_d1_resamples)
        h2_d2_rubin <- self$combine_rubin(h2_d2_resamples)
        gc_rubin    <- self$combine_rubin(gc_resamples)
      } else {
        # Rubin → Meta: combine per-year, then meta-analyze
        h2_d1_tmax_list <- lapply(results, `[[`, "h2_d1_tmax")
        h2_d2_tmax_list <- lapply(results, `[[`, "h2_d2_tmax")
        gc_tmax_list    <- lapply(results, `[[`, "gc_tmax")

        h2_d1_rubin <- private$rubin_then_meta(h2_d1_tmax_list, "h2", "se", "h2",
                                               private$h2_analysis)
        h2_d2_rubin <- private$rubin_then_meta(h2_d2_tmax_list, "h2", "se", "h2",
                                               private$h2_analysis)
        gc_rubin    <- private$rubin_then_meta(gc_tmax_list, "rhh", "se", "rhh",
                                               private$gc_analysis)
      }

      list(
        h2_d1 = list(rubin_meta = h2_d1_rubin, resample_meta = h2_d1_resamples),
        h2_d2 = list(rubin_meta = h2_d2_rubin, resample_meta = h2_d2_resamples),
        gc    = list(rubin_meta = gc_rubin,    resample_meta = gc_resamples)
      )
    }
  )
)
