#' @title Class that takes care of running all parts of the complete pipeline.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @export
Pipeline <- R6::R6Class( #nolint
  "Pipeline",
  private = list(
    pool         = NULL,
    sub_analyses = NULL
  ),
  public = list(
    #' @description
    #' Creates an pipeline instance that stores the given time-to-event data.
    initialize = function(...) {
      validator <- ArgumentsValidator$new(
        pool = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            person_id = list(
              type     = "string",
              required = TRUE
            ),
            disorder = list(
              type     = "string",
              required = TRUE
            ),
            failure_status = list(
              type     = "integer",
              enum     = list(0, 1, 2),
              required = TRUE
            ),
            failure_time = list(
              type     = "numeric",
              minimum  = 0,
              required = TRUE
            ),
            relationship_kind = list(
              type     = "string",
              enum     = names(epimight:::relationship_kinds),
              required = TRUE
            ),
            relatives = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            ),
            relatives_diagnosed = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            )
          )
        )
      )

      args <- validator$run(...)
      private$pool <- args$pool
      private$sub_analyses <- list(
        core = Analysis$new(),
        h2   = HeritabilityAnalysis$new(),
        cif  = CumulativeIncidenceAnalysis$new(),
        gc   = GeneticCorrelationAnalysis$new()
      )
    },
    #' @description
    #' Downsample relative counts to independent Bernoulli indicators.
    #'
    #' For each individual, draws a 0/1 indicator with probability
    #' p = relatives_diagnosed / relatives.  This avoids cohort dilution
    #' at high prevalence, where nearly everyone has at least one affected
    #' relative and the genetic enrichment of c2/c3 vanishes.
    downsample_relatives_diagnosed = function(relatives_diagnosed, relatives) {
      p <- ifelse(relatives > 0, pmin(relatives_diagnosed / relatives, 1.0), 0.0)

      return(as.integer(rbinom(length(p), size = 1L, prob = p)))
    },
    #' @description
    #' Retrieves time-to-event data to use in a run based on the given disorders, relationship kind and group columns.
    #' Makes sure that the retrieved data fulfills the requirements of carrying out a single pipeline run.
    get_tte = function(relkind, disorder1_id, disorder2_id, stratify_columns = NULL) {
      tte <- private$pool |>
        filter(relationship_kind == relkind) |>
        select(-relationship_kind)

      tte_d1 <- tte |>
        filter(disorder == disorder1_id) |>
        select(-disorder) |>
        rename(
          d1_failure_status      = failure_status,
          d1_failure_time        = failure_time,
          d1_relatives_diagnosed = relatives_diagnosed
        )

      tte_d2 <- tte |>
        filter(disorder == disorder2_id) |>
        select(-disorder) |>
        rename(
          d2_failure_status      = failure_status,
          d2_failure_time        = failure_time,
          d2_relatives_diagnosed = relatives_diagnosed
        ) |>
        select(person_id, d2_failure_status, d2_failure_time, d2_relatives_diagnosed)

      d1_nrow <- tte_d1 |> nrow()
      d2_nrow <- tte_d2 |> nrow()

      if (d1_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder1_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d2_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder2_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d1_nrow != d2_nrow) {
        stop("Sample imbalance found, d1 had ", d1_nrow, " individuals, d2 had ", d2_nrow, " individuals")
      }

      combined <- inner_join(tte_d1, tte_d2, by = join_by(person_id))

      if (!is.list(stratify_columns)) return(combined)

      for (col in stratify_columns) {
        if (!(col %in% colnames(combined))) {
          stop("group_column \"", col, "\" was not found in TTE dataset: ", paste(colnames(combined), collapse = ", "))
        }
      }

      return(combined)
    },
    #' @description
    #' Helper that runs cif on the given time-to-event data and handles all prefixes of columns.
    run_cif = function(tte, disorder, cohort, stratify_columns, earliest_onset, latest_onset) {
      status_col <- paste0(disorder, "_failure_status")
      time_col   <- paste0(disorder, "_failure_time")

      tmp_tte <- tte |>
        rename(
          failure_status = !!as.name(status_col),
          failure_time   = !!as.name(time_col)
        ) |>
        as.data.table()

      private$sub_analyses$cif$run(
        tte              = tmp_tte,
        stratify_columns = stratify_columns,
        earliest_onset   = earliest_onset,
        latest_onset     = latest_onset
      ) |>
        select(!!!stratify_columns, time, cif, se, l95, u95, var, cases) |>
        rename_with(~ paste0(cohort, "_", .), .cols = c(cif)) |>
        rename_with(~ paste0(cohort, "_cif_", .), .cols = c(se, l95, u95, var, cases))
    },
    #' @description
    #' Helper that runs heritability on the given time-to-event data and handles all prefixes of columns.
    run_h2 = function(disorder, cif_c1, cif_c2, relationship_kind, stratify_columns) {
      cif <-  cif_c1 |>
        inner_join(cif_c2, by = join_by(time, !!!stratify_columns))

      curr_prefix <- paste0(disorder, "_h2")

      private$sub_analyses$h2$run(
        relationship_kind = relationship_kind,
        estimates         = cif
      ) |>
        rename_with(~ curr_prefix, .cols = c(h2)) |>
        rename_with(~ paste0(curr_prefix, "_", .), .cols = c(se, l95, u95)) |>
        select(time, !!!stratify_columns, starts_with(curr_prefix))
    },
    #' @description
    #' Runs a single draw which produces stratified genetic correlation for the 2 disorders specified in the
    #' pipeline run function.
    run_draw = function(tte_c1, cif_d1_c1, cif_d2_c1, args) {
      result  <- AnalysisResult$new()
      tmp_tte <- copy(tte_c1)

      tmp_tte$d1_relatives_diagnosed <- self$downsample_relatives_diagnosed(
        tmp_tte$d1_relatives_diagnosed,
        tmp_tte$d1_relatives
      )

      tmp_tte$d2_relatives_diagnosed <- self$downsample_relatives_diagnosed(
        tmp_tte$d2_relatives_diagnosed,
        tmp_tte$d2_relatives
      )

      tte_c2 <- tmp_tte[d1_relatives_diagnosed > 0]
      if (nrow(tte_c2) == 0) return(result$fail("tte_c2", "tte", "empty"))

      tte_c3 <- tmp_tte[d2_relatives_diagnosed > 0]
      if (nrow(tte_c3) == 0) return(result$fail("tte_c3", "tte", "empty"))

      cif_d1_c2 <- self$run_cif(
        tte_c2, "d1", "c2",
        args$stratify_columns,
        args$disorder1$earliest_onset,
        args$disorder1$latest_onset
      )
      if (is.null(cif_d1_c2)) return(result$fail("cif_d1_c2", "cif", "empty"))

      cif_d1_c3 <- self$run_cif(
        tte_c3, "d1", "c3",
        args$stratify_columns,
        args$disorder1$earliest_onset,
        args$disorder1$latest_onset
      )
      if (is.null(cif_d1_c3)) return(result$fail("cif_d1_c3", "cif", "empty"))

      cif_d2_c3 <- self$run_cif(
        tte_c3,
        "d2",
        "c3",
        args$stratify_columns,
        args$disorder2$earliest_onset,
        args$disorder2$latest_onset
      )
      if (is.null(cif_d2_c3)) return(result$fail("cif_d2_c3", "cif", "empty"))

      h2_d1 <- self$run_h2("d1", cif_d1_c1, cif_d1_c2, args$relationship_kind, args$stratify_columns)
      if (is.null(h2_d1)) return(result$fail("h2_d1", "h2", "empty"))

      h2_d2 <- self$run_h2(
        "d2",
        cif_d2_c1,
        cif_d2_c3 |> rename(c2_cif = c3_cif, c2_cif_cases = c3_cif_cases),
        args$relationship_kind,
        args$stratify_columns
      )
      if (is.null(h2_d2)) return(result$fail("h2_d2", "h2", "empty"))

      cif_d1_c1 <- cif_d1_c1 |> rename_with(~ paste0("d1_", .), .cols = starts_with("c1_"))
      cif_d1_c3 <- cif_d1_c3 |> rename_with(~ paste0("d1_", .), .cols = starts_with("c3_"))
      cif_d2_c1 <- cif_d2_c1 |> rename_with(~ paste0("d2_", .), .cols = starts_with("c1_"))

      join_columns <- list("time")

      if ("stratify_columns" %in% names(args) && is.list(args$stratify_columns)) {
        join_columns <- c(join_columns, args$stratify_columns)
      }

      join_symbols <- rlang::syms(join_columns)

      combined <- cif_d1_c1 |>
        inner_join(cif_d1_c3, by = join_by(!!!join_columns)) |>
        inner_join(cif_d2_c1, by = join_by(!!!join_columns)) |>
        inner_join(h2_d1, by = join_by(!!!join_columns)) |>
        inner_join(h2_d2, by = join_by(!!!join_columns)) |>
        select(all_of(unlist(join_columns)), everything()) |>
        group_by(!!!join_symbols) |>
        arrange(desc(time)) |>
        filter(row_number() == 1) |>
        as.data.table()

      if (nrow(combined) == 0) return(result$fail("combined", "tte", "empty"))

      rg <- private$sub_analyses$gc$run(
        relationship_kind = args$relationship_kind,
        estimates         = combined
      ) |>
        select(!!!args$stratify_columns, rg, se, rg_l95, rg_u95) |>
        rename(rg_se = se)

      if (nrow(rg) == 0) return(result$fail("rg", "rg", "empty"))

      remove_cif_prefix <- function(dt, disorder, cohort) {
        prefix <- paste0(disorder, "_", cohort, "_")

        dt |>
          mutate(disorder = disorder, cohort = cohort) |>
          rename_with(~
            str_remove(., paste0("^", prefix)),
            .cols = starts_with(prefix)
          ) |>
          select(disorder, cohort, !!!args$stratify_column, everything())
      }

      remove_h2_prefix <- function(dt, disorder) {
        prefix <- paste0(disorder, "_")

        dt |>
          mutate(disorder = disorder) |>
          rename_with(~
            str_remove(., paste0("^", prefix)),
            .cols = starts_with(prefix)
          ) |>
          select(disorder, !!!args$stratify_column, everything())
      }

      result$success(list(
        cif = rbindlist(list(
          remove_cif_prefix(cif_d1_c1, "d1", "c1"),
          remove_cif_prefix(cif_d1_c3, "d1", "c3"),
          remove_cif_prefix(cif_d2_c1, "d2", "c1")
        )),
        h2 = rbindlist(list(
          remove_h2_prefix(h2_d1, "d1"),
          remove_h2_prefix(h2_d2, "d2")
        )),
        rg = rg
      ))
    },
    #' @description
    #' Runs a single experiment using the given disorders and relationship_kind.
    run = function(...) {
      validator <- ArgumentsValidator$new(
        disorder1 = list(
          required   = TRUE,
          type       = "named_list",
          properties = list(
            id = list(
              required = TRUE,
              type     = "string"
            ),
            earliest_onset = list(
              type    = "integer",
              minimum = 1,
              default = 1
            ),
            latest_onset = list(
              type    = "integer",
              minimum = 1
            )
          )
        ),
        disorder2 = list(
          required   = TRUE,
          type       = "named_list",
          properties = list(
            id = list(
              required = TRUE,
              type     = "string"
            ),
            earliest_onset = list(
              type    = "integer",
              minimum = 1,
              default = 1
            ),
            latest_onset = list(
              type    = "integer",
              minimum = 1
            )
          )
        ),
        relationship_kind = list(
          type     = "string",
          enum     = names(epimight:::relationship_kinds),
          required = TRUE
        ),
        stratify_columns = list(
          type    = "list",
          items   = list(type = "string"),
          default = list()
        ),
        draws = list(
          type    = "integer",
          minimum = 1,
          default = 1
        )
      )

      args   <- validator$run(...)
      tte_c1 <- self$get_tte(args$relationship_kind, args$disorder1$id, args$disorder2$id, args$stratify_columns)

      cif_d1_c1 <- self$run_cif(
        tte_c1, "d1", "c1",
        args$stratify_columns,
        args$disorder1$earliest_onset,
        args$disorder1$latest_onset
      )
      if (is.null(cif_d1_c1)) stop("Disorder 1, cohort 1 had no TTE events")

      cif_d2_c1 <- self$run_cif(
        tte_c1, "d2", "c1",
        args$stratify_columns,
        args$disorder2$earliest_onset,
        args$disorder2$latest_onset
      )
      if (is.null(cif_d2_c1)) stop("Disorder 2, cohort 1 had no TTE events")

      errors      <- list()
      cif_results <- NULL
      h2_results  <- NULL
      rg_results  <- NULL

      for (k in seq_len(args$draws)) {
        draw <- self$run_draw(tte_c1, cif_d1_c1, cif_d2_c1, args)

        if (!is.null(draw$error)) {
          errors <- append(errors, draw)
          next
        }

        cif_results <- rbind(cif_results, draw$results$cif |> mutate(draw = k) |> select(draw, everything()))
        h2_results  <- rbind(h2_results,  draw$results$h2  |> mutate(draw = k) |> select(draw, everything()))
        rg_results  <- rbind(rg_results,  draw$results$rg  |> mutate(draw = k) |> select(draw, everything()))
      }

      if (is.null(rg_results) || nrow(rg_results) == 0) stop("All draws failed")

      cif_stratify_columns <- c(list("disorder", "cohort"), args$stratify_columns, list("time"))
      cif_combined <- private$sub_analyses$core$run_rubins_combine(
        estimates        = cif_results,
        estimate_column  = "cif",
        se_column        = "cif_se",
        stratify_columns = cif_stratify_columns
      ) |>
        select(!!!cif_stratify_columns, fixed_meta, fixed_se, fixed_l95, fixed_u95) |>
        rename(cif = fixed_meta, se = fixed_se, l95 = fixed_l95, u95 = fixed_u95)

      h2_stratify_columns <- c(list("disorder"), args$stratify_columns)
      h2_combined <- private$sub_analyses$core$run_rubins_combine(
        estimates        = h2_results,
        estimate_column  = "h2",
        se_column        = "h2_se",
        stratify_columns = h2_stratify_columns
      ) |>
        select(!!!h2_stratify_columns, fixed_meta, fixed_se, fixed_l95, fixed_u95) |>
        rename(h2 = fixed_meta, se = fixed_se, l95 = fixed_l95, u95 = fixed_u95)

      rg_combined <- private$sub_analyses$core$run_rubins_combine(
        estimates        = rg_results,
        estimate_column  = "rg",
        se_column        = "rg_se",
        stratify_columns = args$stratify_columns
      ) |>
        select(!!!args$stratify_columns, fixed_meta, fixed_se, fixed_l95, fixed_u95) |>
        rename(rg = fixed_meta, se = fixed_se, l95 = fixed_l95, u95 = fixed_u95)

      cif_meta <- private$sub_analyses$core$run_meta(
        estimates        = cif_combined,
        estimate_column  = "cif",
        se_column        = "se",
        stratify_columns = list("disorder", "cohort")
      ) |>
        select(disorder, cohort, everything())

      h2_meta <- private$sub_analyses$core$run_meta(
        estimates        = h2_combined,
        estimate_column  = "h2",
        se_column        = "se",
        stratify_columns = list("disorder")
      ) |>
        select(disorder, everything())

      rg_meta <- private$sub_analyses$core$run_meta(
        estimates        = rg_combined,
        estimate_column  = "rg",
        se_column        = "se"
      )

      return(list(
        stratified = list(
          cif = cif_combined,
          h2  = h2_combined,
          rg  = rg_combined
        ),
        meta = list(
          cif = cif_meta,
          h2  = h2_meta,
          rg  = rg_meta
        )
      ))
    }
  )
)
