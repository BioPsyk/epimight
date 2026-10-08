#' @title Pipeline that produces cumulative incidences, heritabilities and genetic correlations from time-to-event data.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @import rjson
#' @import waldo
#' @export
Pipeline <- R6::R6Class( #nolint
  "Pipeline",
  private = list(
    pool          = NULL,
    analyses      = NULL,
    sandwich      = NULL,
    sandwich_runs = list(),
    results       = list(
      cif       = list(),
      h2        = list(),
      h2_pooled = list(),
      rg        = list()
    ),
    cache_key = function(type, args) {
      rules     <- self$validation_rules[[type]]
      validator <- do.call(ArgumentsValidator$new, rules$properties)
      args      <- do.call(validator$run, args)
      args      <- args[order(names(args))]

      rjson::toJSON(args)
    },
    add_cif_prefix = function(cif, prefix, stratify_columns) {
      cif |>
        select(!!!stratify_columns, age, cif, cases) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(cif, cases))
    },
    add_h2_prefix = function(h2, prefix, stratify_columns) {
      h2 |>
        select(!!!stratify_columns, age, h2) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(h2))
    },
    rg_estimates = function(args) {
      stratify_columns <- args$cif_cross$stratify_columns
      join_columns     <- c(list("age"), stratify_columns)
      join_symbols     <- rlang::syms(join_columns)

      cif_t1_pop <- do.call(self$run_cif, args$h2_t1$cif_pop)
      cif_t2_pop <- do.call(self$run_cif, args$h2_t2$cif_pop)
      h2_t1      <- do.call(private$native_h2, args$h2_t1)
      h2_t2      <- do.call(private$native_h2, args$h2_t2)
      cif_cross  <- do.call(self$run_cif, args$cif_cross)

      h2_t1_meta <- "meta_analyze" %in% names(args$h2_t1)
      h2_t2_meta <- "meta_analyze" %in% names(args$h2_t2)

      if (h2_t1_meta || h2_t2_meta) {
        stratify_combinations <- cif_cross$results |>
          group_by(!!!join_symbols) |>
          select(!!!join_columns) |>
          ungroup()

        if (h2_t1_meta) {
          h2_t1$results <- right_join(
            h2_t1$results,
            stratify_combinations,
            by = join_by(age)
          ) |>
            filter(!is.na(index_trait)) |>
            select(index_trait, !!!join_symbols, h2, se, l95, u95)
        }

        if (h2_t2_meta) {
          h2_t2$results <- right_join(
            h2_t2$results,
            stratify_combinations,
            by = join_by(age)
          ) |>
            filter(!is.na(index_trait)) |>
            select(index_trait, !!!join_symbols, h2, se, l95, u95)
        }
      }

      combined <- private$add_cif_prefix(cif_t1_pop$results, "t1_pop", stratify_columns) |>
        inner_join(
          private$add_cif_prefix(cif_cross$results, "cross", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          private$add_cif_prefix(cif_t2_pop$results, "t2_pop", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          private$add_h2_prefix(h2_t1$results, "t1", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          private$add_h2_prefix(h2_t2$results, "t2", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        self$max_age_by_stratification(stratify_columns)

      if (nrow(combined) == 0) stop("After joining all cif and h2 results no data was left")

      private$analyses$rg$run(
        estimates   = combined,
        relatedness = args$relatedness
      )
    },
    # Sandwich SEs. A plan names the sandwich output behind each row of a results table (NA for
    # rows without one), holds the term table of those outputs (see `SandwichAnalysis$h2_terms`)
    # and lists the CIF arguments of the cohorts the terms read. Outputs are keyed by
    # `output_key` and strata by `stratum_key`.
    stratum_key = function(table, stratify_columns) {
      columns <- unlist(stratify_columns)

      if (length(columns) == 0) return(rep(".", nrow(table)))

      do.call(paste, c(unname(as.list(as.data.frame(table)[columns])), sep = "\r"))
    },
    output_key = function(...) {
      paste(..., sep = "|")
    },
    # The CIFs behind h2 values. `points` has `output`, `stratum` and `age`; the result adds the
    # cohort keys `pop` and `fh` and their CIFs `k_pop` and `k_fh` at that stratum and age.
    h2_units = function(args, points) {
      cif_at <- function(cif_args) {
        cif <- do.call(self$run_cif, cif_args)$results

        data.table(
          stratum = private$stratum_key(cif, cif_args$stratify_columns),
          age     = cif$age,
          cif     = cif$cif
        )
      }

      units <- points |>
        select(output, stratum, age) |>
        left_join(cif_at(args$cif_pop), by = join_by(stratum, age)) |>
        rename(k_pop = cif) |>
        left_join(cif_at(args$cif_fh), by = join_by(stratum, age)) |>
        rename(k_fh = cif) |>
        mutate(
          pop = private$cache_key("cif", args$cif_pop),
          fh  = private$cache_key("cif", args$cif_fh)
        )

      if (anyNA(units$k_pop) || anyNA(units$k_fh)) {
        stop("Sandwich SE: no CIF row for an h2 value's stratum and age; the CIF and h2 tables disagree")
      }

      units
    },
    # Influence terms of the h2 values read at `points` (`stratum`, `age`): each stratum's own h2
    # at that age, or with `meta_analyze` the pooled h2 at that age. Returns the output behind
    # each row of `points` and the term table.
    h2_terms_at = function(args, points, prefix) {
      if (!("meta_analyze" %in% names(args))) {
        points <- points |>
          transmute(output = private$output_key(prefix, stratum, age), stratum, age)
        units  <- private$h2_units(args, points)

        return(list(output = points$output, terms = private$sandwich$h2_terms(units, args$relatedness)))
      }

      sub_args              <- args
      sub_args$meta_analyze <- NULL

      # The strata pooled at each age and their run_meta shares, frozen at the full sample.
      local  <- do.call(private$native_h2, sub_args)$results
      pooled <- local |>
        mutate(
          stratum = private$stratum_key(local, sub_args$cif_pop$stratify_columns),
          share   = private$sandwich$calculate_meta_shares(h2, se, age, args$meta_analyze)
        ) |>
        filter(age %in% points$age, !is.na(share)) |>
        transmute(output = private$output_key(prefix, stratum, age), stratum, age, share)

      units <- private$h2_units(sub_args, pooled)
      terms <- private$sandwich$compose_terms(
        private$sandwich$h2_terms(units, sub_args$relatedness),
        pooled |> transmute(from = output, to = private$output_key(prefix, "meta", age), coef = share)
      )

      list(output = private$output_key(prefix, "meta", points$age), terms = terms)
    },
    # The plan of an h2 results table: each stratum's last age gets an output (with
    # `meta_analyze`, the meta table's last age); other rows get NA.
    h2_plan = function(args, h2) {
      stratum <- if ("meta_analyze" %in% names(args)) {
        rep(".", nrow(h2))
      } else {
        private$stratum_key(h2, args$cif_pop$stratify_columns)
      }
      headline <- h2$age == stats::ave(h2$age, stratum, FUN = max)
      points   <- data.table(stratum = stratum, age = h2$age) |> filter(headline)
      inputs   <- private$h2_terms_at(args, points, "h2")
      output   <- rep(NA_character_, nrow(h2))

      output[headline] <- inputs$output

      list(output = output, terms = inputs$terms, cifs = list(args$cif_pop, args$cif_fh))
    },
    # The plan of an rg analysis: one output per stratum, pooled into "rg|meta" with run_meta's
    # shares when `meta_analyze` is set.
    rg_plan = function(args) {
      sub_args              <- args
      sub_args$meta_analyze <- NULL

      estimates <- private$rg_estimates(sub_args)
      strata    <- private$stratum_key(estimates, args$cif_cross$stratify_columns)
      units     <- estimates |>
        transmute(
          output  = private$output_key("rg", strata),
          stratum = strata,
          age,
          pop1    = private$cache_key("cif", args$h2_t1$cif_pop),
          cross   = private$cache_key("cif", args$cif_cross),
          pop2    = private$cache_key("cif", args$h2_t2$cif_pop),
          k_pop1  = t1_pop_cif,
          k_cross = cross_cif,
          k_pop2  = t2_pop_cif,
          h2_t1   = t1_h2,
          h2_t2   = t2_h2
        )
      t1    <- private$h2_terms_at(args$h2_t1, units, "t1")
      t2    <- private$h2_terms_at(args$h2_t2, units, "t2")
      units <- units |> mutate(h2_t1_output = t1$output, h2_t2_output = t2$output)
      terms <- private$sandwich$rg_terms(units, bind_rows(t1$terms, t2$terms), args$relatedness)
      cifs  <- list(args$h2_t1$cif_pop, args$h2_t1$cif_fh, args$h2_t2$cif_pop, args$h2_t2$cif_fh, args$cif_cross)

      if (!("meta_analyze" %in% names(args))) return(list(output = units$output, terms = terms, cifs = cifs))

      shares <- data.table(
        from = units$output,
        to   = private$output_key("rg", "meta"),
        coef = private$sandwich$calculate_meta_shares(
          estimates$rg, estimates$rg_se, rep(1, nrow(estimates)), args$meta_analyze
        )
      ) |>
        filter(!is.na(coef))

      list(output = shares$to[1], terms = private$sandwich$compose_terms(terms, shares), cifs = cifs)
    },
    # The rows a last-age pool reads: each stratum's h2 row at its own last age, as h2_plan picks
    # it, kept when h2 and SE are finite and SE is positive (run_meta would take SE 0 as an
    # infinite weight). Adds the `stratum` key.
    last_age_rows = function(args) {
      stratify_columns <- args$cif_pop$stratify_columns

      if (length(stratify_columns) == 0) stop("Pooling h2 across strata needs `stratify_columns`")

      local <- do.call(private$native_h2, args[c("cif_pop", "cif_fh", "relatedness")])$results
      last  <- local |>
        mutate(stratum = private$stratum_key(local, stratify_columns)) |>
        filter(age == stats::ave(age, stratum, FUN = max)) |>
        filter(is.finite(h2), is.finite(se), se > 0)

      if (nrow(last) == 0) stop("No stratum has a finite h2 with a positive SE at its last age")
      if (args$method == "random" && nrow(last) < 2) {
        stop("A random-effects pool needs at least 2 eligible strata, found ", nrow(last))
      }

      last
    },
    # The plan of a last-age pool: each stratum's h2 at its last age, pooled into "h2|pool" with
    # run_meta's shares.
    h2_pooled_plan = function(args) {
      last   <- private$last_age_rows(args)
      local  <- private$h2_terms_at(args, last, "h2")
      shares <- data.table(
        from = local$output,
        to   = private$output_key("h2", "pool"),
        coef = private$sandwich$calculate_meta_shares(last$h2, last$se, rep(1, nrow(last)), args$method)
      )

      list(output = shares$to[1], terms = private$sandwich$compose_terms(local$terms, shares),
           cifs = list(args$cif_pop, args$cif_fh))
    },
    check_h2_args = function(args, rules) {
      if ("relatives_trait" %in% names(args$cif_pop)) {
        stop("Using `relatives_trait` in `cif_pop` is not allowed")
      } else if ("relatives_kind" %in% names(args$cif_pop)) {
        stop("Using `relatives_kind` in `cif_pop` is not allowed")
      } else if (args$cif_pop$index_trait != args$cif_fh$index_trait) {
        stop("Using different `index_trait` in `cif_pop` and `cif_fh` is not allowed")
      } else if (args$cif_fh$index_trait != args$cif_fh$relatives_trait) {
        stop("Using different `index_trait` and `relatives_trait` in `cif_fh` is not allowed")
      } else if (!identical(args$cif_pop$stratify_columns, args$cif_fh$stratify_columns)) {
        stop("Using different `stratify_columns` in `cif_pop` and `cif_fh` is not allowed")
      } else if ("meta_analyze" %in% names(args$cif_pop)) {
        stop("Using meta-analyzed `cif_pop` as input to h2 is not allowed")
      } else if ("meta_analyze" %in% names(args$cif_fh)) {
        stop("Using meta-analyzed `cif_fh` as input to h2 is not allowed")
      }

      args
    },
    native_h2 = function(...) {

      validator <- do.call(ArgumentsValidator$new, self$validation_rules$h2$properties)
      validator$add_post_validation(private$check_h2_args)

      args      <- validator$run(...)
      metadata  <- list(
        epimight_version   = as.character(packageVersion(methods::getPackageName())),
        analysis_name      = "h2",
        analysis_arguments = args,
        analysis_time      = format(Sys.time(), "%Y-%m-%dT%H:%M")
      )
      cached_h2 <- self$get_results("h2", args)

      if (!is.null(cached_h2)) {
        return(list(metadata = metadata, results = cached_h2))
      }

      if ("meta_analyze" %in% names(args)) {
        sub_args              <- copy(args)
        sub_args$meta_analyze <- NULL

        h2      <- do.call(private$native_h2, sub_args)
        h2_meta <- private$analyses$core$run_meta(
          estimates       = h2$results,
          estimate_column = "h2",
          se_column       = "se",
          group_columns   = list("index_trait", "age")
        ) |>
          rename_with(~ str_remove(., sprintf("^%s_", args$meta_analyze))) |>
          select(index_trait, age, meta, se, l95, u95) |>
          rename(h2 = meta)

        self$add_results("h2", h2_meta, args)

        return(list(metadata = metadata, results = h2_meta))
      }

      stratify_columns <- args$cif_pop$stratify_columns
      stratify_symbols <- rlang::syms(stratify_columns)

      cif_pop <- do.call(self$run_cif, args$cif_pop)
      cif_fh  <- do.call(self$run_cif, args$cif_fh)

      if ("meta_analyze" %in% names(args$cif_pop)) {
        cif <- cif_pop$results |>
          inner_join(cif_fh$results, by = join_by(age))
      } else {
        cif <- cif_pop$results |>
          inner_join(cif_fh$results, by = join_by(age, !!!stratify_columns))
      }

      cif <- cif |>
        rename(
          pop_cif   = cif.x,
          pop_cases = cases.x,
          fh_cif    = cif.y,
          fh_cases  = cases.y
        ) |>
        select(age, !!!stratify_symbols, pop_cif, pop_cases, fh_cif, fh_cases)

      h2 <- private$analyses$h2$run(
        cif         = cif,
        relatedness = args$relatedness
      ) |>
        mutate(index_trait = args$cif_pop$index_trait) |>
        select(index_trait, age, !!!stratify_symbols, h2, se, l95, u95)

      if (is.null(h2)) stop(paste0("No valid results found when producing h2 for trait ", args$cif_pop$index_trait))

      self$add_results("h2", h2, args)

      list(metadata = metadata, results = h2)
    },
    native_rg = function(...) {

      validator <- do.call(ArgumentsValidator$new, self$validation_rules$rg$properties)

      validator$add_post_validation(function(args, rules) {
        if (!identical(args$h2_t1$cif_pop$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `h2_t1` and `h2_t2` is not allowed")
        } else if (!identical(args$cif_cross$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `cif_cross`, `h2_t1` and `h2_t2` is not allowed")
        } else if ("meta_analyze" %in% names(args$cif_cross)) {
          stop("Using meta-analyzed `cif_cross` as input to rg is not supported")
        }

        args
      })

      args     <- validator$run(...)
      metadata <- list(
        epimight_version   = as.character(packageVersion(methods::getPackageName())),
        analysis_name      = "rg",
        analysis_arguments = args,
        analysis_time      = format(Sys.time(), "%Y-%m-%dT%H:%M")
      )

      cached_rg <- self$get_results("rg", args)

      if (!is.null(cached_rg)) {
        return(list(metadata = metadata, results = cached_rg))
      }

      if ("meta_analyze" %in% names(args)) {
        sub_args              <- copy(args)
        sub_args$meta_analyze <- NULL

        rg      <- do.call(private$native_rg, sub_args)
        rg_meta <- private$analyses$core$run_meta(
          estimates       = rg$results,
          estimate_column = "rg",
          se_column       = "se"
        ) |>
          rename_with(~ str_remove(., sprintf("^%s_", args$meta_analyze))) |>
          select(meta, se, l95, u95) |>
          rename(rg = meta)

        self$add_results("rg", rg_meta, args)

        return(list(metadata = metadata, results = rg_meta))
      }

      stratify_columns <- args$cif_cross$stratify_columns
      estimates        <- private$rg_estimates(args)
      rg               <- estimates |> select(!!!stratify_columns, rg, se = rg_se, l95 = rg_l95, u95 = rg_u95)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      self$add_results("rg", rg, args)

      list(metadata = metadata, results = rg)
    },
    # Adds the sandwich columns to the results a public run_h2, run_h2_pooled or run_rg call
    # returns, cached or not. The native_* methods never come through here, so the calls nested
    # inside an analysis skip the sandwich.
    with_sandwich_columns = function(type, out) {
      args <- out$metadata$analysis_arguments
      key  <- private$cache_key(type, args)

      if (!("sandwich_se" %in% names(out$results))) {
        kind <- switch(
          type,
          h2        = list(plan = private$h2_plan(args, out$results), estimate = "h2"),
          h2_pooled = list(plan = private$h2_pooled_plan(args), estimate = "h2"),
          rg        = list(plan = private$rg_plan(args), estimate = "rg"),
          stop("No sandwich plan for results of type \"", type, "\"")
        )
        plan <- kind$plan
        fit  <- private$sandwich$run(plan$terms, private$sandwich_cohorts(plan$cifs, plan$terms))

        out$results <- private$add_sandwich_columns(out$results, plan$output, fit$variance, kind$estimate)
        private$sandwich_runs[[type]][[key]] <- fit[c("batch_size", "passes")]
        self$add_results(type, out$results, args)
      }

      out$metadata$sandwich <- private$sandwich_runs[[type]][[key]]

      out
    },
    # The TTE rows of every cohort the term table reads, keyed like its `cohort` column.
    sandwich_cohorts = function(cif_args, terms) {
      cohorts <- list()

      for (one in cif_args) {
        key <- private$cache_key("cif", one)

        if (!is.null(cohorts[[key]]) || !(key %chin% terms$cohort)) next

        tte <- do.call(self$get_tte, one)
        tte[, stratum := private$stratum_key(tte, one$stratify_columns)]
        cohorts[[key]] <- tte
      }

      cohorts
    },
    # Adds `sandwich_se`, `sandwich_l95` and `sandwich_u95` to `results`, whose rows carry the
    # outputs `output` (NA for none); `variance` has one row per output.
    add_sandwich_columns = function(results, output, variance, estimate) {
      variance <- variance$variance[match(output, variance$output)]
      negative <- !is.na(variance) & variance < 0

      if (any(negative)) {
        warning(sum(negative), " sandwich variance(s) came out negative and are reported as NA")
      }

      # Locals named so that no results column shadows them inside mutate (`se` would).
      row_variance <- ifelse(negative, NA_real_, variance)
      point        <- results[[estimate]]

      results |>
        mutate(
          sandwich_se  = sqrt(row_variance),
          sandwich_l95 = point - 1.96 * sandwich_se,
          sandwich_u95 = point + 1.96 * sandwich_se
        ) |>
        as.data.table()
    }
  ),
  public = list(
    validation_rules = list(
      meta_analyze = list(
        type    = "string",
        enum    = list("random", "fixed")
      )
    ),
    #' Creates a pipeline instance ready to be used to run analyses.
    #'
    #' @seealso [run_default_rg()] For quickly running a full genetic correlation.
    #'
    #' @param pool The pool that contains time-to-event data for all traits and kinds of relatives you want to analyze.
    #' @param pedigree Optional data.table with string columns `person_id`, `mother_id`, `father_id` and an
    #'   optional `twin` (the co-twin's id), with `NA` for an unknown parent. Every `person_id` of the pool needs
    #'   a row. With a pedigree, h2 and rg results gain pedigree-pair sandwich SEs (`sandwich_se`,
    #'   `sandwich_l95`, `sandwich_u95`) at their headline age.
    #' @param max_degree Highest kinship degree of the related pairs the sandwich SEs sum over, 1 to 5 (default 3).
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
            trait = list(
              type     = "string",
              required = TRUE
            ),
            trait_status = list(
              type     = "integer",
              enum     = list(0, 1, 2),
              required = TRUE
            ),
            trait_age = list(
              type     = "numeric",
              minimum  = 0,
              required = TRUE
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            ),
            relatives_n = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            ),
            relatives_n_trait = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            )
          )
        ),
        pedigree = list(
          required = FALSE,
          type     = "data.table",
          columns  = list(
            person_id = list(type = "string", required = TRUE),
            mother_id = list(type = "string", required = TRUE),
            father_id = list(type = "string", required = TRUE),
            twin      = list(type = "string")
          )
        ),
        max_degree = list(
          type    = "integer",
          minimum = 1,
          maximum = 5,
          default = 3L
        )
      )

      args         <- validator$run(...)
      private$pool <- args$pool

      if (!is.null(args$pedigree)) {
        private$sandwich <- SandwichAnalysis$new(
          pedigree   = args$pedigree,
          probands   = unique(private$pool$person_id),
          max_degree = as.integer(args$max_degree)
        )
      }

      self$validation_rules$cif <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          index_trait = list(
            required = TRUE,
            type     = "string"
          ),
          relatives_trait = list(
            required = FALSE,
            type     = "string"
          ),
          relatives_kind = list(
            required = FALSE,
            type     = "string"
          ),
          stratify_columns = list(
            type    = "list",
            items   = list(type = "string"),
            default = list()
          ),
          use_weighted = list(
            type    = "logical",
            default = TRUE
          ),
          meta_analyze = self$validation_rules$meta_analyze
        )
      )

      self$validation_rules$h2 <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          cif_pop = self$validation_rules$cif,
          cif_fh  = self$validation_rules$cif,
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          ),
          meta_analyze = self$validation_rules$meta_analyze
        )
      )

      self$validation_rules$h2_pooled <- self$validation_rules$h2
      self$validation_rules$h2_pooled$properties$meta_analyze <- NULL
      self$validation_rules$h2_pooled$properties$method       <- list(
        type    = "string",
        enum    = list("fixed", "random"),
        default = "fixed"
      )

      self$validation_rules$rg <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          cif_cross  = self$validation_rules$cif,
          h2_t1      = self$validation_rules$h2,
          h2_t2      = self$validation_rules$h2,
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          ),
          meta_analyze = self$validation_rules$meta_analyze
        )
      )
      self$validation_rules$rg$properties$cif_cross$relatives_trait$required <- TRUE
      self$validation_rules$rg$properties$cif_cross$relatives_kind$required  <- TRUE

      private$analyses <- list(
        core = Analysis$new(),
        h2   = HeritabilityAnalysis$new(),
        cif  = CumulativeIncidenceAnalysis$new(),
        rg   = GeneticCorrelationAnalysis$new()
      )
    },
    max_age_by_stratification = function(results, stratify_columns) {
      results |>
        group_by(!!!rlang::syms(stratify_columns)) |>
        arrange(desc(age)) |>
        filter(row_number() == 1) |>
        as.data.table()
    },
    #' Removes all results from the cache.
    clear_results = function() {
      private$results       <- list(cif = list(), h2 = list(), h2_pooled = list(), rg = list())
      private$sandwich_runs <- list()
    },
    #' Adds the given analysis results to the cache.
    #'
    #' @param type A label that identifies what analysis produced the results: "cif", "h2", "h2_pooled" or "rg".
    #' @param results A data.table with the results to cache.
    #' @param args The arguments that was provided to the analysis function that produced the results.
    add_results = function(type, results, args) {
      if (!is.character(type)) stop("Given `type` was not a character")
      if (!is.data.table(results)) stop("Given `results` was not a data.table")
      if (!is.list(args)) stop("Given `args` was not a named list")
      if (!(type %in% names(self$validation_rules))) stop("Given `type` \"", type, "\" was unknown")

      private$results[[type]][[private$cache_key(type, args)]] <- results
    },
    #' Gets the analysis results produced by the given analysis arguments from the cache.
    #'
    #' @param type A label that identifies what analysis produced the results: "cif", "h2", "h2_pooled" or "rg".
    #' @param args The arguments that was provided to the analysis function that produced the results.
    #' @returns A data.table with the cache analysis results.
    get_results = function(type, args) {
      if (!is.character(type)) stop("Given `type` was not a character")
      if (!is.list(args)) stop("Given `args` was not a named list")
      if (!(type %in% names(self$validation_rules))) stop("Given `type` \"", type, "\" was unknown")

      private$results[[type]][[private$cache_key(type, args)]]
    },
    #' Gets time-to-event data from the pool using the given analysis arguments.
    #'
    #' @param index_trait Label of the trait to retrieve time-to-event data for.
    #' @param relative_trait Label of the trait to retrieve time-to-event data for.
    #' @param relative_kind Label of the kind of relative to retrieve time-to-event data for.
    #' @param stratify_columns List of columns to check that they exist in the time-to-event data.
    #' @param use_weighted Boolean on whether to calculate the weight column used in weighted CIF calculations.
    #' @returns A data.table with the relevant time-to-event data.
    get_tte = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$cif$properties)
      args      <- validator$run(...)
      columns   <- c(c("person_id", "trait_status", "trait_age"), unlist(args$stratify_columns))

      for (col in columns) {
        if (!(col %in% colnames(private$pool))) {
          stop("Column \"", col, "\" was not found in the TTE pool: ", paste(colnames(private$pool), collapse = ", "))
        }
      }

      tte <- private$pool[
        trait == args$index_trait
      ][
        , .SD[1], by = "person_id"
      ][
        , ..columns
      ]

      if (nrow(tte) == 0) stop(paste0("No proband TTE data found for trait ", index_trait))

      if (!is.null(args$relatives_trait) && !is.null(args$relatives_kind)) {
        relatives_tte <- private$pool[
          trait == args$relatives_trait & relatives_kind == args$relatives_kind
        ][
          , .SD[1], by = "person_id"
        ][
          , c("person_id", "relatives_kind", "relatives_n", "relatives_n_trait")
        ]

        if (nrow(relatives_tte) == 0) {
          stop(paste0(
            "No family history TTE data found for trait \"", args$relatives_trait,
            "\" and relationship kind \"", args$relatives_kind, "\""
          ))
        }

        tte <- tte[
          relatives_tte,
          on = .(person_id = person_id)
        ][
          relatives_n_trait > 0
        ]

        if (isTRUE(args$use_weighted)) {
          tte <- tte[
            , weight := ifelse(relatives_n_trait > 0.0, relatives_n_trait / relatives_n, 0.0)
          ]
        }

        if (nrow(tte) == 0) {
          stop(paste0(
            "No probands with at least 1 relative (of kind \"",
            args$relatives_kind, "\") with trait \"", args$relatives_trait, "\""
          ))
        }
      }

      tte
    },
    #' Produces cumulative incidence for the given trait and relative kind.
    #'
    #' @param index_trait Label of the trait to retrieve time-to-event data for.
    #' @param relative_trait Label of the trait to retrieve time-to-event data for.
    #' @param relative_kind Label of the kind of relative to retrieve time-to-event data for.
    #' @param stratify_columns List of columns to stratify the results on.
    #' @param use_weighted Boolean on whether to calculate the weight column used in weighted CIF calculations.
    #' @returns A named list with metadata and results.
    run_cif = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$cif$properties)
      args      <- validator$run(...)
      metadata  <- list(
        epimight_version   = as.character(packageVersion(methods::getPackageName())),
        analysis_name      = "cif",
        analysis_arguments = args,
        analysis_time      = format(Sys.time(), "%Y-%m-%dT%H:%M")
      )
      cached_cif <- self$get_results("cif", args)

      if (!is.null(cached_cif)) {
        return(list(metadata = metadata, results = cached_cif))
      }

      if ("meta_analyze" %in% names(args)) {
        if (length(args$stratify_columns) == 0) stop("Can't use meta_analyze without stratify_columns")

        sub_args              <- copy(args)
        sub_args$meta_analyze <- NULL

        group_columns <- list("index_trait", "relatives_trait", "relatives_kind", "age")

        cif      <- do.call(self$run_cif, sub_args)
        cif_meta <- private$analyses$core$run_meta(
          estimates       = cif$results,
          estimate_column = "cif",
          se_column       = "se",
          group_columns   = group_columns
        ) |>
          rename_with(~ str_remove(., sprintf("^%s_", args$meta_analyze))) |>
          select(!!!group_columns, meta, se, l95, u95) |>
          rename(cif = meta)

        self$add_results("cif", cif_meta, args)

        return(list(metadata = metadata, results = cif_meta))
      }

      tte <- do.call(self$get_tte, args)
      cif <- private$analyses$cif$run(
        tte              = tte,
        stratify_columns = args$stratify_columns
      ) |>
        mutate(
          index_trait     = args$index_trait,
          relatives_trait = ifelse("relatives_trait" %in% names(args), args$relatives_trait, NA),
          relatives_kind  = ifelse("relatives_kind" %in% names(args),  args$relatives_kind,  NA)
        ) |>
        select(
          index_trait,
          relatives_trait,
          relatives_kind,
          all_of(unlist(args$stratify_columns)),
          age,
          everything()
        )

      if (is.null(cif)) {
        stop(paste0(
          "No TTE events found when producing cif_", index_trait, "_", rel_trait, "_", rel_kind
        ))
      }

      self$add_results("cif", cif, args)

      list(metadata = metadata, results = cif)
    },
    #' Produces heritability for the given trait and relative kind.
    #'
    #' @param cif_pop Analysis arguments for population cumulative incidence. See run_cif for details.
    #' @param cif_fh Analysis arguments for family history cumulative incidence. See run_cif for details.
    #' @param relatedness Relatedness coefficient to use in h2 calculation.
    #' @returns A named list with metadata and results.
    run_h2 = function(...) {
      out <- private$native_h2(...)

      if (is.null(private$sandwich)) return(out)

      private$with_sandwich_columns("h2", out)
    },
    #' Produces heritability pooled across strata, each stratum read at its own last age.
    #'
    #' Each stratum's h2 at the last age of its h2 table (the row [run_h2()] gives it) enters
    #' when its h2 and SE are finite and its SE is positive; the entering rows are pooled by
    #' inverse-variance weights. `run_h2(meta_analyze=)` instead pools by age, so at its last
    #' age only the strata whose table reaches that age contribute.
    #'
    #' @param cif_pop Analysis arguments for population cumulative incidence, with `stratify_columns`. See run_cif
    #'   for details.
    #' @param cif_fh Analysis arguments for family history cumulative incidence. See run_cif for details.
    #' @param relatedness Relatedness coefficient to use in h2 calculation.
    #' @param method `"fixed"` (default) or `"random"` effects pooling, as in `meta_analyze`. Random needs at least
    #'   two entering strata.
    #' @returns A named list with metadata and results: one row per `index_trait` with `n_strata`, the range of
    #'   the strata's last ages `age_min` and `age_max`, and `h2`, `se`, `l95`, `u95`. With a pedigree, also
    #'   `sandwich_se`, `sandwich_l95` and `sandwich_u95`.
    run_h2_pooled = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$h2_pooled$properties)
      validator$add_post_validation(function(args, rules) {
        # The validator keeps unknown arguments, and h2_terms_at reads `meta_analyze` as a by-age pool.
        if ("meta_analyze" %in% names(args)) stop("run_h2_pooled takes `method`, not `meta_analyze`")

        private$check_h2_args(args, rules)
      })

      args     <- validator$run(...)
      metadata <- list(
        epimight_version   = as.character(packageVersion(methods::getPackageName())),
        analysis_name      = "h2_pooled",
        analysis_arguments = args,
        analysis_time      = format(Sys.time(), "%Y-%m-%dT%H:%M")
      )
      results  <- self$get_results("h2_pooled", args)

      if (is.null(results)) {
        last    <- private$last_age_rows(args)
        results <- private$analyses$core$run_meta(
          estimates       = last,
          estimate_column = "h2",
          se_column       = "se",
          group_columns   = list("index_trait")
        ) |>
          rename_with(~ str_remove(., sprintf("^%s_", args$method))) |>
          mutate(n_strata = nrow(last), age_min = min(last$age), age_max = max(last$age)) |>
          select(index_trait, n_strata, age_min, age_max, h2 = meta, se, l95, u95)

        self$add_results("h2_pooled", results, args)
      }

      out <- list(metadata = metadata, results = results)

      if (is.null(private$sandwich)) return(out)

      private$with_sandwich_columns("h2_pooled", out)
    },
    #' Produces genetic correlations for the two given traits.
    #'
    #' @param cif_cross Analysis arguments for cross trait cumulative incidence. See run_cif for details.
    #' @param h2_t1 Analysis arguments for trait 1 heritability. See run_h2 for details.
    #' @param h2_t2 Analysis arguments for trait 2 heritability. See run_h2 for details.
    #' @param relatedness Relatedness coefficient to use in rg calculation.
    #' @returns A named list with metadata and results.
    run_rg = function(...) {
      out <- private$native_rg(...)

      if (is.null(private$sandwich)) return(out)

      private$with_sandwich_columns("rg", out)
    },
    #' Produces genetic correlations for the two given traits using sane defaults.
    #'
    #' @param heritability1 Analysis arguments for heritability of trait 1.
    #' @param heritability2 Analysis arguments for heritability of trait 2.
    #' @param stratify_columns List of columns to stratify the results on.
    #' @param use_weighted_cif Boolean that controls whether weighted CIF is used or not (defaults to TRUE).
    #' @returns A named list with metadata and results.
    run_default_rg = function(...) {
      h2_rules <- list(
        required = TRUE,
        type = "named_list",
        properties = list(
          trait = list(
            required = TRUE,
            type     = "string"
          ),
          relatives_kind = list(
            required = FALSE,
            type     = "string"
          ),
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          )
        )
      )

      validator <- ArgumentsValidator$new(
        heritability1 = h2_rules,
        heritability2 = h2_rules,
        stratify_columns = list(
          type    = "list",
          items   = list(type = "string"),
          default = list()
        ),
        use_weighted_cif = list(
          type    = "logical",
          default = TRUE
        ),
        meta_analyze = self$validation_rules$meta_analyze
      )

      args <- validator$run(...)

      rg_args <- list(
        h2_t1 = list(
          cif_pop = list(
            index_trait      = args$heritability1$trait,
            stratify_columns = args$stratify_columns,
            use_weighted     = args$use_weighted_cif
          ),
          cif_fh = list(
            index_trait      = args$heritability1$trait,
            relatives_trait  = args$heritability1$trait,
            relatives_kind   = args$heritability1$relatives_kind,
            stratify_columns = args$stratify_columns,
            use_weighted     = args$use_weighted_cif
          ),
          relatedness  = args$heritability1$relatedness
        ),
        h2_t2 = list(
          cif_pop = list(
            index_trait      = args$heritability2$trait,
            stratify_columns = args$stratify_columns,
            use_weighted     = args$use_weighted_cif
          ),
          cif_fh = list(
            index_trait      = args$heritability2$trait,
            relatives_trait  = args$heritability2$trait,
            relatives_kind   = args$heritability2$relatives_kind,
            stratify_columns = args$stratify_columns,
            use_weighted     = args$use_weighted_cif
          ),
          relatedness  = args$heritability2$relatedness
        ),
        cif_cross = list(
          index_trait      = args$heritability1$trait,
          relatives_trait  = args$heritability2$trait,
          relatives_kind   = args$heritability2$relatives_kind,
          stratify_columns = args$stratify_columns,
          use_weighted     = args$use_weighted_cif
        ),
        relatedness  = args$heritability2$relatedness
      )

      if ("meta_analyze" %in% names(args)) {
        rg_args$h2_t1$meta_analyze <- args$meta_analyze
        rg_args$h2_t2$meta_analyze <- args$meta_analyze
        rg_args$meta_analyze       <- args$meta_analyze
      }

      do.call(self$run_rg, rg_args)
    }
  )
)
