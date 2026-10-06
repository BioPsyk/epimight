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
      cif = list(),
      h2  = list(),
      rg  = list()
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
    stratum_key = function(table, stratify_columns) {
      columns <- unlist(stratify_columns)

      if (length(columns) == 0) return(rep(".", nrow(table)))

      do.call(paste, c(unname(as.list(as.data.frame(table)[columns])), sep = "\r"))
    },
    cif_by_stratum = function(cif_args) {
      cif <- as.data.table(do.call(self$run_cif, cif_args)$results)

      data.table(stratum = private$stratum_key(cif, cif_args$stratify_columns), age = cif$age, cif = cif$cif)
    },
    # One h2 unit (see `h2_terms`) per row of `at`: `output`, `stratum`, `age`.
    h2_units = function(args, at) {
      units <- copy(at)

      units[private$cif_by_stratum(args$cif_pop), k_pop := i.cif, on = .(stratum, age)]
      units[private$cif_by_stratum(args$cif_fh), k_fh := i.cif, on = .(stratum, age)]

      if (anyNA(units$k_pop) || anyNA(units$k_fh)) {
        stop("Sandwich SE: no CIF row for an h2 value's stratum and age; the CIF and h2 tables disagree")
      }

      units[, `:=`(pop = private$cache_key("cif", args$cif_pop), fh = private$cache_key("cif", args$cif_fh))]
    },
    # Influence terms of the h2 values behind `at` (`stratum`, `age`): each stratum's own h2 at that age,
    # or with `meta_analyze` the pooled h2 at that age. Returns the output per row of `at` and the terms.
    h2_inputs = function(args, at, prefix) {
      if (!("meta_analyze" %in% names(args))) {
        output <- paste0(prefix, "|", at$stratum, "|", at$age)
        units  <- private$h2_units(args, data.table(output = output, stratum = at$stratum, age = at$age))

        return(list(output = output, terms = h2_terms(units, args$relatedness)))
      }

      sub_args              <- copy(args)
      sub_args$meta_analyze <- NULL

      local <- copy(as.data.table(do.call(self$run_h2, sub_args)$results))
      local[, `:=`(
        sandwich_stratum = private$stratum_key(local, sub_args$cif_pop$stratify_columns),
        sandwich_share   = meta_shares(h2, se, age, args$meta_analyze)
      )]
      pooled <- local[age %in% at$age & !is.na(sandwich_share)]
      pooled[, sandwich_output := paste0(prefix, "|", sandwich_stratum, "|", age)]

      units <- private$h2_units(sub_args, pooled[, .(output = sandwich_output, stratum = sandwich_stratum, age)])
      terms <- compose_terms(
        h2_terms(units, sub_args$relatedness),
        pooled[, .(from = sandwich_output, to = paste0(prefix, "|meta|", age), coef = sandwich_share)]
      )

      list(output = paste0(prefix, "|meta|", at$age), terms = terms)
    },
    # A sandwich plan names the output behind each results row (NA for none) and holds their term table.
    h2_plan = function(args, h2) {
      stratum <- if ("meta_analyze" %in% names(args)) {
        rep(".", nrow(h2))
      } else {
        private$stratum_key(h2, args$cif_pop$stratify_columns)
      }
      headline <- h2$age == stats::ave(h2$age, stratum, FUN = max)
      inputs   <- private$h2_inputs(args, data.table(stratum = stratum, age = h2$age)[headline], "h2")
      output   <- rep(NA_character_, nrow(h2))

      output[headline] <- inputs$output

      list(output = output, terms = inputs$terms)
    },
    # The plan of every row of `estimates` (from `rg_estimates`).
    rg_plan = function(args, estimates) {
      stratum <- private$stratum_key(estimates, args$cif_cross$stratify_columns)
      units   <- data.table(
        output  = paste0("rg|", stratum),
        stratum = stratum,
        age     = estimates$age,
        pop1    = private$cache_key("cif", args$h2_t1$cif_pop),
        cross   = private$cache_key("cif", args$cif_cross),
        pop2    = private$cache_key("cif", args$h2_t2$cif_pop),
        k_pop1  = estimates$t1_pop_cif,
        k_cross = estimates$cross_cif,
        k_pop2  = estimates$t2_pop_cif,
        h2_t1   = estimates$t1_h2,
        h2_t2   = estimates$t2_h2
      )
      t1 <- private$h2_inputs(args$h2_t1, units, "t1")
      t2 <- private$h2_inputs(args$h2_t2, units, "t2")
      units[, `:=`(h2_t1_output = t1$output, h2_t2_output = t2$output)]

      list(output = units$output, terms = rg_terms(units, rbind(t1$terms, t2$terms), args$relatedness))
    },
    # The plan of the meta-analyzed rg, pooled with run_meta's shares of the per-stratum rows.
    meta_rg_plan = function(args) {
      sub_args              <- copy(args)
      sub_args$meta_analyze <- NULL

      estimates <- private$rg_estimates(sub_args)
      local     <- private$rg_plan(sub_args, estimates)
      shares    <- data.table(
        from = local$output,
        to   = "rg|meta",
        coef = meta_shares(estimates$rg, estimates$rg_se, rep(1, nrow(estimates)), args$meta_analyze)
      )

      list(output = "rg|meta", terms = compose_terms(local$terms, shares[!is.na(coef)]))
    },
    native_h2 = function(...) {

      validator <- do.call(ArgumentsValidator$new, self$validation_rules$h2$properties)
      validator$add_post_validation(function(args, rules) {
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
      })

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
    # Adds the sandwich columns to the results a public run_h2 or run_rg call returns, cached or not. The
    # native_* methods never come through here, so the calls nested inside an analysis skip the sandwich.
    with_sandwich_columns = function(type, out) {
      args <- out$metadata$analysis_arguments
      key  <- private$cache_key(type, args)

      if (!("sandwich_se" %in% names(out$results))) {
        plan <- if (type == "h2") {
          private$h2_plan(args, out$results)
        } else if ("meta_analyze" %in% names(args)) {
          private$meta_rg_plan(args)
        } else {
          private$rg_plan(args, private$rg_estimates(args))
        }
        cifs <- if (type == "h2") list(args$cif_pop, args$cif_fh) else private$rg_cifs(args)
        fit  <- private$sandwich_fit(plan$terms, cifs)

        out$results <- private$with_sandwich(out$results, plan$output, fit, type)
        private$sandwich_runs[[type]][[key]] <- fit[c("batch_size", "passes")]
        self$add_results(type, out$results, args)
      }

      out$metadata$sandwich <- private$sandwich_runs[[type]][[key]]

      out
    },
    rg_cifs = function(args) {
      list(args$h2_t1$cif_pop, args$h2_t1$cif_fh, args$h2_t2$cif_pop, args$h2_t2$cif_fh, args$cif_cross)
    },
    sandwich_fit = function(terms, cif_args) {
      cohorts <- list()

      for (one in cif_args) {
        key <- private$cache_key("cif", one)

        if (!is.null(cohorts[[key]]) || !(key %chin% terms$cohort)) next

        tte <- do.call(self$get_tte, one)
        tte[, stratum := private$stratum_key(tte, one$stratify_columns)]
        cohorts[[key]] <- tte
      }

      private$sandwich$run(terms, cohorts)
    },
    # Adds the sandwich columns to `results`, whose rows carry the outputs `output` (NA for none).
    with_sandwich = function(results, output, fit, estimate) {
      variance <- fit$variance$variance[match(output, fit$variance$output)]
      negative <- !is.na(variance) & variance < 0

      if (any(negative)) {
        warning(sum(negative), " sandwich variance(s) came out negative and are reported as NA")
      }

      se      <- sqrt(ifelse(negative, NA_real_, variance))
      point   <- results[[estimate]]
      results <- as.data.table(results)

      set(results, j = "sandwich_se", value = se)
      set(results, j = "sandwich_l95", value = point - 1.96 * se)
      set(results, j = "sandwich_u95", value = point + 1.96 * se)

      results
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
    #'   `sandwich_l95`, `sandwich_u95`) at their headline age; this needs the `pedigreegraph` package.
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
        if (!requireNamespace("pedigreegraph", quietly = TRUE)) {
          stop("Sandwich standard errors need the `pedigreegraph` package; install it or leave out `pedigree`")
        }

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
      private$results       <- list(cif = list(), h2 = list(), rg = list())
      private$sandwich_runs <- list()
    },
    #' Adds the given analysis results to the cache.
    #'
    #' @param type A label that identifies what analysis produced the results: "cif", "h2" or "rg".
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
    #' @param type A label that identifies what analysis produced the results: "cif", "h2" or "rg".
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
