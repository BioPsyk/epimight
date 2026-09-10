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
    pool     = NULL,
    analyses = NULL,
    results  = list(
      cif = list(),
      h2  = list(),
      rg  = list()
    ),
    max_age_by_stratification = function(results, stratify_columns) {
      results |>
        group_by(!!!rlang::syms(stratify_columns)) |>
        arrange(desc(age)) |>
        filter(row_number() == 1) |>
        as.data.table()
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
    }
  ),
  public = list(
    validation_rules = list(),
    #' Creates a pipeline instance ready to be used to run analyses.
    #'
    #' @seealso [run_default_rg()] For quickly running a full genetic correlation.
    #'
    #' @param pool The pool that contains time-to-event data for all traits and kinds of relatives you want to analyze.
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
        )
      )

      args         <- validator$run(...)
      private$pool <- args$pool

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
          )
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
          )
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
          )
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
    #' Removes all results from the cache.
    clear_results = function() {
      private$results <- list(cif = list(), h2 = list(), rg = list())
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

      rules     <- self$validation_rules[[type]]
      validator <- do.call(ArgumentsValidator$new, rules$properties)
      args      <- do.call(validator$run, args)
      args      <- args[order(names(args))]
      key       <- rjson::toJSON(args)

      private$results[[type]][[key]] <- results
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

      rules     <- self$validation_rules[[type]]
      validator <- do.call(ArgumentsValidator$new, rules$properties)
      args      <- do.call(validator$run, args)
      args      <- args[order(names(args))]
      key       <- rjson::toJSON(args)

      private$results[[type]][[key]]
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
    #' @returns A named list with metadata, results and intermediate results.
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
        return(list(
          metadata = metadata,
          results  = cached_cif
        ))
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

      list(
        metadata = metadata,
        results  = cif
      )
    },
    #' Produces heritability for the given trait and relative kind.
    #'
    #' @param cif_pop Analysis arguments for population cumulative incidence. See run_cif for details.
    #' @param cif_fh Analysis arguments for family history cumulative incidence. See run_cif for details.
    #' @param relatedness Relatedness coefficient to use in h2 calculation.
    #' @returns A named list with metadata, results and intermediate results.
    run_h2 = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$h2$properties)
      validator$add_post_validation(function(args, rules) {
        if ("relatives_trait" %in% names(args$cif_pop)) {
          stop("Using `relatives_trait` in `cif_pop` is not allowed")
        }

        if ("relatives_kind" %in% names(args$cif_pop)) {
          stop("Using `relatives_kind` in `cif_pop` is not allowed")
        }

        if (args$cif_pop$index_trait != args$cif_fh$index_trait) {
          stop("Using different `index_traits` in `cif_pop` and `cif_fh` is not allowed")
        }

        if (args$cif_fh$index_trait != args$cif_fh$relatives_trait) {
          stop("Using different `index_traits` and `relatives_trait` in `cif_fh` is not allowed")
        }

        if (!identical(args$cif_pop$stratify_columns, args$cif_fh$stratify_columns)) {
          stop("Using different `stratify_columns` in `cif_pop` and `cif_fh` is not allowed")
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

      cif_pop <- do.call(self$run_cif, args$cif_pop)
      cif_fh  <- do.call(self$run_cif, args$cif_fh)

      if (!is.null(cached_h2)) {
        return(list(
          metadata = metadata,
          results  = cached_h2,
          intermediate = list(
            cif_pop = cif_pop,
            cif_fh  = cif_fh
          )
        ))
      }

      stratify_columns <- args$cif_pop$stratify_columns
      stratify_symbols <- rlang::syms(stratify_columns)

      cif <- cif_pop$results |>
        inner_join(cif_fh$results, by = join_by(age, !!!stratify_columns)) |>
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

      list(
        metadata     = metadata,
        results      = h2,
        intermediate = list(
          cif_pop = cif_pop,
          cif_fh  = cif_fh
        )
      )
    },
    #' Produces genetic correlations for the two given traits.
    #'
    #' @param cif_cross Analysis arguments for cross trait cumulative incidence. See run_cif for details.
    #' @param h2_t1 Analysis arguments for trait 1 heritability. See run_h2 for details.
    #' @param h2_t2 Analysis arguments for trait 2 heritability. See run_h2 for details.
    #' @param relatedness Relatedness coefficient to use in rg calculation.
    #' @returns A named list with metadata, results and intermediate results.
    run_rg = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$rg$properties)

      validator$add_post_validation(function(args, rules) {
        if (!identical(args$h2_t1$cif_pop$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `h2_t1` and `h2_t2` is not allowed")
        }

        if (!identical(args$cif_cross$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `cif_cross`, `h2_t1` and `h2_t2` is not allowed")
        }

        args
      })

      args      <- validator$run(...)
      metadata  <- list(
        epimight_version   = as.character(packageVersion(methods::getPackageName())),
        analysis_name      = "rg",
        analysis_arguments = args,
        analysis_time      = format(Sys.time(), "%Y-%m-%dT%H:%M")
      )
      cached_rg <- self$get_results("rg", args)

      cif_t1_pop <- do.call(self$run_cif, args$h2_t1$cif_pop)
      cif_t2_pop <- do.call(self$run_cif, args$h2_t2$cif_pop)
      h2_t1      <- do.call(self$run_h2, args$h2_t1)
      h2_t2      <- do.call(self$run_h2, args$h2_t1)
      cif_cross  <- do.call(self$run_cif, args$cif_cross)

      if (!is.null(cached_rg)) {
        return(list(
          metadata = metadata,
          results  = cached_rg,
          intermediate = list(
            cif_t1_pop = cif_t1_pop,
            cif_t2_pop = cif_t2_pop,
            h2_t1      = h2_t1,
            h2_t2      = h2_t2,
            cif_cross  = cif_cross
          )
        ))
      }

      stratify_columns <- args$cif_cross$stratify_columns
      join_columns     <- c(list("age"), stratify_columns)
      join_symbols     <- rlang::syms(join_columns)

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
        private$max_age_by_stratification(stratify_columns)

      if (nrow(combined) == 0) stop("After joining all cif and h2 results no data was left")

      rg <- private$analyses$rg$run(
        estimates   = combined,
        relatedness = args$relatedness
      ) |>
        select(!!!stratify_columns, rg, se, l95, u95)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      self$add_results("rg", rg, args)

      list(
        metadata     = metadata,
        results      = rg,
        intermediate = list(
          cif_t1_pop = cif_t1_pop,
          cif_t2_pop = cif_t2_pop,
          h2_t1      = h2_t1,
          h2_t2      = h2_t2,
          cif_cross  = cif_cross
        )
      )
    },
    #' Produces genetic correlations for the two given traits using sane defaults.
    #'
    #' @param heritability1 Analysis arguments for heritability of trait 1.
    #' @param heritability2 Analysis arguments for heritability of trait 2.
    #' @param stratify_columns List of columns to stratify the results on.
    #' @param use_weighted_cif Boolean that controls whether weighted CIF is used or not (defaults to TRUE).
    #' @returns A named list with metadata, results and intermediate results.
    run_default_rg = function(...) {
      heritability_rules <- list(
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
        heritability1 = heritability_rules,
        heritability2 = heritability_rules,
        stratify_columns = list(
          type    = "list",
          items   = list(type = "string"),
          default = list()
        ),
        use_weighted_cif = list(
          type    = "logical",
          default = TRUE
        )
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
          relatedness = args$heritability1$relatedness
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
          relatedness = args$heritability2$relatedness
        ),
        cif_cross = list(
          index_trait      = args$heritability1$trait,
          relatives_trait  = args$heritability2$trait,
          relatives_kind   = args$heritability2$relatives_kind,
          stratify_columns = args$stratify_columns,
          use_weighted     = args$use_weighted_cif
        ),
        relatedness = args$heritability2$relatedness
      )

      do.call(self$run_rg, rg_args)
    },
    #' Meta analyzes stratified analysis (supports rg, h2 and cif) results.
    #'
    #' @param metadata Metadata from analysis that produced the given results.
    #' @param results Data.table with the analysis results to meta analyze.
    #' @returns A data.table with the meta analyzed results.
    run_meta = function(...) {
      validator <- ArgumentsValidator$new(
        metadata = list(
          type       = "named_list",
          required   = TRUE,
          strict     = FALSE,
          properties = list(
            analysis_name = list(
              type     = "string",
              required = TRUE
            )
          )
        ),
        results = list(
          type     = "data.table",
          required = TRUE,
          columns  = list(
            se = list(
              type     = "numeric",
              required = TRUE
            )
          )
        )
      )

      args <- validator$run(...)

      if (args$metadata$analysis_name == "cif") {
        estimate_column  <- "cif"
        stratify_columns <- list("index_trait", "relatives_trait", "relatives_kind", "age")

      } else if (args$metadata$analysis_name == "h2") {
        estimate_column  <- "h2"
        stratify_columns <- list("index_trait")
      } else if (args$metadata$analysis_name == "rg") {
        estimate_column  <- "rg"
        stratify_columns <- list()
      } else {
        stop(paste0("Unknown metadata$analysis_name: ", args$metadata$analysis_name))
      }

      private$analyses$core$run_meta(
        estimates        = args$results,
        estimate_column  = estimate_column,
        se_column        = "se",
        stratify_columns = stratify_columns
      )
    }
  )
)
