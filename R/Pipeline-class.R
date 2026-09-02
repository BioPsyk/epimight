#' @title Class that takes care of running all parts of the complete pipeline.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @import rjson
#' @export
Pipeline <- R6::R6Class( #nolint
  "Pipeline",
  private = list(
    pool     = NULL,
    cache    = NULL,
    analyses = NULL
  ),
  public = list(
    validation_rules = list(
      analysis = list(
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
            required = TRUE,
            type     = "string"
          ),
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          )
        )
      )
    ),
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

      args             <- validator$run(...)
      private$pool     <- args$pool
      private$cache    <- list()
      private$analyses <- list(
        core = Analysis$new(),
        h2   = HeritabilityAnalysis$new(),
        cif  = CumulativeIncidenceAnalysis$new(),
        rg   = GeneticCorrelationAnalysis$new()
      )
      self$validation_rules$run <- list(
        heritability1       = self$validation_rules$analysis,
        heritability2       = self$validation_rules$analysis,
        genetic_correlation = self$validation_rules$analysis,
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
      self$validation_rules$run$genetic_correlation$required                 <- FALSE
      self$validation_rules$run$genetic_correlation$relatives_trait$required <- TRUE
    },
    add_to_cache = function(results, ...) {
      if (!is.data.table(results)) stop("Given results was not a data.table")

      walk_deep <- function(target, path, value) {
        step <- path[[1]]

        if (length(path) == 1) {
          target[[step]] <- value
          return(target)
        }

        if (!step %in% names(target)) {
          target[[step]] <- list()
        }

        target[[step]] <- walk_deep(target[[step]], path[-1], value)

        return(target)
      }

      private$cache <- walk_deep(private$cache, list(...), results)
    },
    get_from_cache = function(...) {
      walk_deep <- function(target, path) {
        if (!is.list(path)) return(NULL)

        step <- path[[1]]

        if (is.null(step)) return(NULL)

        if (!step %in% names(target)) {
          return(NULL)
        }

        if (length(path) == 1) {
          results <- target[[step]]

          if (!is.data.table(results)) return(NULL)

          return(results)
        }

        walk_deep(target[[step]], path[-1])
      }

      walk_deep(private$cache, list(...))
    },
    #' @description
    #' Retrieves time-to-event data to use in a run based on the given traits, relationship kind and
    #' straitfy columns. Makes sure that the retrieved data fulfills the requirements of carrying out
    #' a single pipeline run.
    get_tte = function(stratify_columns, use_weighted_cif, index_trait, rel_trait = NULL, rel_kind = NULL) {
      columns <- c("person_id", "trait_status", "trait_age")

      if (!is.null(stratify_columns) && is.list(stratify_columns)) {
        columns <- c(columns, unlist(stratify_columns))
      }

      for (col in columns) {
        if (!(col %in% colnames(private$pool))) {
          stop("Column \"", col, "\" was not found in the TTE pool: ", paste(colnames(private$pool), collapse = ", "))
        }
      }

      tte <- private$pool[
        trait == index_trait
      ][
        , .SD[1], by = "person_id"
      ][
        , ..columns
      ]

      if (nrow(tte) == 0) stop(paste0("No proband TTE data found for trait ", index_trait))

      if (!is.null(rel_trait) && !is.null(rel_kind)) {
        relatives_tte <- private$pool[
          trait == rel_trait & relatives_kind == rel_kind
        ][
          , .SD[1], by = "person_id"
        ][
          , c("person_id", "relatives_kind", "relatives_n", "relatives_n_trait")
        ]

        if (nrow(relatives_tte) == 0) {
          stop(paste0(
            "No family history TTE data found for trait \"", rel_trait,
            "\" and relationship kind \"", rel_kind, "\""
          ))
        }

        tte <- tte[
          relatives_tte,
          on = .(person_id = person_id)
        ][
          relatives_n_trait > 0
        ]

        if (isTRUE(use_weighted_cif)) {
          tte <- tte[
            , weight := ifelse(relatives_n_trait > 0.0, relatives_n_trait / relatives_n, 0.0)
          ]
        }

        if (nrow(tte) == 0) {
          stop(paste0(
            "No probands with at least 1 relative (of kind \"",
            rel_kind, "\") with trait \"", rel_trait, "\""
          ))
        }
      }

      return(tte)
    },
    #' @description
    #' Helper that aggregates the given estimates dataset down to a single row per stratification
    #' combination, where the kept row is the one with the largest `time` value within it's group.
    #'
    #' If we have a dataset of cumulative incidences stratified by birth year and sex, each
    #' stratification combination will have multiple rows, like this:
    #'
    #'   |------+------------+--------+------------+------|
    #'   | time | birth_year | sex    |  estimates | case |
    #'   |------+------------+--------+------------+------|
    #'   |   43 |       1981 | f      | 0.10639881 |  141 |
    #'   |   42 |       1981 | f      | 0.09763101 |  131 |
    #'   |   41 |       1981 | f      | 0.09335325 |  125 |
    #'   |   43 |       1981 | m      | 0.09816850 |  134 |
    #'   |   40 |       1981 | m      | 0.09417040 |  122 |
    #'   |   39 |       1981 | m      | 0.09747292 |  141 |
    #'   |------+------------+--------+------------+------|
    #'
    #' Running `max_time_by_stratification(cif_example, list("birth_year", "sex"))`
    #' on this dataset would produce:
    #'
    #' |------+------------+--------+------------+------|
    #' | time | birth_year | sex    |  estimates | case |
    #' |------+------------+--------+------------+------|
    #' |   43 |       1981 | f      | 0.10639881 |  141 |
    #' |   43 |       1981 | m      | 0.09816850 |  134 |
    #' |------+------------+--------+------------+------|
    max_time_by_stratification = function(estimates, stratify_columns) {
      estimates |>
        group_by(!!!rlang::syms(stratify_columns)) |>
        arrange(desc(time)) |>
        filter(row_number() == 1) |>
        as.data.table()
    },
    #' @description
    #' Helper that runs cif on the given time-to-event data and handles prefixing columns according to
    #' given trait and cohort naming.
    run_cif = function(stratify_columns, use_weighted_cif, index_trait, rel_trait = NULL, rel_kind = NULL) {
      tte <- self$get_tte(stratify_columns, use_weighted_cif, index_trait, rel_trait, rel_kind)
      cif <- private$analyses$cif$run(
        tte              = tte,
        stratify_columns = stratify_columns
      ) |>
        mutate(
          index_trait     = index_trait,
          relatives_trait = ifelse(is.null(rel_trait), NA, rel_trait),
          relatives_kind  = ifelse(is.null(rel_kind), NA, rel_kind)
        ) |>
        select(
          index_trait,
          relatives_trait,
          relatives_kind,
          all_of(unlist(stratify_columns)),
          time,
          everything()
        )

      #self$add_to_cache(cif, list("cif", analysis$index_trait))
      #self$get_from_cache("cif", analysis$index_trait)

      if (is.null(cif)) {
        stop(paste0(
          "No TTE events found when producing cif_", index_trait, "_", rel_trait, "_", rel_kind
        ))
      }

      return(cif)
    },
    #' @description
    #' Helper that runs h2 on the given time-to-event data and handles prefixing columns according to
    #' given trait and cohort naming.
    run_h2 = function(analysis, stratify_columns, cif_pop, cif_fh) {
      stratify_symbols <- rlang::syms(stratify_columns)

      cif <- cif_pop |>
        inner_join(cif_fh, by = join_by(time, !!!stratify_columns)) |>
        rename(
          pop_cif   = cif.x,
          pop_cases = cases.x,
          fh_cif    = cif.y,
          fh_cases  = cases.y
        ) |>
        select(time, !!!stratify_symbols, pop_cif, pop_cases, fh_cif, fh_cases)

      h2 <- private$analyses$h2$run(
        cif         = cif,
        relatedness = analysis$relatedness
      ) |>
        mutate(index_trait = analysis$index_trait) |>
        select(index_trait, time, !!!stratify_symbols, h2, se, l95, u95)

      if (is.null(h2)) stop(paste0("No valid results found when producing h2_", trait_key))

      return(h2)
    },
    add_cif_prefix = function(cif, prefix, stratify_columns) {
      cif |>
        select(!!!stratify_columns, time, cif, cases) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(cif, cases))
    },
    #' @description
    #' Helper that removes prefixes from column names that the function `run_h2` adds
    #' to its results.
    add_h2_prefix = function(h2, prefix, stratify_columns) {
      h2 |>
        select(!!!stratify_columns, time, h2) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(h2))
    },
    #' @description
    #' Runs a single analysis using the given two traits, relationship kind, straitfy colums
    #' and amount of draws.
    run = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$run)

      args <- validator$run(...)

      if (!("relatives_trait" %in% names(args$heritability1))) {
        args$heritability1$relatives_trait <- args$heritability1$index_trait
      }

      if (!("relatives_trait" %in% names(args$heritability2))) {
        args$heritability2$relatives_trait <- args$heritability2$index_trait
      }

      if (!("genetic_correlation" %in% names(args))) {
        args$genetic_correlation <- list(
          index_trait     = args$heritability1$index_trait,
          relatives_trait = args$heritability2$relatives_trait,
          relatives_kind  = args$heritability2$relatives_kind,
          relatedness     = args$heritability2$relatedness
        )
      }

      cif_t1_pop <- self$run_cif(
        args$stratify_columns,
        args$use_weighted_cif,
        args$heritability1$index_trait
      )

      stop("asd")
      cif_t1_fh1 <- self$run_cif(args$heritability1, args$stratify_columns, args$use_weighted_cif)
      cif_cross  <- self$run_cif(args$genetic_correlation, args$stratify_columns, args$use_weighted_cif)
      cif_t2_pop <- self$run_cif(
        list(index_trait = args$heritability2$index_trait),
        args$stratify_columns,
        args$use_weighted_cif
      )
      cif_t2_fh2 <- self$run_cif(args$heritability2, args$stratify_columns, args$use_weighted_cif)

      h2_t1 <- self$run_h2(args$heritability1, args$stratify_columns, cif_t1_pop, cif_t1_fh1)
      h2_t2 <- self$run_h2(args$heritability2, args$stratify_columns, cif_t2_pop, cif_t2_fh2)

      join_columns <- c(list("time"), args$stratify_columns)
      join_symbols <- rlang::syms(join_columns)

      combined <- self$add_cif_prefix(cif_t1_pop, "t1_pop", args$stratify_columns) |>
        inner_join(
          self$add_cif_prefix(cif_cross, "cross", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_cif_prefix(cif_t2_pop, "t2_pop", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_t1, "t1", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_t2, "t2", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        self$max_time_by_stratification(args$stratify_columns)

      if (nrow(combined) == 0) stop("After joining all cif and h2 results no data was left")

      rg <- private$analyses$rg$run(
        estimates   = combined,
        relatedness = args$genetic_correlation$relatedness
      ) |>
        select(!!!args$stratify_columns, rg, se, l95, u95)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      list(
        metadata = list(
          version   = as.character(packageVersion(methods::getPackageName())),
          arguments = args
        ),
        cif = rbindlist(list(
          cif_t1_pop,
          cif_t1_fh1,
          cif_cross,
          cif_t2_pop,
          cif_t2_fh2
        )),
        h2 = rbindlist(list(h2_t1, h2_t2)),
        rg = rg
      )
    },
    run_meta = function(results) {
      validator <- ArgumentsValidator$new(
        metadata = list(
          required   = TRUE,
          type       = "named_list",
          properties = list(
            version = list(
              required   = TRUE,
              type       = "string"
            ),
            arguments = list(
              required   = TRUE,
              type       = "named_list",
              properties = self$validation_rules$run
            )
          )
        ),
        cif = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            index_trait = list(
              type     = "string",
              required = TRUE
            ),
            relatives_trait = list(
              type     = "string",
              required = TRUE
            ),
            relatives_kind = list(
              type     = "string",
              required = TRUE
            ),
            cif = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        ),
        h2 = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            index_trait = list(
              type     = "string",
              required = TRUE
            ),
            h2 = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        ),
        rg = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            rg = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        )
      )
      args <- do.call(validator$run, results)

      #---------------------------------------------------------------------------------
      # Cumulative incidence

      cif_meta <- private$analyses$core$run_meta(
        estimates        = args$cif,
        estimate_column  = "cif",
        se_column        = "se",
        stratify_columns = list("index_trait", "relatives_trait", "relatives_kind", "time")
      ) |>
        select(index_trait, relatives_trait, relatives_kind, everything())

      #---------------------------------------------------------------------------------
      # Heritability

      h2_meta <- private$analyses$core$run_meta(
        estimates        = args$h2,
        estimate_column  = "h2",
        se_column        = "se",
        stratify_columns = list("index_trait")
      ) |>
        select(index_trait, everything())

      #---------------------------------------------------------------------------------
      # Genetic correlation

      rg_meta <- private$analyses$core$run_meta(
        estimates       = args$rg,
        estimate_column = "rg",
        se_column       = "se"
      )

      return(list(
        cif = cif_meta,
        h2  = h2_meta,
        rg  = rg_meta
      ))
    }
  )
)
