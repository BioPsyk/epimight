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
    pool      = NULL,
    analyses  = NULL
  ),
  public = list(
    validation_rules = list(
      run = list(
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
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            ),
            relatives_coefficient = list(
              required = TRUE,
              type     = "numeric"
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
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            ),
            relatives_coefficient = list(
              required = TRUE,
              type     = "numeric"
            )
          )
        ),
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
            relatives = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            ),
            relatives_diagnosed = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            )
          )
        )
      )

      args             <- validator$run(...)
      private$pool     <- args$pool
      private$analyses <- list(
        core = Analysis$new(),
        h2   = HeritabilityAnalysis$new(),
        cif  = CumulativeIncidenceAnalysis$new(),
        rg   = GeneticCorrelationAnalysis$new()
      )

    },
    #' @description
    #' Retrieves time-to-event data to use in a run based on the given disorders, relationship kind and
    #' straitfy columns. Makes sure that the retrieved data fulfills the requirements of carrying out
    #' a single pipeline run.
    get_tte = function(...) {
      validator <- ArgumentsValidator$new(
        list(
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
        list(
          required   = FALSE,
          type       = "named_list",
          properties = list(
            id = list(
              required = TRUE,
              type     = "string"
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            )
          )
        ),
        self$validation_rules$run$stratify_columns,
        self$validation_rules$run$use_weighted_cif
      )

      args <- validator$run(...)

      proband_disorder   <- args[[1]]
      relatives_disorder <- args[[2]]
      stratify_columns   <- args[[3]]
      use_weighted_cif   <- args[[4]]

      columns <- c("person_id", "failure_status", "failure_time")

      if (!is.null(stratify_columns) && is.list(stratify_columns)) {
        columns <- c(columns, unlist(stratify_columns))
      }

      for (col in columns) {
        if (!(col %in% colnames(private$pool))) {
          stop("Column \"", col, "\" was not found in the TTE pool: ", paste(colnames(private$pool), collapse = ", "))
        }
      }

      tte <- private$pool[
        disorder == proband_disorder$id
      ][
        , .SD[1], by = "person_id"
      ][
        , ..columns
      ]

      if (nrow(tte) == 0) stop(paste0("No proband TTE data found for disorder", proband_disorder))

      if (is.list(relatives_disorder)) {
        relative_tte <- private$pool[
          disorder == relatives_disorder$id & relatives_kind == relatives_disorder$relatives_kind
        ][
          , .SD[1], by = "person_id"
        ][
          , c("person_id", "relatives", "relatives_diagnosed", "relatives_kind")
        ]

        if (nrow(relative_tte) == 0) {
          stop(paste0(
            "No family history TTE data found for disorder \"", relatives_disorder$id,
            "\" and relationship kind \"", relatives_disorder$relatives_kind, "\""
          ))
        }

        tte <- tte[
          relative_tte,
          on = .(person_id = person_id)
        ][
          relatives_diagnosed > 0
        ]

        if (isTRUE(use_weighted_cif)) {
          tte <- tte[
            , weight := ifelse(relatives_diagnosed > 0.0, relatives_diagnosed / relatives, 0.0)
          ]
        }

        if (nrow(tte) == 0) {
          stop(paste0(
            "No probands with at least 1 relative (of kind \"",
            relatives_disorder$relatives_kind, "\") diagnosed with \"", relatives_disorder$id, "\""
          ))
        }
      }

      return(tte)
    },
    #' @description
    #' Helper that aggregates the given estimates dataset down to a single row per stratification
    #' combination, where the kept row is the one with the largest `time` value within it's group.
    #'
    #' If we have a dataset of cumulative incidences stratified by birth year and gender, each
    #' stratification combination will have multiple rows, like this:
    #'
    #'   |------+------------+--------+------------+------|
    #'   | time | birth_year | gender |  estimates | case |
    #'   |------+------------+--------+------------+------|
    #'   |   43 |       1981 | f      | 0.10639881 |  141 |
    #'   |   42 |       1981 | f      | 0.09763101 |  131 |
    #'   |   41 |       1981 | f      | 0.09335325 |  125 |
    #'   |   43 |       1981 | m      | 0.09816850 |  134 |
    #'   |   40 |       1981 | m      | 0.09417040 |  122 |
    #'   |   39 |       1981 | m      | 0.09747292 |  141 |
    #'   |------+------------+--------+------------+------|
    #'
    #' Running `max_time_by_stratification(cif_example, list("birth_year", "gender"))`
    #' on this dataset would produce:
    #'
    #' |------+------------+--------+------------+------|
    #' | time | birth_year | gender |  estimates | case |
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
    #' given disorder and cohort naming.
    run_cif = function(disorder_key, cohort_key, proband_disorder, relatives_disorder, stratify_columns, use_weighted_cif) {
      tte <- self$get_tte(proband_disorder, relatives_disorder, stratify_columns, use_weighted_cif)
      cif <- private$analyses$cif$run(
        tte              = tte,
        stratify_columns = stratify_columns,
        earliest_onset   = proband_disorder$earliest_onset,
        latest_onset     = proband_disorder$latest_onset
      ) |>
        mutate(disorder = disorder_key, cohort = cohort_key) |>
        select(disorder, cohort, all_of(unlist(stratify_columns)), time, everything())

      if (is.null(cif)) stop(paste0("No TTE events found when producing cif_", disorder_key, "_", cohort_key))

      return(cif)
    },
    #' @description
    #' Helper that runs h2 on the given time-to-event data and handles prefixing columns according to
    #' given disorder and cohort naming.
    run_h2 = function(disorder_key, relationship_coefficient, stratify_columns, cif_pop, cif_fh) {
      stratify_symbols <- rlang::syms(stratify_columns)

      cif <- cif_pop |>
        inner_join(cif_fh, by = join_by(time, !!!stratify_columns)) |>
        rename(
          pop_cif   = cif.x,
          pop_cases = cases.x,
          fh_cif    = cif.y,
          fh_cases  = cases.y
        ) |>
        select(time, !!!stratify_symbols, pop_cif, pop_cases, fh_cif, fh_cases) |>
        self$max_time_by_stratification(stratify_columns)

      h2 <- private$analyses$h2$run(
        cif                      = cif,
        relationship_coefficient = relationship_coefficient
      ) |>
        mutate(disorder = disorder_key) |>
        select(disorder, time, !!!stratify_symbols, h2, se, l95, u95)

      if (is.null(h2)) stop(paste0("No valid results found when producing h2_", disorder_key))

      return(h2)
    },
    add_cif_prefix = function(cif, disorder_key, cohort_key, stratify_columns) {
      cif |>
        select(!!!stratify_columns, time, cif, cases) |>
        rename_with(~ paste0(disorder_key, "_", cohort_key, "_", .), .cols = c(cif, cases))
    },
    #' @description
    #' Helper that removes prefixes from column names that the function `run_h2` adds
    #' to its results.
    add_h2_prefix = function(h2, disorder_key, stratify_columns) {
      h2 |>
        select(!!!stratify_columns, time, h2) |>
        rename_with(~ paste0(disorder_key, "_", .), .cols = c(h2))
    },
    #' @description
    #' Runs a single analysis using the given two disorders, relationship kind, straitfy colums
    #' and amount of draws.
    run = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$run)

      args <- validator$run(...)

      cif_d1_pop <- self$run_cif("d1", "pop", args$disorder1, NA, args$stratify_columns, args$use_weighted_cif)
      cif_d1_fh1 <- self$run_cif("d1", "fh1", args$disorder1, args$disorder1, args$stratify_columns, args$use_weighted_cif)
      cif_d1_fh2 <- self$run_cif("d1", "fh2", args$disorder1, args$disorder2, args$stratify_columns, args$use_weighted_cif)
      cif_d2_pop <- self$run_cif("d2", "pop", args$disorder2, NA, args$stratify_columns, args$use_weighted_cif)
      cif_d2_fh2 <- self$run_cif("d2", "fh2", args$disorder2, args$disorder2, args$stratify_columns, args$use_weighted_cif)

      h2_d1 <- self$run_h2("d1", args$disorder1$relatives_coefficient, args$stratify_columns, cif_d1_pop, cif_d1_fh1)
      h2_d2 <- self$run_h2("d1", args$disorder2$relatives_coefficient, args$stratify_columns, cif_d2_pop, cif_d2_fh2)

      join_columns <- c(list("time"), args$stratify_columns)
      join_symbols <- rlang::syms(join_columns)

      combined <- self$add_cif_prefix(cif_d1_pop, "d1", "pop", args$stratify_columns) |>
        inner_join(
          self$add_cif_prefix(cif_d1_fh2, "d1", "fh2", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_cif_prefix(cif_d2_pop, "d2", "pop", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_d1, "d1", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_d2, "d2", args$stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        self$max_time_by_stratification(args$stratify_columns)

      if (nrow(combined) == 0) stop("After joining h2 results for both disorders no data was left")

      rg <- private$analyses$rg$run(
        estimates                = combined,
        relationship_coefficient = args$disorder2$relatives_coefficient
      ) |>
        select(!!!args$stratify_columns, rg, se, l95, u95)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      list(
        args = args,
        cif = rbindlist(list(
          cif_d1_pop,
          cif_d1_fh1,
          cif_d1_fh2,
          cif_d2_pop,
          cif_d2_fh2
        )) |> select(-var, -se, -l95, -u95), # We do not produce these statistics with the weighted method.
        h2 = rbindlist(list(h2_d1, h2_d2)),
        rg = rg
      )
    },
    run_meta = function(results) {
      validator <- ArgumentsValidator$new(
        args = list(
          required   = TRUE,
          type       = "named_list",
          properties = self$validation_rules$run
        ),
        #cif = list(
        #  required = TRUE,
        #  type     = "data.table",
        #  columns  = list(
        #    disorder = list(
        #      type     = "string",
        #      required = TRUE
        #    ),
        #    cohort = list(
        #      type     = "string",
        #      required = TRUE
        #    ),
        #    cif = list(
        #      type     = "numeric",
        #      required = TRUE
        #    ),
        #    cif_var = list(
        #      type = "numeric"
        #    ),
        #    cif_se = list(
        #      type = "numeric"
        #    ),
        #    cif_l95 = list(
        #      type = "numeric"
        #    ),
        #    cif_u95 = list(
        #      type = "numeric"
        #    ),
        #    cif_cases = list(
        #      type     = "numeric",
        #      required = TRUE
        #    )
        #  )
        #),
        h2 = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            disorder = list(
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

      #cif_meta <- private$analyses$core$run_meta(
      #  estimates        = args$cif,
      #  estimate_column  = "cif",
      #  se_column        = "cif_se",
      #  stratify_columns = list("disorder", "cohort", "time")
      #) |>
      #  select(disorder, cohort, everything())

      #---------------------------------------------------------------------------------
      # Heritability

      h2_meta <- private$analyses$core$run_meta(
        estimates        = args$h2,
        estimate_column  = "h2",
        se_column        = "se",
        stratify_columns = list("disorder")
      ) |>
        select(disorder, everything())

      #---------------------------------------------------------------------------------
      # Genetic correlation

      rg_meta <- private$analyses$core$run_meta(
        estimates       = args$rg,
        estimate_column = "rg",
        se_column       = "se"
      )

      return(list(
        #cif = cif_meta,
        h2  = h2_meta,
        rg  = rg_meta
      ))
    }
  )
)
