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
    pool                   = NULL,
    sub_analyses           = NULL,
    mk_run_validator_rules = function() {
      list(
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
        use_weighted_cif = list(
          type    = "logical",
          default = TRUE
        )
      )
    }
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
    #' Retrieves time-to-event data to use in a run based on the given disorders, relationship kind and
    #' straitfy columns. Makes sure that the retrieved data fulfills the requirements of carrying out
    #' a single pipeline run.
    get_tte = function(relkind, disorder1_id, disorder2_id, stratify_columns = NULL, use_weighted_cif = FALSE) {
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

      if (use_weighted_cif == TRUE) {
        combined <- combined |>
          mutate(
            d1_weight = ifelse(d1_relatives_diagnosed > 0.0, d1_relatives_diagnosed / relatives, 0.0),
            d2_weight = ifelse(d2_relatives_diagnosed > 0.0, d2_relatives_diagnosed / relatives, 0.0)
          ) |>
          as.data.table()
      }

      if (!is.list(stratify_columns)) return(combined)

      for (col in stratify_columns) {
        if (!(col %in% colnames(combined))) {
          stop("group_column \"", col, "\" was not found in TTE dataset: ", paste(colnames(combined), collapse = ", "))
        }
      }

      return(combined)
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
    run_cif = function(tte, disorder, cohort, stratify_columns, earliest_onset, latest_onset) {
      tte_renamed <- tte |>
        rename_with(
          ~ str_remove(., sprintf("^%s_", disorder)),
          starts_with(sprintf("%s_", disorder))
        ) |>
        as.data.table()

      if ("weight" %in% colnames(tte_renamed) && cohort == "c1") {
        tte_renamed <- tte_renamed |> mutate(weight = 1.0)
      }

      private$sub_analyses$cif$run(
        tte              = tte_renamed,
        stratify_columns = stratify_columns,
        earliest_onset   = earliest_onset,
        latest_onset     = latest_onset
      ) |>
        select(!!!stratify_columns, time, cif, cases, var, se, l95, u95) |>
        rename_with(~ paste0(cohort, "_", .), .cols = c(cif)) |>
        rename_with(~ paste0(cohort, "_cif_", .), .cols = c(cases, var, se, l95, u95))
    },
    #' @description
    #' Helper that runs h2 on the given time-to-event data and handles prefixing columns according to
    #' given disorder and cohort naming.
    run_h2 = function(disorder, cif_c1, cif_c2, relationship_kind, stratify_columns) {
      cif <-  cif_c1 |>
        inner_join(cif_c2, by = join_by(time, !!!stratify_columns)) |>
        self$max_time_by_stratification(stratify_columns)

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
    #' Helper that removes prefixes from column names that the function `run_cif` adds
    #' to its results.
    remove_cif_prefix = function(dt, disorder, cohort, stratify_columns) {
      prefix <- paste0(disorder, "_", cohort, "_")

      dt |>
        mutate(disorder = disorder, cohort = cohort) |>
        rename_with(
          ~ str_remove(., paste0("^", prefix)),
          .cols = starts_with(prefix)
        ) |>
        select(disorder, cohort, !!!stratify_columns, everything())
    },
    #' @description
    #' Helper that removes prefixes from column names that the function `run_h2` adds
    #' to its results.
    remove_h2_prefix = function(dt, disorder, stratify_columns) {
      prefix <- paste0(disorder, "_")

      dt |>
        mutate(disorder = disorder) |>
        rename_with(
          ~ str_remove(., paste0("^", prefix)),
          .cols = starts_with(prefix)
        ) |>
        select(disorder, !!!stratify_columns, everything())
    },
    #' @description
    #' Runs a single analysis using the given two disorders, relationship kind, straitfy colums
    #' and amount of draws.
    run = function(...) {
      validator <- do.call(ArgumentsValidator$new, private$mk_run_validator_rules())
      args      <- validator$run(...)

      tte_c1 <- self$get_tte(
        args$relationship_kind,
        args$disorder1$id,
        args$disorder2$id,
        args$stratify_columns,
        args$use_weighted_cif
      )

      tte_c2 <- tte_c1[d1_relatives_diagnosed > 0]
      if (nrow(tte_c2) == 0) stop("TTE cohort 2 had no rows")

      tte_c3 <- tte_c1[d2_relatives_diagnosed > 0]
      if (nrow(tte_c3) == 0) stop("TTE cohort 3 had no rows")

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

      cif_d1_c2 <- self$run_cif(
        tte_c2, "d1", "c2",
        args$stratify_columns,
        args$disorder1$earliest_onset,
        args$disorder1$latest_onset
      )
      if (is.null(cif_d1_c2)) stop("Disorder 1, cohort 2 had no TTE events")

      cif_d1_c3 <- self$run_cif(
        tte_c3, "d1", "c3",
        args$stratify_columns,
        args$disorder1$earliest_onset,
        args$disorder1$latest_onset
      )
      if (is.null(cif_d1_c3)) stop("Disorder 1, cohort 3 had no TTE events")

      cif_d2_c3 <- self$run_cif(
        tte_c3,
        "d2",
        "c3",
        args$stratify_columns,
        args$disorder2$earliest_onset,
        args$disorder2$latest_onset
      )
      if (is.null(cif_d2_c3)) stop("Disorder 2, cohort 3 had no TTE events")

      h2_d1 <- self$run_h2("d1", cif_d1_c1, cif_d1_c2, args$relationship_kind, args$stratify_columns)
      if (is.null(h2_d1)) stop("Disorder 1 had no h2 results")

      h2_d2 <- self$run_h2(
        "d2",
        cif_d2_c1,
        cif_d2_c3 |> rename(c2_cif = c3_cif, c2_cif_cases = c3_cif_cases),
        args$relationship_kind,
        args$stratify_columns
      )
      if (is.null(h2_d1)) stop("Disorder 2 had no h2 results")

      cif_d1_c1 <- cif_d1_c1 |> rename_with(~ paste0("d1_", .), .cols = starts_with("c1_"))
      cif_d1_c2 <- cif_d1_c2 |> rename_with(~ paste0("d1_", .), .cols = starts_with("c2_"))
      cif_d1_c3 <- cif_d1_c3 |> rename_with(~ paste0("d1_", .), .cols = starts_with("c3_"))
      cif_d2_c1 <- cif_d2_c1 |> rename_with(~ paste0("d2_", .), .cols = starts_with("c1_"))
      cif_d2_c3 <- cif_d2_c3 |> rename_with(~ paste0("d2_", .), .cols = starts_with("c3_"))

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
        self$max_time_by_stratification(args$stratify_columns) |>
        select(
          d1_c1_cif,
          d1_c3_cif,
          d2_c1_cif,
          d1_c1_cif_cases,
          d1_c3_cif_cases,
          d2_c1_cif_cases,
          d1_h2,
          d2_h2,
          !!!join_columns
        )

      if (nrow(combined) == 0) stop("After joining h2 results for both disorders no data was left")

      rg <- private$sub_analyses$gc$run(
        relationship_kind = args$relationship_kind,
        estimates         = combined
      ) |>
        select(!!!args$stratify_columns, rg, se, rg_l95, rg_u95) |>
        rename(rg_se = se)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      return(list(
        args = args,
        cif = rbindlist(list(
          self$remove_cif_prefix(cif_d1_c1, "d1", "c1", args$stratify_columns) |> select(-cif_var, -cif_se, -cif_l95, -cif_u95),
          self$remove_cif_prefix(cif_d1_c2, "d1", "c2", args$stratify_columns) |> select(-cif_var, -cif_se, -cif_l95, -cif_u95),
          self$remove_cif_prefix(cif_d1_c3, "d1", "c3", args$stratify_columns) |> select(-cif_var, -cif_se, -cif_l95, -cif_u95),
          self$remove_cif_prefix(cif_d2_c1, "d2", "c1", args$stratify_columns) |> select(-cif_var, -cif_se, -cif_l95, -cif_u95),
          self$remove_cif_prefix(cif_d2_c3, "d2", "c3", args$stratify_columns) |> select(-cif_var, -cif_se, -cif_l95, -cif_u95)
        )),
        h2 = rbindlist(list(
          self$remove_h2_prefix(h2_d1, "d1", args$stratify_columns),
          self$remove_h2_prefix(h2_d2, "d2", args$stratify_columns)
        )),
        rg = rg
      ))
    },
    run_meta = function(results) {
      validator <- ArgumentsValidator$new(
        args = list(
          required   = TRUE,
          type       = "named_list",
          properties = private$mk_run_validator_rules()
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
            h2_se = list(
              type     = "numeric",
              required = TRUE
            ),
            h2_l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            h2_u95 = list(
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
            rg_se = list(
              type     = "numeric",
              required = TRUE
            ),
            rg_l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            rg_u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        )
      )
      args <- do.call(validator$run, results)

      #---------------------------------------------------------------------------------
      # Cumulative incidence

      #cif_meta <- private$sub_analyses$core$run_meta(
      #  estimates        = args$cif,
      #  estimate_column  = "cif",
      #  se_column        = "cif_se",
      #  stratify_columns = list("disorder", "cohort", "time")
      #) |>
      #  select(disorder, cohort, everything())

      #---------------------------------------------------------------------------------
      # Heritability

      h2_meta <- private$sub_analyses$core$run_meta(
        estimates        = args$h2,
        estimate_column  = "h2",
        se_column        = "h2_se",
        stratify_columns = list("disorder")
      ) |>
        select(disorder, everything())

      #---------------------------------------------------------------------------------
      # Genetic correlation

      rg_meta <- private$sub_analyses$core$run_meta(
        estimates       = args$rg,
        estimate_column = "rg",
        se_column       = "rg_se"
      )

      return(list(
        #cif = cif_meta,
        h2  = h2_meta,
        rg  = rg_meta
      ))
    }
  )
)
