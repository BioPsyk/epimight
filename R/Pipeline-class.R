#' @title Class that takes care of running all parts of the complete pipeline.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @export
Pipeline <- R6::R6Class( #nolint
  "Pipeline",
  private = list(
    tte = NULL,
    analysis = NULL,
    #' @description
    #' Downsample relative counts to independent Bernoulli indicators.
    #'
    #' For each individual, draws a 0/1 indicator with probability
    #' p = relatives_diagnosed / relatives.  This avoids cohort dilution
    #' at high prevalence, where nearly everyone has at least one affected
    #' relative and the genetic enrichment of c2/c3 vanishes.
    downsample_relatives_diagnosed = function(relatives_diagnosed, relatives) {
      p = ifelse(relatives > 0, pmin(relatives_diagnosed / relatives, 1.0), 0.0)

      return(as.integer(rbinom(length(p), size = 1L, prob = p)))
    },
    get_tte = function(disorder1_id, disorder2_id, relkind, group_columns) {
      tte <- private$tte |>
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

      tte_d2 <- private$tte |>
        filter(disorder == disorder2_id) |>
        select(-disorder) |>
        rename(
          d2_failure_status      = failure_status,
          d2_failure_time        = failure_time,
          d2_relatives_diagnosed = relatives_diagnosed
        ) |> select(person_id, d2_failure_status, d2_failure_time, d2_relatives_diagnosed)

      d1_nrow <- tte_d1 |> nrow()
      d2_nrow <- tte_d2 |> nrow()

      if (d1_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder1_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d2_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder2_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d1_nrow != d2_nrow) {
        stop("Sample imbalance found, disorder 1 had ", d1_nrow, " individuals, disorder 2 had ", d2_nrow, " individuals")
      }

      combined <- inner_join(tte_d1, tte_d2, by = join_by(person_id))

      if (!is.list(group_columns)) return(combined)

      for (col in group_columns) {
        if (!(col %in% colnames(combined))) {
          stop("group_column \"", col, "\" was not found in TTE dataset: ", paste(colnames(combined), collapse = ", "))
        }
      }

      return(combined)
    },
    run_cif = function(tte, disorder_prefix, cohort_prefix, group_columns, earliest_onset, latest_onset) {
      status_col   <- paste0(disorder_prefix, "_failure_status")
      time_col     <- paste0(disorder_prefix, "_failure_time")

      tmp_tte <- tte |>
        rename(
          failure_status = !!as.name(status_col),
          failure_time   = !!as.name(time_col)
        ) |>
        as.data.table()

      private$analysis$cif$run(
        tte            = tmp_tte,
        group_columns  = group_columns,
        earliest_onset = earliest_onset,
        latest_onset   = latest_onset
      ) |>
        rename_with(~ paste0(cohort_prefix, "_", .), .cols = c(estimate, cases, variance, l95, u95))
    },
    run_h2 = function(disorder_prefix, re_c1, re_c2, relationship_kind, group_columns) {
      private$analysis$h2$run(
        relationship_kind = relationship_kind,
        estimates         = re_c1 |> inner_join(re_c2, by = join_by(time, !!!group_columns))
      ) |>
        rename_with(~ paste0("h2_", disorder_prefix, "_", .), .cols = c(se, l95, u95)) |>
        rename_with(~ paste0("h2_", disorder_prefix, .), .cols = c(h2))
    },
    run_draw = function(tte_c1, re_d1_c1, re_d2_c1, args) {
      tmp_tte <- copy(tte_c1)

      tmp_tte$d1_relatives_diagnosed = private$downsample_relatives_diagnosed(tmp_tte$d1_relatives_diagnosed, tmp_tte$d1_relatives)
      tmp_tte$d2_relatives_diagnosed = private$downsample_relatives_diagnosed(tmp_tte$d2_relatives_diagnosed, tmp_tte$d2_relatives)

      tte_c2 <- tmp_tte[d1_relatives_diagnosed > 0]
      if (nrow(tte_c2) == 0) rlang::abort(message = "Creating tte_c2 resulted in an empty dataset", class = "DataError")

      tte_c3 <- tmp_tte[d2_relatives_diagnosed > 0]
      if (nrow(tte_c3) == 0) rlang::abort(message = "Creating tte_c3 resulted in an empty dataset", class = "DataError")

      re_d1_c2 <- private$run_cif(tte_c2, "d1", "c2", args$group_columns, args$disorder1$earliest_onset, args$disorder1$latest_onset)
      if (is.null(re_d1_c2)) rlang::abort(message = "Running CIF on d1 c2 produced an empty result", class = "CifError")

      re_d1_c3 <- private$run_cif(tte_c3, "d1", "c3", args$group_columns, args$disorder1$earliest_onset, args$disorder1$latest_onset)
      if (is.null(re_d1_c3)) rlang::abort(message = "Running CIF on d1 c3 produced an empty result", class = "CifError")

      re_d2_c3 <- private$run_cif(tte_c3, "d2", "c3", args$group_columns, args$disorder2$earliest_onset, args$disorder2$latest_onset)
      if (is.null(re_d2_c3)) rlang::abort(message = "Running CIF on d2 c3 produced an empty result", class = "CifError")

      h2_d1 <- private$run_h2("d1", re_d1_c1, re_d1_c2, args$relationship_kind, args$group_columns)
      if (is.null(h2_d1)) rlang::abort(message = "Running h2 on re_d1_c1 and re_d1_c2 produced an empty result", class = "H2Error")

      h2_d2 <- private$run_h2(
        "d2",
        re_d2_c1,
        re_d2_c3 |> rename(c2_estimate = c3_estimate, c2_cases = c3_cases),
        args$relationship_kind,
        args$group_columns
      )
      if (is.null(h2_d2)) rlang::abort(message = "Running h2 on re_d2_c1 and re_d2_c3 produced an empty result", class = "H2Error")

      re_d1_c1 <- re_d1_c1 |> rename_with(~ paste0("re_d1_", .), .cols = starts_with("c1_"))
      re_d1_c3 <- re_d1_c3 |> rename_with(~ paste0("re_d1_", .), .cols = starts_with("c3_"))
      re_d2_c1 <- re_d2_c1 |> rename_with(~ paste0("re_d2_", .), .cols = starts_with("c1_"))

      join_columns <- list("time")

      if ("group_columns" %in% names(args) && is.list(args$group_columns)) {
        join_columns <- c(join_columns, args$group_columns)
      }

      join_symbols <- rlang::syms(join_columns)

      combined <- re_d1_c1 |>
        inner_join(re_d1_c3, by = join_by(!!!join_columns)) |>
        inner_join(re_d2_c1, by = join_by(!!!join_columns)) |>
        inner_join(h2_d1, by = join_by(!!!join_columns)) |>
        inner_join(h2_d2, by = join_by(!!!join_columns)) |>
        group_by(!!!join_symbols) |>
        arrange(desc(time)) |>
        filter(row_number() == 1) |>
        as.data.table()

      if (nrow(combined) == 0) rlang::abort(message = "Joining all Cif and H2 results produced an empty dataset", class = "DataError")

      gc <- private$analysis$gc$run(
        relationship_kind = args$relationship_kind,
        estimates         = combined
      )

      if (nrow(gc) == 0) rlang::abort(message = "Running gc on all cif and h2 results produced an empty result", class = "GcError")

      return(gc)
    }
  ),
  public = list(
    #' @description
    #' Creates an pipeline instance that stores the given time-to-event data.
    initialize = function(...) {
      validator <- ArgumentsValidator$new(
        tte = list(
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
      private$tte <- args$tte
      private$analysis <- list(
        h2  = HeritabilityAnalysis$new(),
        cif = CumulativeIncidenceAnalysis$new(),
        gc  = GeneticCorrelationAnalysis$new()
      )
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
        group_columns = list(
          type  = "list",
          items = list(type = "string")
        ),
        draws = list(
          type    = "integer",
          minimum = 1,
          default = 1
        ),
        rubin_level = list(
          type    = "string",
          enum    = list("meta", "per_year"),
          default = "meta"
        )
      )

      args   <- validator$run(...)
      tte_c1 <- private$get_tte(args$disorder1$id, args$disorder2$id, args$relationship_kind, args$group_columns)

      re_d1_c1 <- private$run_cif(tte_c1, "d1", "c1", args$group_columns, args$disorder1$earliest_onset, args$disorder1$latest_onset)
      if (is.null(re_d1_c1)) stop("Disorder 1, cohort 1 had no TTE events")

      re_d2_c1 <- private$run_cif(tte_c1, "d2", "c1", args$group_columns, args$disorder2$earliest_onset, args$disorder2$latest_onset)
      if (is.null(re_d2_c1)) stop("Disorder 2, cohort 1 had no TTE events")

      successful_draws <- list()
      failed_draws     <- list()

      for (k in seq_len(args$draws)) {
        tryCatch(
          {
            successful_draws <- append(
              successful_draws,
              private$run_draw(tte_c1, re_d1_c1, re_d2_c1, args)
            )
          },
          DataError = function(cnd) {
            print("had cif error")
            print(cnd)
          },
          CifError = function(cnd) {
            print("had cif error")
            print(cnd)
          },
          H2Error = function(cnd) {
            print("had h2 error")
            print(cnd)
          },
          GcError = function(cnd) {
            print("had gc error")
            print(cnd)
          }
        )
      }

      print("successful")
      print(successful_draws)
      print("failed")
      print(failed_draws)

      #failed_draws <- args$draws - length(successful_draws)

      #if (failed_draws == args$draws) {
      #  stop("None of the ", args$draws, " draws were successful")
      #}

      #if (failed_draws > 0) {
      #  message("Warning: ", failed_draws, " draws failed")
      #}
    }
  )
)
