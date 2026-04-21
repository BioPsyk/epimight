#' @title Estimate cumulative incidence functions from competing risks data and
#' test equality across groups
#' @description
#' R6 class that performs different types of analyses.
#' @docType class
#' @import cmprsk
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import parallel
#' @import tidyr
#' @export
CumulativeIncidenceAnalysis <- R6::R6Class( #nolint
  "CumulativeIncidenceAnalysis",
  private = list(
    args = NULL,
    #' @description
    #' Dynamically builds a dplyr filter expression for the given columns.
    #'
    #' @param columns List of column names to filter.
    #' @param row Data.table with the values to use for each column.
    #' @returns Filter expression.
    build_filter_expression = function(columns, row) {
      if (is.null(ncol(row))) {
        col <- columns[[1]]

        return(
          rlang::call2("==", sym(col), row)
        )
      }

      results <- NULL

      for (col in columns) {
        eq_expr <- rlang::call2("==", sym(col), row[[col]])

        if (is.null(results)) {
          results <- eq_expr
        } else {
          results <- rlang::call2("&", results, eq_expr)
        }
      }

      return(results)
    },
    #' @description
    #' Dynamically builds a dplyr mutate expression for the given columns.
    #'
    #' @param columns List of column names to filter.
    #' @param row Data.table with the values to use for each column.
    #' @returns Filter expression.
    build_mutate_expression = function(columns, row) {
      results <- list()

      if (is.null(ncol(row))) {
        col <- columns[[1]]

        results[[col]] <- row
      } else {

        for (col in columns) {
          results[[col]] <- row[[col]]
        }
      }

      results <- rlang::call2("mutate", as.symbol("group_results"), !!!results)

      return(results)
    },
    run_weighted_single = function(tte, earliest_onset, latest_onset) {
      failure_time_max <- max(tte$failure_time)

      weights <- tte |>
        mutate(
          weight_event_1 = ifelse(failure_status == 1, weight, 0.0),
          weight_event_n = ifelse(failure_status != 0, weight, 0.0),
        ) |>
        group_by(failure_time) |>
        summarise(
          weight_all     = sum(weight),
          weight_event_1 = sum(weight_event_1),
          weight_event_n = sum(weight_event_n)
        ) |>
        ungroup() |>
        # Make sure we have a row for `failure_time` from 0 up to `failure_time_max`
        right_join(
          data.table(
            failure_time = seq(0, failure_time_max)
          ),
          by = join_by(failure_time)
        ) |>
        # The failure times that were filled in will have NA's in missing columns,
        # so we make sure to replace them with 0.0
        mutate(
          across(everything(), ~ replace_na(.x, 0.0))
        ) |>
        # Risk needs to be accumulated starting from the largest failure_time value
        arrange(desc(failure_time)) |>
        mutate(
          at_risk = cumsum(weight_all)
        ) |>
        arrange(failure_time)

      estimates <- weights |>
        filter(weight_event_n > 0.0) |>
        mutate(
          surv = ifelse(
            at_risk > 0.0,
            cumprod(1.0 - weight_event_n / at_risk),
            surv
          ),
          cif_acc = ifelse(
            at_risk > 0.0,
            cumsum(
              replace_na(lag(surv), 1.0) * weight_event_1 / at_risk
            ),
            cif_acc
          ),
          cif = replace_na(lag(cif_acc), 0.0),
        ) |>
        filter(
          failure_time   >= earliest_onset,
          failure_time   <= latest_onset,
          weight_event_1 > 0.0
        ) |>
        mutate(
          cases = cumsum(weight_event_1)
        ) |>
        rename(time = failure_time) |>
        select(time, cif, cases) |>
        mutate(
          var = 0.0,
          se  = 0.0,
          l95 = 0.0,
          u95 = 0.0
        ) |>
        as.data.table()

      return(estimates)
    },
    #' @description
    #' Runs CIF on the given TTE data as a single group.
    #'
    #' @param tte Data.table of TTE data to use.
    #' @param earliest_onset Integer with the earliest age of onset to use.
    #' @param latest_onset Integer with the latest age of onset to use.
    #' @returns Risk estimations.
    run_single = function(tte, earliest_onset, latest_onset) {
      if (!is.data.table(tte)) {
        stop("Given TTE was not a data.table")
      }

      counts <- tte[, .N, by = .(failure_status, failure_time)]

      # This check is needed because if we only have censored invidivuals
      # then cuminc will fail with an internal error.
      events_amount <- counts[failure_status != 0]

      if (nrow(events_amount) == 0) return(NULL)

      if ("weight" %in% colnames(tte)) {
        return(
          private$run_weighted_single(tte, earliest_onset, latest_onset)
        )
      }

      # We want to use the results from group 1 and for the status 1 (affected),
      # therefor we use the name `1 1` when selecting the data from the cuminc results:
      cuminc_results <- cuminc(
        ftime   = tte$failure_time,
        fstatus = tte$failure_status,
        cencode = 0
      )$`1 1`

      if (is.null(cuminc_results)) return(NULL)

      results <- data.table(
        time = cuminc_results$time,
        cif  = cuminc_results$est,
        var  = cuminc_results$var
      )[
        time >= earliest_onset & time <= latest_onset,
        head(.SD, 1),
        by = time
      ][
        ,
        .(
          time,
          cif,
          se  = sqrt(var),
          l95 = cif - qnorm(0.975) * sqrt(var),
          u95 = cif + qnorm(0.975) * sqrt(var),
          var
        )
      ]

      results <- counts[
        failure_status == 1,
        .(
          time         = failure_time,
          cases_amount = N
        )
      ][
        results,
        on = .(time)
      ][
        ,
        .(
          time, cif, se, l95, u95, var,
          cases = cumsum(
            ifelse(is.na(cases_amount), 0, cases_amount)
          )
        )
      ]

      return(results)
    }
  ),
  public = list(
    initialize = function() {
    },
    #' @description
    #' Runs CIF on the given TTE data using the provided arguments.
    #'
    #' @returns Risk estimations.
    run = function(...) {
      validator <- ArgumentsValidator$new(
        tte = list(
          type     = "data.table",
          required = TRUE,
          columns  = list(
            failure_status = list(
              type     = "integer",
              required = TRUE
            ),
            failure_time = list(
              type     = "integer",
              required = TRUE
            ),
            weight = list(
              type = "numeric"
            )
          )
        ),
        stratify_columns = list(
          type  = "list",
          items = list(type = "string")
        ),
        earliest_onset = list(
          type    = "integer",
          default = 1,
          minimum = 0
        ),
        latest_onset = list(
          type    = "integer",
          default = 100,
          minimum = 0
        )
      )

      args <- validator$run(...)

      if (!exists("stratify_columns", where = args) || length(args$stratify_columns) == 0) {
        return(
          private$run_single(
            tte            = args$tte,
            earliest_onset = args$earliest_onset,
            latest_onset   = args$latest_onset
          )
        )
      }

      stratify_symbols <- rlang::syms(args$stratify_columns)
      permutations  <- args$tte |>
        select(!!!stratify_symbols) |>
        distinct(!!!stratify_symbols) |>
        arrange(!!!stratify_symbols) |>
        as.data.frame()

      runner <- function(idx) {
        filter_expr <- private$build_filter_expression(
          args$stratify_columns,
          permutations[idx, ]
        )

        group_tte <- args$tte[rlang::eval_tidy(filter_expr)]

        if (nrow(group_tte) == 0) return(NULL)

        group_results <- private$run_single(
          tte            = group_tte,
          earliest_onset = args$earliest_onset,
          latest_onset   = args$latest_onset
        )

        if (is.null(group_results)) return(NULL)

        mutate_expr <- private$build_mutate_expression(
          args$stratify_columns, permutations[idx, ]
        )

        group_results <- rlang::eval_tidy(mutate_expr) |> collect()

        return(group_results)
      }

      indexes       <- seq_len(nrow(permutations))
      group_results <- mclapply(indexes, runner)
      results       <- rbindlist(group_results)

      return(results)
    }
  )
)
