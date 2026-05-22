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

      # We want to use the results from group 1 and for the status 1 (affected),
      # therefor we use the name `1 1` when selecting the data from the cuminc results:
      cuminc_results <- cuminc(
        ftime   = tte$failure_time,
        fstatus = tte$failure_status,
        cencode = 0
      )$`1 1`

      if (is.null(cuminc_results)) return(NULL)

      results <- data.table(
        time     = cuminc_results$time,
        estimate = cuminc_results$est,
        variance = cuminc_results$var
      )[
        time >= earliest_onset & time <= latest_onset,
        head(.SD, 1),
        by = time
      ][
        ,
        .(
          time, variance, estimate,
          l95 = estimate - qnorm(0.975) * sqrt(variance),
          u95 = estimate + qnorm(0.975) * sqrt(variance)
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
          time, estimate, variance, l95, u95,
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
          minimum = 1
        ),
        latest_onset = list(
          type    = "integer",
          default = 100,
          minimum = 1
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

      group_symbols <- rlang::syms(args$stratify_columns)
      permutations  <- args$tte |>
        select(!!!group_symbols) |>
        distinct(!!!group_symbols) |>
        arrange(!!!group_symbols) |>
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
    },
    #' @description
    #' Retrieves all distinct group values of the provided column of the provided TTE data.
    #'
    #' @param tte TTE data to use.
    #' @param group_column Column to retrieve values from.
    #' @returns List of values.
    get_group_values = function(tte, group_column) {
      groups <- tte |>
        rename(group = !!as.name(group_column)) |>
        arrange(group) |>
        distinct(group) |>
        mutate(group = as.character(group)) |>
        pull(group)

      return(groups)
    },
    #' @description
    #' Summarizes the estimates for each group.
    #'
    #' @param tte TTE data to use.
    #' @param cif_results Results that the "run" function produced.
    #' @param group_column Column to group the summary on.
    #' @returns Data.table with summary.
    make_group_summary = function(tte, cif_results, group_column) {
      summary <- data.table(
        group = c("all", self$get_group_values(tte, group_column))
      ) |>
        left_join(
          cif_results |> select(!!as.name(group_column), time, estimate),
          by = c("group" = group_column)
        ) |>
        pivot_wider(
          names_from  = group,
          values_from = estimate
        ) |>
        arrange(time)

      return(summary)
    }
  )
)
