#' @title Abstract class that contains common functionality used by all analysis classes.
#' @description
#' R6 class that performs different types of analyses.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @export
Analysis <- R6::R6Class( #nolint
  "Analysis",
  private = list(
    validator = NULL
  ),
  public = list(
    #' @description
    #' Creates an analysis instance and sets up a validator for the meta functions.
    initialize = function() {
      private$validator <- ArgumentsValidator$new(
        estimates = list(
          required = TRUE,
          type     = "data.table",
          columns  = list()
        ),
        estimate_column = list(required = TRUE, type = "string"),
        se_column       = list(required = TRUE, type = "string"),
        group_columns   = list(
          type    = "list",
          items   = list(required = TRUE, type = "string"),
          default = list()
        )
      )

      # Makes sure the meta column actually exists in the results
      # and that it has the right type.
      private$validator$add_post_validation(function(args, rules) {
        rule <- rules$estimates
        rule$columns[[args$estimate_column]] <- list(
          required = TRUE,
          type     = "numeric"
        )
        rule$columns[[args$se_column]] <- list(
          required = TRUE,
          type     = "numeric"
        )

        if (!("group_columns" %in% rules && is.list(rules$group_columns))) {
          private$validator$check_type("estimates", rule, args$estimates)

          return(args)
        }

        for (gcol in rules$group_columns) {
          rule$columns[[args$se_column]] <- list(
            required = TRUE,
            type     = "any"
          )
        }

        private$validator$check_type("estimates", rule, args$estimates)

        return(args)
      })
    },
    #' @description
    #' Helper for converting a relationship kind into a relationship coefficient.
    #'
    #' @param kind Relationship kind to convert
    #' @return Relationship coefficient
    relationship_coefficient_from_kind = function(kind) {
      return(
        epimight:::relationship_kinds[[kind]]
      )
    },
    #' @description
    #' Runs a meta analysis on the value of the given column of the given analysis results.
    #'
    #' @return Meta analysis result
    run_meta = function(...) {
      print("run_meta")
      args          <- private$validator$run(...)
      group_symbols <- rlang::syms(args$group_columns)

      args$estimates |>
        filter_all(
          all_vars(!is.infinite(.) & !is.na(.))
        ) |>
        rename(
          estimate = !!as.name(args$estimate_column),
          se       = !!as.name(args$se_column)
        ) |>
        mutate(
          fixed_se = 1 / (se ^ 2),
          rand_se  = 1 / ((se ^ 2) + var(estimate))
        ) |>
        group_by(!!!group_symbols) |>
        summarise(
          fixed_se_sum = sum(fixed_se),
          fixed_meta   = sum(estimate * fixed_se) / fixed_se_sum,
          rand_se_sum  = sum(rand_se),
          rand_meta    = sum(estimate * rand_se) / rand_se_sum
        ) |>
        mutate(
          fixed_se  = sqrt(1 / fixed_se_sum),
          fixed_l95 = fixed_meta - 1.96 * fixed_se,
          fixed_u95 = fixed_meta + 1.96 * fixed_se,
          rand_se   = sqrt(1 / rand_se_sum),
          rand_l95  = rand_meta - 1.96 * rand_se,
          rand_u95  = rand_meta + 1.96 * rand_se
        ) |>
        select(-fixed_se_sum, -rand_se_sum) |>
        relocate(rand_meta, .before = rand_se) |>
        as.data.table()
    },
    run_rubin = function(...) {
      args          <- private$validator$run(...)
      group_symbols <- rlang::syms(args$group_columns)
      k_resamples   <- nrow(args$estimates)

      args$estimates |>
        filter_all(
          all_vars(!is.infinite(.) & !is.na(.))
        ) |>
        rename(
          estimate = !!as.name(args$estimate_column), # theta
          se       = !!as.name(args$se_column)
        ) |>
        group_by(!!!group_symbols) |>
        summarise(
          fixed_meta  = mean(estimate), # theta bar
          within_var  = mean(se ^ 2),   # W
          between_var = var(estimate),  # B
        ) |>
        mutate(
          k_resamples = k_resamples,
          total_var   = within_var + (1 + 1 / k_resamples) * between_var, # T_var
          b_over_t    = between_var / total_var,
          fixed_se    = sqrt(total_var),
          fixed_l95   = fixed_meta - 1.96 * fixed_se,
          fixed_u95   = fixed_meta + 1.96 * fixed_se
        ) |>
        as.data.table()
    }
  )
)
