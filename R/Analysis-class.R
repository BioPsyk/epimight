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
Analysis <- R6::R6Class(
  "Analysis",
  private = list(),
  public = list(
    initialize = function() {
    },
    #' @description
    #' Helper for converting a relationship kind into a relationship coefficient.
    #'
    #' @param kind Relationship kind to convert
    #' @return Relationship coefficient
    relationship_coefficient_from_kind = function(kind) {
      return(
        IbpRiskEstimations:::relationship_kinds[[kind]]
      )
    },
    #' @description
    #' Runs a meta analysis on the value of the given column of the given analysis results.
    #'
    #' @param analysis_results Data table with the analysis result to meta analyse.
    #' @param meta_column Name of column to run the meta analysis on.
    #' @return Meta analysis result
    run_meta = function(...) {
      validator <- ArgumentsValidator$new(
        results = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            se = list(required = TRUE, type = "numeric")
          )
        ),
        meta_column = list(required = TRUE, type = "string")
      )

      # Makes sure the meta column actually exists in the results
      # and that it has the right type.
      validator$add_post_validation(function(args, rules) {
        rule <- rules$results
        rule$columns[[args$meta_column]] <- list(
          required = TRUE,
          type     = "numeric"
        )

        validator$check_type("results", rule, args$results)
      })

      args <- validator$run(...)

      meta_results <- args$results |>
        filter_all(
          all_vars(!is.infinite(.) & !is.na(.))
        ) |>
        rename(
          meta = !!as.name(args$meta_column)
        ) |>
        mutate(
          fixed_se = 1 / (se ^ 2),
          rand_se  = 1 / ((se ^ 2) + var(meta))
        ) |>
        summarise(
          fixed_se_sum = sum(fixed_se),
          fixed_meta   = sum(meta * fixed_se) / fixed_se_sum,
          rand_se_sum  = sum(rand_se),
          rand_meta    = sum(meta * rand_se) / rand_se_sum
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

      return(meta_results)
    }
  )
)
