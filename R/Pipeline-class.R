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
    tte = NULL
  ),
  public = list(
    #' @description
    #' Creates an pipeline instance.
    initialize = function() {
      validator <- ArgumentsValidator$new(
        tte = list(
          required = TRUE,
          type = "data.table",
          columns = list(
            re_d1_c1_estimates = list(type = "numeric"),
            re_d1_c1_cases     = list(type = "integer"),
            re_d1_c3_estimates = list(type = "numeric"),
            re_d1_c3_cases     = list(type = "integer"),
            re_d2_c1_estimates = list(type = "numeric"),
            re_d2_c1_cases     = list(type = "integer"),
            h2_d1              = list(type = "numeric"),
            h2_d2              = list(type = "numeric")
          )
        )
      )

      args <- validator$run(...)
    }
  )
)
