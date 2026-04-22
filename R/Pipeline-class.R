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
  private = list(),
  public = list(
    #' @description
    #' Creates an pipeline instance.
    initialize = function() {
    }
  )
)
