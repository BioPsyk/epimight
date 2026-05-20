#' @title Class that represents the results of a running a single pipeline draw.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @export
AnalysisResult <- R6::R6Class( #nolint
  "AnalysisResult",
  private = list(
    start_time = NULL,
    end_time   = NULL
  ),
  public = list(
    error   = NULL,
    results = NULL,
    successful = NULL,
    initialize = function() {
      private$start_time = Sys.time()
    },
    fail = function(id, category, problem) {
      private$end_time = Sys.time()

      self$error = list(
        id       = id,
        category = category,
        problem  = problem
      )
      return(self)
    },
    success = function(results) {
      private$end_time = Sys.time()

      self$results = results
      return(self)
    },
    get_runtime_seconds = function() {
      as.numeric(
        difftime(private$end_time, private$start_time, units = "secs")
      )
    }
  )
)
