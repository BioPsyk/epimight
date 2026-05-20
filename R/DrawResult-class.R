#' @title Class that represents the results of a running a single pipeline draw.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @export
DrawResult <- R6::R6Class( #nolint
  "DrawResult",
  private = list(
    start_time = NULL,
    end_time   = NULL
  ),
  public = list(
    error  = NULL,
    result = NULL,
    initialize = function() {
      private$start_time <- Sys.time()
    },
    failed = function(id, category, problem) {
      private$end_time <- Sys.time()
      private$error    <- list(
        id       = id,
        category = category,
        problem  = problem
      )
    },
    successful = function(h2_d1, h2_d2, gc_d2) {
      private$end_time <- Sys.time()
      private$results  <- list(
        h2_d1    = h2_d1,
        h2_d2    = h2_d2,
        gc_d1_d2 = gc_d1_d2
      )
    },
    get_runtime_seconds = function() {
      as.numeric(
        difftime(private$end_time, private$start_time, units = "secs")
      )
    }
  )
)
