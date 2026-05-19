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

      # Makes sure that there are atleast 2 distinct disorders in the given time-to-event data.
      validator$add_post_validation(function(args, rules) {
        distinct_disorders <- args$tte |> distinct(disorder) |> nrow()

        if (distinct_disorders < 2) {
          stop("Given tte had less than 2 distinct disorders")
        }

        print(
          args$tte |> group_by(disorder, relationship_kind) |> summarise(count = n())
        )

        return(args)
      })

      args <- validator$run(...)
      private$tte = args$tte
    },
    #' @description
    #' Creates an pipeline instance that stores the given time-to-event data.
    run_experiment = function(...) {
      validator <- ArgumentsValidator$new(
        disorder_1 = list(
          required = TRUE,
          type     = "string"
        ),
        disorder_2 = list(
          required = TRUE,
          type     = "string"
        ),
        relationship_kind = list(
          type     = "character",
          enum     = names(epimight:::relationship_kinds),
          required = TRUE
        )
      )

      args <- validator$run(...)
    }
  )
)
