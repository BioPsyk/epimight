#' @title Class that takes care of running all parts of the complete pipeline.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import knitr
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
    #'
    #' @param tte TTE data
    #' @return TTE data with relatives downsampled.
    downsample_relatives = function(tte) {
      return(
        tte |>
        mutate(
          p = ifelse(relatives > 0, pmin(relatives_diagnosed / relatives, 1.0), 0.0),
          relatives_diagnosed = as.integer(rbinom(
            length(p),
            size = 1L,
            prob = p
          ))
        ) |>
        select(-p)
      )
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
              minimum = 0,
              default = 0
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
              minimum = 0,
              default = 0
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

      args <- validator$run(...)

      run_tte <- private$tte |>
        filter(
          disorder == args$disorder1$id | disorder == args$disorder2$id
        )

      sample_counts     <- run_tte |> group_by(disorder) |> summarise(count = n())
      sample_counts_fmt <- paste(kable(sample_counts, format = "simple"), collapse = "\n")

      if (length(unique(sample_counts |> pull(count))) > 1) {
        stop(paste0("Sample imbalance found:\n", sample_counts_fmt))
      } else if (sample_counts |> filter(disorder == args$disorder1$id) |> nrow() == 0) {
        stop(paste0("disorder1 (", args$disorder1$id, ") was not found in TTE data:\n", sample_counts_fmt))
      } else if (sample_counts |> filter(disorder == args$disorder2$id) |> nrow() == 0) {
        stop(paste0("disorder2 (", args$disorder2$id, ") was not found in TTE data:\n", sample_counts_fmt))
      }

      run_tte <- run_tte |> filter(relationship_kind == args$relationship_kind)

      if (run_tte |> nrow() == 0) {
        stop(paste0("relationship_kind (", args$relationship_kind, ") was not found in TTE data"))
      }

      print(run_tte)
    }
  )
)
