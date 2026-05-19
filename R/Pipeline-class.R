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
    },
    prepare_tte_for_run = function(disorder1_id, disorder2_id, relkind) {
      d1_tte <- private$tte |>
        filter(
          disorder          == disorder1_id,
          relationship_kind == relkind
        ) |>
        select(-disorder)

      d2_tte <- private$tte |>
        filter(
          disorder          == disorder2_id,
          relationship_kind == relkind
        ) |>
        select(-disorder)

      d1_tte_nrow <- d1_tte |> nrow()
      d2_tte_nrow <- d2_tte |> nrow()

      if (d1_tte_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder1_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d2_tte_nrow == 0) {
        stop("No rows left after filter: disorder == \"", disorder2_id, "\" && relationship_kind == \"", relkind, "\")")
      } else if (d1_tte_nrow != d2_tte_nrow) {
        stop("Sample imbalance found, disorder 1 had ", d1_tte_nrow, " individuals, disorder 2 had ", d2_tte_nrow, " individuals")
      }

      return(list(
        d1 = d1_tte,
        d2 = d2_tte
      ))
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
              minimum = 1,
              default = 1
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
              minimum = 1,
              default = 1
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
        group_columns = list(
          type  = "list",
          items = list(type = "string")
        ),
        rubin_level = list(
          type    = "string",
          enum    = list("meta", "per_year"),
          default = "meta"
        )
      )

      args <- validator$run(...)
      tte  <- private$prepare_tte_for_run(args$disorder1$id, args$disorder2$id, args$relationship_kind)

      cif_d1_c1 <- private$analysis$cif$run(
        tte            = tte$d1,
        earliest_onset = args$disorder1$earliest_onset,
        group_columns  = args$group_columns
      )

      cif_d2_c1 <- private$analysis$cif$run(
        tte            = tte$d2,
        earliest_onset = args$disorder2$earliest_onset,
        group_columns  = args$group_columns
      )
    }
  )
)
