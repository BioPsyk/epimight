#' @title Estimate heritability from risk estimates.
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
HeritabilityAnalysis <- R6::R6Class( #nolint
  "HeritabilityAnalysis",
  inherit = Analysis,
  private = list(),
  public = list(
    initialize = function() {
      super$initialize()
    },
    #' @description
    #' Calulates heritability (h2) along with standard error and confidence intervals.
    #'
    #' @param id Id of row, when providing stratified results.
    #' @param k1 lifetime prevalence general population
    #' @param kr lifetime prevalence in the relatives of the affected ones
    #' @param a1 number of cases used to calculate K1
    #' @param ar number of cases used to calculate Kr
    #' @param rc relationship coefficient.
    #' @returns Data.table with results
    calculate_h2 = function(id, k1, kr, a1, ar, rc) {
      if (is.character(rc)) {
        rc <- self$relationship_coefficient_from_kind(rc)
      }

      # lifetime prevalence unaffected/general population represeting the upper tail z value
      t1   <- qnorm(k1, lower.tail = FALSE)
      y    <- dnorm(t1)
      i    <- y / k1
      # lifetime prevalence in the relatives of the affected ones represeting the upper tail z value
      tr   <- qnorm(kr, lower.tail = FALSE)
      yr   <- dnorm(tr)

      num  <- t1 - tr * sqrt(1 - (1 - t1 / i) * (t1 ^ 2 - tr ^ 2))
      den  <- rc * (i + (i - t1) * tr ^ 2)
      h2   <- num / den
      # se estimation
      wg   <- (((k1 ^ 2) / (y ^ 2)) * (1 - k1)) / a1
      vvg  <- (1 / i - rc * h2 * (i - t1)) ^ 2 # there is a + in Wray and a - in Falconer
      wr   <- kr ^ 2 / yr ^ 2 * (1 - kr) / ar
      vvr  <- (1 / i) ^ 2

      se   <- 1 / rc * sqrt(vvg * wg + vvr * wr)
      l95 <- h2 - 1.96 * se
      u95 <- h2 + 1.96 * se

      output <- data.table(
        id       = id,
        estimate = h2,
        se       = se,
        l95      = l95,
        u95      = u95
      )

      return(output)
    },
    #' @description
    #' Calculates heritability from the given risk estimates.
    #'
    #' @returns Data.table with heritability.
    run = function(...) {
      validator <- ArgumentsValidator$new(
        relationship_kind = list(
          required = TRUE,
          type = "string",
          enum = names(epimight:::relationship_kinds)
        ),
        estimates = list(
          required = TRUE,
          type = "data.table",
          columns = list(
            c1_estimate = list(
              required = TRUE,
              type     = "numeric"
            ),
            c1_cases = list(
              required = TRUE,
              type     = "integer",
              minimum  = 0
            ),
            c2_estimate = list(
              required = TRUE,
              type     = "numeric"
            ),
            c2_cases = list(
              required = TRUE,
              type     = "integer",
              minimum  = 0
            )
          )
        )
      )

      args <- validator$run(...)

      estimates <- args$estimates |> mutate(id = row_number())

      suppressWarnings({
        results <- self$calculate_h2(
          estimates$id,
          estimates$c1_estimate,
          estimates$c2_estimate,
          estimates$c1_cases,
          estimates$c2_cases,
          args$relationship_kind
        ) |>
          filter_all(
            all_vars(!is.infinite(.) & !is.na(.))
          )
      })

      results <- estimates |>
        inner_join(results, by = join_by(id)) |>
        select(-id)

      return(results)
    },
    run_meta = function(...) {
      super$run_meta(...)
    }
  )
)
