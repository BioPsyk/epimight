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
GeneticCorrelationAnalysis <- R6::R6Class( #nolint
  "GeneticCorrelationAnalysis",
  inherit = Analysis,
  private = list(),
  public = list(
    initialize = function() {
      super$initialize()
    },
    #' @description
    #' Calulates genetic correlation (rg/rhh) along with standard error and confidence intervals.
    #'
    #' @param id Id of row, when providing stratified results.
    #' @param Kc lifetime prevalence disorder 1 in the general population.
    #' @param Krc lifetime prevalence of disorder 1 on those whose parents have CAD.
    #' @param kf lifetime prevalence disorder 2 in the general population.
    #' @param Ac number of cases used to calculate Kc.
    #' @param Arc number of cases used to calculate Krc.
    #' @param Af number of cases used to calculate Kf.
    #' @param h2_d1 heritability of disorder 1.
    #' @param h2_d2 heritability of disorder 2.
    #' @param rc Relationship coefficient.
    #' @returns Data.table with results
    calculate_rg = function(id, kc, krc, kf, ac, arc, af, h2_d1, h2_d2, rc) {
      if (is.character(rc)) {
        rc <- self$relationship_coefficient_from_kind(rc)
      }

      tc  <- qnorm(kc, lower.tail = FALSE)
      yc  <- dnorm(tc)
      trc <- qnorm(krc, lower.tail = FALSE)
      yrc <- dnorm(trc)
      tf  <- qnorm(kf, lower.tail = FALSE)
      yf  <- dnorm(tf)
      h2  <- sqrt(h2_d2 * h2_d1)
      i   <- yf / kf
      num <- tc - trc * sqrt(1 - (1 - tf / i) * (tc ^ 2 - trc ^ 2))
      den <- rc * (i + (i - tf) * trc ^ 2)
      rhh <- num / den
      rg  <- rhh / h2

      # se estimation
      wg  <- kf ^ 2 / yf ^ 2 * (1 - kf) / af
      vvg <- (1 / i - rc * rhh * (i - tf)) ^ 2 # there is a + in Wray and a - in Falconer
      wr  <- krc ^ 2 / yrc ^ 2 * (1 - krc) / arc + kc ^ 2 / yc ^ 2 * (1 - kc) / ac
      vvr <- (1 / i) ^ 2
      se  <- 1 / rc * sqrt(vvg * wg + vvr * wr)
      l95 <- rhh - 1.96 * se
      u95 <- rhh + 1.96 * se

      results <- data.table(
        id     = id,
        rhh    = rhh,
        se     = se,
        l95    = l95,
        u95    = u95,
        rg     = rg,
        h2_l95 = l95 / h2,
        h2_u95 = u95 / h2
      )

      return(results)
    },
    #' @description
    #' Calculates genetic correlation from the given risk and heritability estimates for two disorders.
    #'
    #' @returns Data.table with genetic correlations.
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
            cif_d1_c1_estimate = list(type = "numeric"),
            cif_d1_c1_cases    = list(type = "integer"),
            cif_d1_c3_estimate = list(type = "numeric"),
            cif_d1_c3_cases    = list(type = "integer"),
            cif_d2_c1_estimate = list(type = "numeric"),
            cif_d2_c1_cases    = list(type = "integer"),
            h2_d1_estimate     = list(type = "numeric"),
            h2_d2_estimate     = list(type = "numeric")
          )
        )
      )

      args      <- validator$run(...)
      estimates <- args$estimates |>
        filter_all(
          all_vars(!is.infinite(.) & !is.na(.))
        ) |>
        mutate(id = row_number())

      suppressWarnings({
        results <- self$calculate_rg(
          estimates$id,
          estimates$cif_d1_c1_estimate,
          estimates$cif_d1_c3_estimate,
          estimates$cif_d2_c1_estimate,
          estimates$cif_d1_c1_cases,
          estimates$cif_d1_c3_cases,
          estimates$cif_d2_c1_cases,
          estimates$h2_d1_estimate,
          estimates$h2_d2_estimate,
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
