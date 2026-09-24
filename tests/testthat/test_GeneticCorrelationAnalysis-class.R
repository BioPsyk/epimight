library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

gc_analysis <- GeneticCorrelationAnalysis$new()

# Plausible bivariate inputs: trait 1 prevalence in the population and among
# relatives of trait 2 cases, trait 2 prevalence, case counts, and the two
# heritabilities.
estimates <- data.table(
  id    = c(1, 2, 3),
  kc    = c(0.05, 0.08, 0.06),
  krc   = c(0.12, 0.15, 0.11),
  kf    = c(0.08, 0.10, 0.07),
  ac    = c(500, 620, 480),
  arc   = c(60, 85, 55),
  af    = c(800, 950, 700),
  h2_t1 = c(0.60, 0.55, 0.65),
  h2_t2 = c(0.40, 0.45, 0.35)
)

calculate <- function(h2_t1 = estimates$h2_t1, h2_t2 = estimates$h2_t2) {
  gc_analysis$calculate_rg(
    estimates$id,
    estimates$kc,
    estimates$krc,
    estimates$kf,
    estimates$ac,
    estimates$arc,
    estimates$af,
    h2_t1,
    h2_t2,
    0.5
  )
}

#=================================================================================
# Tests
#=================================================================================

describe("calculate_rg", {
  it("keeps se/l95/u95 on the rhh scale", {
    results <- calculate()

    expect_equal(results$l95, results$rhh - 1.96 * results$se)
    expect_equal(results$u95, results$rhh + 1.96 * results$se)
  })

  it("emits rg_se on the rg scale, consistent with rg_l95/rg_u95", {
    results <- calculate()
    h2      <- sqrt(estimates$h2_t1 * estimates$h2_t2)

    expect_equal(results$rg_se, results$se / h2)
    expect_equal(results$rg - 1.96 * results$rg_se, results$rg_l95)
    expect_equal(results$rg + 1.96 * results$rg_se, results$rg_u95)
  })

  it("collapses the rg scale onto the rhh scale with unit heritabilities", {
    with_h2 <- calculate()
    unit_h2 <- calculate(rep(1, nrow(estimates)), rep(1, nrow(estimates)))

    expect_equal(with_h2$rhh, unit_h2$rhh)
    expect_equal(with_h2$se,  unit_h2$se)
    expect_equal(unit_h2$rg,     unit_h2$rhh)
    expect_equal(unit_h2$rg_se,  unit_h2$se)
    expect_equal(unit_h2$rg_l95, unit_h2$l95)
    expect_equal(unit_h2$rg_u95, unit_h2$u95)
  })
})
