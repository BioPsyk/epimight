library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

h2_analysis <- HeritabilityAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("calculate_h2", {
  it("Produces same results regardless of vectorization or not", {
    estimates <- data.table(
      id = c(1, 2, 3),
      k1 = c(0.09653048, 0.1248009, 0.1060537),
      kr = c(0.2701333, 0.3139035, 0.3562753),
      a1 = c(6, 7, 6),
      ar = c(4, 5, 4),
      rc = c(0.25, 0.25, 0.25)
    )

    results1 <- h2_analysis$calculate_h2(
      estimates$id,
      estimates$k1,
      estimates$kr,
      estimates$a1,
      estimates$ar,
      estimates$rc
    )

    results2 <- NULL

    for (i in seq_len(nrow(estimates))) {
      est      <- estimates[i, ]
      curr     <- h2_analysis$calculate_h2(est$id, est$k1, est$kr, est$a1, est$ar, est$rc)
      results2 <- rbind(results2, curr)
    }

    expect_dataframe_equal(results1, results2)
  })
})
