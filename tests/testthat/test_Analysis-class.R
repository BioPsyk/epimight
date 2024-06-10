library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

analysis <- Analysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("relationship_coefficient_from_kind", {
  it("Produces correct coefficients for common kinds", {
    expect_equal(analysis$relationship_coefficient_from_kind("PO"), 0.5)
    expect_equal(analysis$relationship_coefficient_from_kind("FS"), 0.5)
    expect_equal(analysis$relationship_coefficient_from_kind("HS"), 0.25)
  })
})

describe("run_meta", {
  it("Fails when no arguments are given", {
    expect_error(analysis$run_meta())
  })

  it("Fails when incorrect types are given", {
    expect_error(
      analysis$run_meta(
        results     = "no data.table?",
        meta_column = "h2"
      )
    )

    expect_error(
      analysis$run_meta(
        results = data.table(
          se = c(0.2, 0.3),
          h2 = c(0.5, 0.4)
        ),
        meta_column = 2
      )
    )

    expect_error(
      analysis$run_meta(
        results = data.table(
          se = c(0.2, 0.3),
          h2 = c("data", "table")
        ),
        meta_column = "h2"
      )
    )

    expect_error(
      analysis$run_meta(
        results = data.table(
          se = c(TRUE, FALSE),
          h2 = c(0.5, 0.4)
        ),
        meta_column = "h2"
      )
    )
  })

  it("Fails when meta column is missing", {
    expect_error(
      analysis$run_meta(
        results = data.table(
          se = c(0.2, 0.3),
          h2 = c(0.5, 0.4)
        ),
        meta_column = "rhh"
      )
    )
  })
})
