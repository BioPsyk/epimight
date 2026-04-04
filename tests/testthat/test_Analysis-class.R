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

describe("downsample_relatives", {
  it("Returns vector of 0/1 of correct length", {
    result <- analysis$downsample_relatives(c(3, 0, 1), c(5, 5, 5), seed = 42)
    expect_equal(length(result), 3)
    expect_true(all(result %in% c(0L, 1L)))
  })

  it("Returns 0 when n_relatives is 0", {
    result <- analysis$downsample_relatives(c(0, 0), c(0, 0), seed = 42)
    expect_equal(result, c(0L, 0L))
  })

  it("Is deterministic with the same seed", {
    r1 <- analysis$downsample_relatives(c(3, 2, 1), c(5, 5, 5), seed = 99)
    r2 <- analysis$downsample_relatives(c(3, 2, 1), c(5, 5, 5), seed = 99)
    expect_equal(r1, r2)
  })

  it("Returns 1 when all relatives are diagnosed", {
    result <- analysis$downsample_relatives(c(5), c(5), seed = 42)
    expect_equal(result, 1L)
  })

  it("Caps probability at 1 when diagnosed > n_relatives", {
    result <- analysis$downsample_relatives(c(10), c(5), seed = 42)
    expect_equal(result, 1L)
  })

  it("Fails when vector lengths differ", {
    expect_error(analysis$downsample_relatives(c(1, 2), c(3)))
  })

  it("Fails when inputs are not numeric", {
    expect_error(analysis$downsample_relatives(c("a"), c("b")))
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
