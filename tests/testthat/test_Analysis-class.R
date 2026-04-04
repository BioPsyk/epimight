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

describe("combine_rubin", {
  it("Produces correct hand-calculated values", {
    # K=3 estimates: 0.5, 0.4, 0.6 with SEs 0.1, 0.1, 0.1
    est <- data.table(fixed_meta = c(0.5, 0.4, 0.6), fixed_se = c(0.1, 0.1, 0.1))
    result <- analysis$combine_rubin(est)

    expect_equal(result$fixed_meta, 0.5)  # mean
    expect_equal(result$within_var, 0.01) # mean(0.01, 0.01, 0.01)
    expect_equal(result$between_var, var(c(0.5, 0.4, 0.6)))  # 0.01
    # T = 0.01 + (1 + 1/3) * 0.01 = 0.01 + 0.01333 = 0.02333
    expect_equal(result$total_var, 0.01 + (1 + 1/3) * 0.01, tolerance = 1e-10)
    expect_equal(result$k_resamples, 3)
  })

  it("Returns B=0 when all estimates are identical", {
    est <- data.table(fixed_meta = c(0.5, 0.5, 0.5), fixed_se = c(0.1, 0.2, 0.15))
    result <- analysis$combine_rubin(est)

    expect_equal(result$fixed_meta, 0.5)
    expect_equal(result$between_var, 0)
    expect_equal(result$b_over_t, 0)
  })

  it("Produces large B/T when estimates vary widely", {
    # With K=2, max B/T = K/(K+1) = 2/3 ≈ 0.667 due to (1+1/K) factor
    est <- data.table(fixed_meta = c(0.1, 0.9), fixed_se = c(0.001, 0.001))
    result <- analysis$combine_rubin(est)

    expect_true(result$b_over_t > 0.6)
  })

  it("Has all expected output columns", {
    est <- data.table(fixed_meta = c(0.5, 0.4), fixed_se = c(0.1, 0.1))
    result <- analysis$combine_rubin(est)

    expected_cols <- c("fixed_meta", "fixed_se", "fixed_l95", "fixed_u95",
                       "within_var", "between_var", "total_var", "b_over_t",
                       "k_resamples")
    expect_true(all(expected_cols %in% colnames(result)))
    expect_equal(nrow(result), 1)
  })

  it("Respects custom column names", {
    est <- data.table(h2 = c(0.5, 0.6), se = c(0.1, 0.1))
    result <- analysis$combine_rubin(est, estimate_column = "h2", se_column = "se")
    expect_equal(result$fixed_meta, 0.55)
  })

  it("Fails with fewer than 2 rows", {
    est <- data.table(fixed_meta = 0.5, fixed_se = 0.1)
    expect_error(analysis$combine_rubin(est))
  })

  it("Fails when columns are missing", {
    est <- data.table(x = c(0.5, 0.4), y = c(0.1, 0.1))
    expect_error(analysis$combine_rubin(est))
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
