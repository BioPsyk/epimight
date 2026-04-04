library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Helpers
#=================================================================================

make_synthetic_c1_tte <- function(n = 5000, seed = 42) {
  set.seed(seed)
  born_years <- sample(1960:1964, n, replace = TRUE)
  data.table(
    person_id              = seq_len(n),
    born_at_year           = born_years,
    d1_failure_status      = as.integer(rbinom(n, 1, 0.10)),
    d1_failure_time        = as.integer(pmax(1, rpois(n, 30))),
    d1_diagnosed_relatives = as.integer(rbinom(n, 3, 0.10)),
    d1_n_relatives         = as.integer(sample(1:4, n, replace = TRUE)),
    d2_failure_status      = as.integer(rbinom(n, 1, 0.10)),
    d2_failure_time        = as.integer(pmax(1, rpois(n, 30))),
    d2_diagnosed_relatives = as.integer(rbinom(n, 3, 0.10)),
    d2_n_relatives         = as.integer(sample(1:4, n, replace = TRUE))
  )
}

#=================================================================================
# Tests
#=================================================================================

describe("MultipleImputationAnalysis", {

  it("Fails when c1_tte is missing required columns", {
    bad_tte <- data.table(person_id = 1:10, born_at_year = 1960L)
    expect_error(
      MultipleImputationAnalysis$new(bad_tte, "FS", K = 2L),
      "missing required columns"
    )
  })

  it("Fails when relationship_kind is not a string", {
    tte <- make_synthetic_c1_tte(100)
    expect_error(MultipleImputationAnalysis$new(tte, 123, K = 2L))
  })

  it("Returns correct structure with K=2", {
    tte <- make_synthetic_c1_tte(5000)
    mi <- MultipleImputationAnalysis$new(tte, "FS", K = 2L, seed = 42L)
    results <- mi$run()

    # Top-level structure
    expect_true(is.list(results))
    expect_true(all(c("h2_d1", "h2_d2", "gc") %in% names(results)))

    # Each element has rubin_meta and resample_meta
    for (nm in c("h2_d1", "h2_d2", "gc")) {
      expect_true("rubin_meta" %in% names(results[[nm]]))
      expect_true("resample_meta" %in% names(results[[nm]]))
      expect_equal(nrow(results[[nm]]$rubin_meta), 1)
      expect_equal(nrow(results[[nm]]$resample_meta), 2)
    }

    # Rubin meta has expected columns
    rubin_cols <- c("fixed_meta", "fixed_se", "fixed_l95", "fixed_u95",
                    "within_var", "between_var", "total_var", "b_over_t",
                    "k_resamples")
    expect_true(all(rubin_cols %in% colnames(results$h2_d1$rubin_meta)))
    expect_equal(results$h2_d1$rubin_meta$k_resamples, 2)
  })

  it("Returns K=1 with NA diagnostics", {
    tte <- make_synthetic_c1_tte(5000)
    mi <- MultipleImputationAnalysis$new(tte, "FS", K = 1L, seed = 42L)
    results <- mi$run()

    expect_equal(nrow(results$h2_d1$resample_meta), 1)
    expect_equal(results$h2_d1$rubin_meta$k_resamples, 1L)
    expect_true(is.na(results$h2_d1$rubin_meta$b_over_t))
    expect_true(is.na(results$h2_d1$rubin_meta$within_var))
  })

  it("Is deterministic with the same seed", {
    tte <- make_synthetic_c1_tte(5000)
    mi1 <- MultipleImputationAnalysis$new(tte, "FS", K = 2L, seed = 99L)
    mi2 <- MultipleImputationAnalysis$new(tte, "FS", K = 2L, seed = 99L)
    r1 <- mi1$run()
    r2 <- mi2$run()

    expect_equal(r1$h2_d1$rubin_meta$fixed_meta,
                 r2$h2_d1$rubin_meta$fixed_meta)
  })

  it("Different seeds produce different results", {
    tte <- make_synthetic_c1_tte(5000)
    mi1 <- MultipleImputationAnalysis$new(tte, "FS", K = 2L, seed = 1L)
    mi2 <- MultipleImputationAnalysis$new(tte, "FS", K = 2L, seed = 100L)
    r1 <- mi1$run()
    r2 <- mi2$run()

    expect_false(isTRUE(all.equal(
      r1$h2_d1$rubin_meta$fixed_meta,
      r2$h2_d1$rubin_meta$fixed_meta
    )))
  })
})
