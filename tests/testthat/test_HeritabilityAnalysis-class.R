library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../legacy.R")
source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

set.seed(6)

tte <- generate_random_tte(10000)
tte <- generate_failure(tte, 20, 10)
tte <- generate_diagnosed_relatives(tte, "diagnosed_relatives") |>
  select(-born_at_year, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(diagnosed_relatives, .after = relatives) |>
  as.data.frame()

tte_dt <- data.table(tte)

relationship_kind <- "PO"
h2_analysis       <- HeritabilityAnalysis$new()
cif_analysis      <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("relationship_coefficient_from_kind", {
  it("Produces correct coefficients for common kinds", {
    expect_equal(h2_analysis$relationship_coefficient_from_kind("PO"), 0.5)
    expect_equal(h2_analysis$relationship_coefficient_from_kind("FS"), 0.5)
    expect_equal(h2_analysis$relationship_coefficient_from_kind("HS"), 0.25)
  })
})

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
      est      <- estimates[i,]
      curr     <- h2_analysis$calculate_h2(est$id, est$k1, est$kr, est$a1, est$ar, est$rc)
      results2 <- rbind(results2, curr)
    }

    expect_dataframe_equal(results1, results2)
  })
})

describe("run", {
  it("Produces heritability estimates larger than 0.5 for weighted relatives", {
    tte2 <- tte_dt |>
      mutate(cohort = "all") |>
      union_all(
        tte_dt |>
        filter(diagnosed_relatives > 0) |>
        mutate(cohort = "affected_relatives")
      )

    estimates <- cif_analysis$run(
      tte = tte2,
      group_columns = list("cohort")
    )

    cohort1 <- estimates |> filter(cohort == "all")
    cohort2 <- estimates |> filter(cohort == "affected_relatives")

    combined <- cohort1 |>
      inner_join(cohort2, by = join_by(time)) |>
      rename(
        c1_estimate = estimate.x,
        c1_cases    = cases.x,
        c2_estimate = estimate.y,
        c2_cases    = cases.y
      ) |>
      select(time, c1_estimate, c1_cases, c2_estimate, c2_cases) |>
      arrange(desc(time)) |>
      filter(row_number() == 1) |>
      as.data.table()

    results <- h2_analysis$run(
      relationship_kind = relationship_kind,
      estimates         = combined
    )

    expect_gt(results[[1]], 0.5)
  })

  it("Produces heritability stratified by birth year", {
    tte2 <- tte_dt |>
      mutate(
        born_at_year = as.character(format(born_at, "%Y"))
      )

    tte2 <- tte2 |>
      mutate(cohort = "all") |>
      union_all(
        tte2 |>
        filter(diagnosed_relatives > 0) |>
        mutate(cohort = "affected_relatives")
      )

    estimates <- cif_analysis$run(
      tte = tte2,
      group_columns = list("born_at_year", "cohort")
    )

    #=================================================================================
    # Variant 1
    #=================================================================================

    cohort1 <- estimates |> filter(cohort == "all")
    cohort2 <- estimates |> filter(cohort == "affected_relatives")

    combined <- cohort1 |>
      inner_join(cohort2, by = join_by(time, born_at_year)) |>
      rename(
        c1_estimate = estimate.x,
        c1_cases    = cases.x,
        c2_estimate = estimate.y,
        c2_cases    = cases.y
      ) |>
      select(time, born_at_year, c1_estimate, c1_cases, c2_estimate, c2_cases) |>
      group_by(born_at_year) |>
      arrange(desc(time)) |>
      filter(row_number() == 1) |>
      as.data.table()

    results <- h2_analysis$run(
      relationship_kind = relationship_kind,
      estimates         = combined
    )

    validator <- ArgumentsValidator$new(
      meta = list(
        required = TRUE,
        type     = "data.table",
        columns  = list(
          fixed_meta = list(required = TRUE, type = "numeric"),
          fixed_se   = list(required = TRUE, type = "numeric"),
          fixed_l95  = list(required = TRUE, type = "numeric"),
          fixed_u95  = list(required = TRUE, type = "numeric"),
          rand_meta  = list(required = TRUE, type = "numeric"),
          rand_se    = list(required = TRUE, type = "numeric"),
          rand_l95   = list(required = TRUE, type = "numeric"),
          rand_u95   = list(required = TRUE, type = "numeric")
        )
      )
    )

    validator$run(meta = h2_analysis$run_meta(results))
  })

  it("Produces same results as dk_h2byYOB", {
    #=================================================================================
    # Old results
    #=================================================================================

    old_results <- dk_h2byYOB("MDD", tte, 1, 100, "a1", "PO", "fixed") |>
      as.data.table() |>
      select(
        Year, h2, Variance, L95, U95
      ) |>
      rename(
        born_at_year = Year,
        se           = Variance,
        l95          = L95,
        u95          = U95
      ) |>
      filter_all(
        all_vars(!is.infinite(.) & !is.na(.))
      )

    #=================================================================================
    # New results
    #=================================================================================

    tte2 <- tte_dt |>
      mutate(
        born_at_year = as.character(format(born_at, "%Y"))
      )

    tte2 <- tte2 |>
      mutate(cohort = "all") |>
      union_all(
        tte2 |>
        filter(diagnosed_relatives > 0) |>
        mutate(cohort = "affected_relatives")
      )

    estimates <- cif_analysis$run(
      tte = tte2,
      group_columns = list("born_at_year", "cohort")
    )

    cohort1 <- estimates |> filter(cohort == "all")
    cohort2 <- estimates |> filter(cohort == "affected_relatives")

    combined <- cohort1 |>
      inner_join(cohort2, by = join_by(time, born_at_year)) |>
      rename(
        c1_estimate = estimate.x,
        c1_cases    = cases.x,
        c2_estimate = estimate.y,
        c2_cases    = cases.y
      ) |>
      select(time, born_at_year, c1_estimate, c1_cases, c2_estimate, c2_cases) |>
      group_by(born_at_year) |>
      arrange(desc(time)) |>
      filter(row_number() == 1) |>
      as.data.table()

    new_results <- h2_analysis$run(
      relationship_kind = relationship_kind,
      estimates         = combined
    ) |> select(born_at_year, h2, se, l95, u95)

    new_meta <- h2_analysis$run_meta(new_results) |>
      select(starts_with("fixed_")) |>
      mutate(born_at_year = "Meta_fixed") |>
      rename(
        h2  = fixed_meta,
        se  = fixed_se,
        l95 = fixed_l95,
        u95 = fixed_u95
      )

    new_results <- new_results |> rbind(new_meta) |> arrange(desc(born_at_year))
    old_results <- old_results |> arrange(desc(born_at_year))

    expect_dataframe_equal(old_results, new_results)
  })
})
