library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

set.seed(1)

tte <- read_csv(
  "../data/pipeline-tte.csv",
  show_col_type = FALSE,
  col_types=cols(person_id = col_character()),
) |>
  mutate(
    weight = ifelse(relatives_diagnosed > 0.0, relatives_diagnosed / relatives, 0.0)
  ) |>
  filter(
    disorder          == "SCZ",
    relationship_kind == "PO"
  ) |>
  select(person_id, failure_status, failure_time, weight, relatives, relatives_diagnosed) |>
  as.data.table()

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("run", {
  it("produces different results if different cohorts and same method is used", {
    cif_c1 <- analysis$run(
      tte              = tte |> select(-weight),
      earliest_onset   = 0,
      latest_onset     = 100
    )

    cif_c2 <- analysis$run(
      tte              = tte[relatives_diagnosed > 0] |> select(-weight),
      earliest_onset   = 0,
      latest_onset     = 100
    )

    expect_dataframe_not_equal(cif_c1, cif_c2)

    cif_c1 <- analysis$run(
      tte              = tte |> mutate(weight = 1.0),
      earliest_onset   = 0,
      latest_onset     = 100
    )

    cif_c2 <- analysis$run(
      tte              = tte[relatives_diagnosed > 0],
      earliest_onset   = 0,
      latest_onset     = 100
    )

    cif_c2_no_filter <- analysis$run(
      tte              = tte,
      earliest_onset   = 0,
      latest_onset     = 100
    )

    expect_dataframe_not_equal(cif_c1, cif_c2)
    expect_dataframe_equal(cif_c2, cif_c2_no_filter)
  })

  it("produces different results if weight is given", {
    original <- analysis$run(
      tte              = tte |> select(-weight),
      earliest_onset   = 1,
      latest_onset     = 100
    ) |>
      rename(
        cif_original   = cif,
        cases_original = cases
      )

    weighted <- analysis$run(
      tte              = tte,
      earliest_onset   = 1,
      latest_onset     = 100
    ) |>
      rename(
        cif_weighted   = cif,
        cases_weighted = cases
      )

    combined <- inner_join(original, weighted, by = join_by(time)) |>
      mutate(
        cif_diff   = abs(cif_original - cif_weighted),
        cases_diff = abs(cases_original - cases_weighted)
      )

    combined_diff <- combined |> filter(cif_diff > testthat_tolerance())

    expect_true(nrow(combined) > 0)
    expect_equal(nrow(combined), nrow(combined_diff))
  })

  it("produces same results if weight is set to 1", {
    original <- analysis$run(
      tte              = tte |> select(-weight),
      earliest_onset   = 1,
      latest_onset     = 100
    ) |>
      rename(
        cif_original   = cif,
        cases_original = cases
      )

    weighted <- analysis$run(
      tte              = tte |> mutate(weight = 1.0),
      earliest_onset   = 1,
      latest_onset     = 100
    ) |>
      rename(
        cif_weighted   = cif,
        cases_weighted = cases
      )

    combined <- inner_join(original, weighted, by = join_by(time)) |>
      mutate(
        cif_diff   = abs(cif_original - cif_weighted),
        cases_diff = abs(cases_original - cases_weighted)
      ) |>
      filter(cif_diff > testthat_tolerance())

    expect_equal(nrow(combined), 0)
  })
})
