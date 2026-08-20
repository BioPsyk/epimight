library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

set.seed(1)

pipeline_tte <- read_csv(
  "../data/pipeline-tte.csv",
  show_col_type = FALSE,
  col_types=cols(person_id = col_character()),
) |>
  mutate(
    weight = ifelse(relatives_trait_n > 0.0, relatives_trait_n / relatives_n, 0.0)
  ) |>
  filter(
    trait     == "SCZ",
    relatives == "parents"
  ) |>
  select(person_id, trait_status, trait_onset_time, weight, relatives_n, relatives_trait_n) |>
  as.data.table()

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("run", {
  it("it fails when an individual appears more than once", {
    expect_error(analysis$run(
     tte = pipeline_tte |> mutate(person_id = 1)
    ))
  })

  it("returns NULL when no individuals have the trait", {
    results <- analysis$run(
      tte = pipeline_tte |> mutate(trait_status = 0),
      use_weighted_cif = FALSE
    )

    expect_null(results)
  })

  it("produces different results if different cohorts and same method is used", {
    cif_c1 <- analysis$run(
     tte = pipeline_tte |> select(-weight)
   )

    cif_c2 <- analysis$run(
      tte = pipeline_tte[relatives_trait_n > 0] |> select(-weight)
    )

    expect_dataframe_not_equal(cif_c1, cif_c2)

    cif_c1 <- analysis$run(
      tte = pipeline_tte |> mutate(weight = 1.0)
    )

    cif_c2 <- analysis$run(
      tte = pipeline_tte[relatives_trait_n > 0]
    )

    cif_c2_no_filter <- analysis$run(
      tte = pipeline_tte
    )

    expect_dataframe_not_equal(cif_c1, cif_c2)
    expect_dataframe_equal(cif_c2, cif_c2_no_filter)
  })

  it("produces different results if weight is given", {
    original <- analysis$run(tte = pipeline_tte |> select(-weight))
    weighted <- analysis$run(tte = pipeline_tte)

    combined <- inner_join(original, weighted, by = join_by(time)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y)
      ) |>
      filter(
        time >= 1
      )

    diff <- combined |>
      filter(cif_diff > testthat_tolerance())

    expect_gt(nrow(combined), 0)
    expect_equal(nrow(combined), nrow(diff))
  })

  it("produces same results if weight is set to 1", {
    original <- analysis$run(tte = pipeline_tte |> select(-weight))
    weighted <- analysis$run(tte = pipeline_tte |> mutate(weight = 1.0))

    diff <- inner_join(original, weighted, by = join_by(time)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y),
      ) |>
      filter(
        cif_diff > testthat_tolerance()
      )

    expect_equal(nrow(diff), 0)
  })
})
