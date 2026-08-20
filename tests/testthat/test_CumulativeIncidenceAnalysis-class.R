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
    trait_name     == "SCZ",
    relatives_kind == "parents"
  ) |>
  select(person_id, trait_status, trait_onset, weight, relatives_n, relatives_trait_n) |>
  as.data.table()

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("run", {
  it("it fails when required columns are missing", {
    expect_error(analysis$run(
      tte = data.table(
        trait_status = c(1, 0),
        trait_onset  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_onset  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(1, 0)
      )
    ))
  })

  it("it fails when rows contain invalid values", {
    expect_error(analysis$run(
      tte = data.table(
        person_id    = c(1, 2),
        trait_status = c(1, 0),
        trait_onset  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(1, 0),
        trait_onset  = c(-20, -21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(-2, 99),
        trait_onset  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(1, 0),
        trait_onset  = c(20, 21),
        weight       = c(-2, 3)
      )
    ))
  })

  it("it fails when an individual appears more than once", {
    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc1"),
        trait_status = c(1, 0),
        trait_onset  = c(20, 21)
      )
    ))
  })

  it("returns NULL when no individuals have the trait", {
    results <- analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(0, 0),
        trait_onset  = c(20, 21)
      )
    )

    expect_null(results)

    results <- analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(0, 0),
        trait_onset  = c(20, 21)
      ),
      weight = c(0.5, 0.2)
    )

    expect_null(results)
  })

  it("only produces CIF values that increase as time progresses", {
    faults <- analysis$run(
      tte = pipeline_tte
    ) |> filter(cif < lag(cif))

    expect_equal(nrow(faults), 0)

    faults <- analysis$run(
      tte = pipeline_tte |> select(-weight)
    ) |> filter(cif < lag(cif))

    expect_equal(nrow(faults), 0)
  })

  it("only produces CIF values that are a probability", {
    faults <- analysis$run(
      tte = pipeline_tte
    ) |> filter(cif < 0 | cif > 1)

    expect_equal(nrow(faults), 0)

    faults <- analysis$run(
      tte = pipeline_tte |> select(-weight)
    ) |> filter(cif < 0 | cif > 1)

    expect_equal(nrow(faults), 0)
  })

  it("produces a different result if competing risk is censored", {
    original <- analysis$run(
      tte = pipeline_tte
    )

    no_competing_risk <- analysis$run(
      tte = pipeline_tte |>
        mutate(
          trait_status = ifelse(trait_status == 1, 1, 0)
        )
    )

    combined <- inner_join(original, no_competing_risk, by = join_by(time)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y),
      )

    #message("diff:")
    #print(combined)
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

  it("produces same results as the unweighted method if weight is set to 1", {
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
