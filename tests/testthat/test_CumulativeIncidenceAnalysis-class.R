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
    weight = ifelse(relatives_n_trait > 0.0, relatives_n_trait / relatives_n, 0.0)
  ) |>
  filter(
    trait     == "SCZ",
    relatives_kind == "parents"
  ) |>
  select(person_id, trait_status, trait_age, weight, relatives_n, relatives_n_trait) |>
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
        trait_age  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_age  = c(20, 21)
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
        trait_age  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(1, 0),
        trait_age  = c(-20, -21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(-2, 99),
        trait_age  = c(20, 21)
      )
    ))

    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(1, 0),
        trait_age  = c(20, 21),
        weight       = c(-2, 3)
      )
    ))
  })

  it("it fails when an individual appears more than once", {
    expect_error(analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc1"),
        trait_status = c(1, 0),
        trait_age  = c(20, 21)
      )
    ))
  })

  it("returns NULL when no individuals have the trait", {
    results <- analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(0, 0),
        trait_age  = c(20, 21)
      )
    )

    expect_null(results)

    results <- analysis$run(
      tte = data.table(
        person_id    = c("abc1", "abc2"),
        trait_status = c(0, 0),
        trait_age  = c(20, 21)
      ),
      weight = c(0.5, 0.2)
    )

    expect_null(results)
  })

  it("produces no NA results", {
    original <- analysis$run(tte = pipeline_tte |> select(-weight)) |>
      filter(
        if_any(everything(), ~ is.na(.))
      )

    expect_equal(nrow(original), 0)

    weighted <- analysis$run(tte = pipeline_tte |> mutate(weight = 1.0)) |>
      filter(
        if_any(everything(), ~ is.na(.))
      )

    expect_equal(nrow(weighted), 0)
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

    diff <- inner_join(original, no_competing_risk, by = join_by(age)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y),
      ) |>
      filter(
        cif_diff > testthat_tolerance()
      )

    expect_gt(nrow(diff), 0)

    original <- analysis$run(
      tte = pipeline_tte |> select(-weight)
    )

    no_competing_risk <- analysis$run(
      tte = pipeline_tte |>
        select(-weight) |>
        mutate(
          trait_status = ifelse(trait_status == 1, 1, 0)
        )
    )

    diff <- inner_join(original, no_competing_risk, by = join_by(age)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y),
      ) |>
      filter(
        cif_diff > testthat_tolerance()
      )

    expect_gt(nrow(diff), 0)
  })

  it("produces different results if different cohorts and same method is used", {
    cif_c1 <- analysis$run(
     tte = pipeline_tte |> select(-weight)
   )

    cif_c2 <- analysis$run(
      tte = pipeline_tte[relatives_n_trait > 0] |> select(-weight)
    )

    expect_dataframe_not_equal(cif_c1, cif_c2)

    cif_c1 <- analysis$run(
      tte = pipeline_tte |> mutate(weight = 1.0)
    )

    cif_c2 <- analysis$run(
      tte = pipeline_tte[relatives_n_trait > 0]
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

    combined <- inner_join(original, weighted, by = join_by(age)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y)
      ) |>
      filter(
        age >= 1
      )

    diff <- combined |>
      filter(cif_diff > testthat_tolerance())

    expect_gt(nrow(combined), 0)
    expect_equal(nrow(combined), nrow(diff))
  })

  it("handles multiple competing risks as single competing risk", {
    original <- analysis$run(tte = pipeline_tte)
    multiple <- analysis$run(
      tte = pipeline_tte |>
        mutate(
          trait_status = ifelse(
            trait_status == 2,
            sample(
              seq(2, 10),
              1,
              replace = TRUE
            ),
            trait_status
          )
        )
    )

    combined <- inner_join(original, multiple, by = join_by(age)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y)
      ) |>
      filter(
        age >= 1
      )

    diff <- combined |>
      filter(cif_diff > testthat_tolerance())

    expect_equal(nrow(diff), 0)
  })

  it("produces same results as the unweighted method if all weights are set to 1", {
    original <- analysis$run(tte = pipeline_tte |> select(-weight))
    weighted <- analysis$run(tte = pipeline_tte |> mutate(weight = 1.0))

    cif_diff <- inner_join(original, weighted, by = join_by(age)) |>
      mutate(
        cif_diff = abs(cif.x - cif.y)
      ) |>
      filter(
        cif_diff > testthat_tolerance()
      )

    expect_equal(nrow(cif_diff), 0)

    se_diff <- inner_join(original, weighted, by = join_by(age)) |>
      mutate(
        se_diff  = abs(se.x - se.y)
      ) |>
      filter(
        se_diff > 3e-05
      )

    expect_equal(nrow(se_diff), 0)
  })

  it("produces cif that strongly correlates with the probablity of having the trait", {
    probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    results       <- rep(0, length(probabilities))

    for (i in 1:length(probabilities)) {
      p <- probabilities[[i]]

      tte <- tte_random_probands(1000) |>
        tte_random_trait("SCZ", p, 10, 1) |>
        select(person_id, trait_status, trait_age) |>
        as.data.table()

      cif <- analysis$run(tte = tte) |>
        arrange(desc(age)) |>
        filter(row_number() == 1) |>
        pull(cif)

      results[[i]] <- cif
    }

    test_result <- cor.test(probabilities, results)

    expect_gt(test_result$statistic, 0.99)
    expect_lt(test_result$p.value, 0.05)
  })
})
