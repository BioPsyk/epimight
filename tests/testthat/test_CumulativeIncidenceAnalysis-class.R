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

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Tests
#=================================================================================

describe("run", {
  it("doesn't allow empty arguments", {
    expect_error(analysis$run())
  })

  it("doesn't allow data.frame as tte", {
    expect_error(analysis$run(tte = tte))
  })

  it("handles data with only censored individuals", {
    inp <- data.table(
      born_at = c(
        as.Date("1980-01-02"),
        as.Date("1990-02-03")
      ),
      failure_status = c(0, 0),
      failure_time = c(24, 10)
    )

    results <- analysis$run(tte = inp)

    expect_equal(results, NULL)
  })

  it("Produces same results as legacy implementation", {
    old_results <- CIF_General_Population_Risk(tte, earliest_onset = 1, latest_onset = 100)
    new_results <- analysis$run(tte = tte_dt) |> rename_with(capitalize) |> as.data.frame()

    expect_dataframe_equal(old_results, new_results)
  })
})

describe("run", {
  custom_arrange <- function(df, column) {
    df <- df |>
      arrange(
        match(
          .data[[column]],
          c(
            "Any",
            "NoFamilyMembers",
            as.character(c(0:20))
          )
        )
      )

    return(df)
  }

  tte_year <- tte |>
    mutate(
      born_at_year = as.character(format(born_at, "%Y"))
    )

  tte_year_dt <- data.table(tte_year)

  it("Produces same results as CumulativeIncidence_GenPop_byYOB", {
    old_results <- CumulativeIncidence_GenPop_byYOB(tte, earliest_onset = 1, latest_onset = 100)

    all_results   <- analysis$run(tte = tte_year_dt) |> mutate(born_at_year = "all")
    group_results <- analysis$run(tte = tte_year_dt, group_columns = list("born_at_year"))
    new_results   <- union_all(all_results, group_results)
    new_summary   <- analysis$make_group_summary(tte_year_dt, new_results, "born_at_year")

    new_results <- new_results |>
      mutate(cases = as.character(cases)) |>
      rename(
        year = born_at_year,
        `N Affected Individuals` = cases
      ) |>
      rename_with(capitalize) |>
      as.data.frame()

    new_summary <- new_summary |>
      rename_with(capitalize) |>
      rename(
        Age = Time,
        all = All
      ) |>
      head(-1) |>
      as.data.frame()

    expect_dataframe_equal(old_results[[1]], new_results)
    expect_dataframe_equal(old_results[[2]], new_summary)
  })

  it("Produces same results as CumulativeIncidence_familial_withinDisorder", {
    old_results <- CumulativeIncidence_familial_withinDisorder(
      tte,
      earliest_onset = 1,
      latest_onset = 100,
      nFamMember = "a1"
    )

    group_tte <- tte_year_dt |>
      filter(diagnosed_relatives > 0) |>
      mutate(diagnosed_relatives = as.character(diagnosed_relatives)) |>
      union_all(
        tte_year_dt |>
          filter(diagnosed_relatives > 0) |>
          mutate(diagnosed_relatives = "Any")
      ) |>
      union_all(
        tte_year_dt |>
          filter(relatives == 0) |>
          mutate(diagnosed_relatives = "NoFamilyMembers")
      ) |>
      custom_arrange("diagnosed_relatives")

    group_results <- analysis$run(tte = group_tte, group_columns = list("diagnosed_relatives"))

    new_results <- group_results |>
      rename(group = "diagnosed_relatives") |>
      custom_arrange("group") |>
      relocate(group, .after = variance) |>
      rename_with(capitalize) |>
      rename(
        "N Affected Family Members" = Group,
        "N Affected Individuals"    = Cases
      ) |>
      as.data.frame()

    expect_dataframe_equal(old_results, new_results)
  })

  it("Produces same results as CumulativeIncidence_familial_withinDisorder_byYOB", {
    old_results <- CumulativeIncidence_familial_withinDisorder_byYOB(
      tte,
      earliest_onset = 1,
      latest_onset = 100,
      nFamMember = "a1"
    )

    group_columns <- list(
      "born_at_year",
      "diagnosed_relatives"
    )

    group_tte <- tte_year_dt |>
      mutate(diagnosed_relatives = as.character(diagnosed_relatives)) |>
      union_all(
        tte_year_dt |>
          filter(diagnosed_relatives > 0) |>
          mutate(diagnosed_relatives = "Any")
      ) |>
      union_all(
        tte_year_dt |>
          filter(relatives == 0) |>
          mutate(diagnosed_relatives = "NoFamilyMembers")
      )

    group_tte <- group_tte |>
      union_all(
        group_tte |>
          mutate(born_at_year = "all")
      )

    new_results <- analysis$run(tte = group_tte, group_columns = group_columns) |>
      relocate(diagnosed_relatives, .after = variance) |>
      arrange(
        match(
          diagnosed_relatives,
          c("Any", "NoFamilyMembers", as.character(c(0:20)))
        ),
        match(
          born_at_year,
          c("all", as.character(c(0:2023)))
        )
      ) |>
      rename(
        "N Affected Family Members" = diagnosed_relatives,
        "Year"                      = born_at_year,
        "N Affected Individuals"    = cases
      ) |>
      rename_with(capitalize) |>
      mutate(`N Affected Individuals` = as.character(`N Affected Individuals`)) |>
      as.data.frame()

    expect_dataframe_equal(old_results, new_results)
  })

  it("Produces same results as CumulativeIncidence_familial_betweenDisorder", {
    old_results <- CumulativeIncidence_familial_betweenDisorder(
      tte,
      earliest_onset = 1,
      latest_onset = 100,
      earliest_onset_target = 1,
      latest_onset_target = 100,
      nFamMember = "a1"
    )

    group_columns <- list("diagnosed_relatives")

    group_tte <- tte_year_dt |>
      mutate(diagnosed_relatives = as.character(diagnosed_relatives)) |>
      union_all(
        tte_year |>
          filter(diagnosed_relatives > 0) |>
          mutate(diagnosed_relatives = "Any")
      ) |>
      union_all(
        tte_year |>
          filter(relatives == 0) |>
          mutate(diagnosed_relatives = "NoFamilyMembers")
      )

    new_results <- analysis$run(tte = group_tte, group_columns = group_columns) |>
      relocate(diagnosed_relatives, .after = variance) |>
      custom_arrange("diagnosed_relatives") |>
      rename(
        "N Affected Family Members" = diagnosed_relatives,
        "N Affected Individuals"    = cases
      ) |>
      rename_with(capitalize) |>
      as.data.frame()

    expect_dataframe_equal(old_results, new_results)
  })
})
