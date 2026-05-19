library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

set.seed(6)

d1_tte <- generate_random_tte(10000)
d1_tte <- generate_failure(d1_tte, 20, 10)
d1_tte <- generate_diagnosed_relatives(d1_tte, "relatives_diagnosed") |>
  select(-born_at, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "PO") |>
  as.data.frame()

d1_tte_dt <- data.table(d1_tte)

d2_tte <- generate_random_tte(10000)
d2_tte <- generate_failure(d2_tte, 19, 11)
d2_tte <- generate_diagnosed_relatives(d2_tte, "relatives_diagnosed") |>
  select(-born_at, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "PO") |>
  as.data.frame()

d2_tte_dt <- data.table(d2_tte)

tte_dt <- rbindlist(list(d1_tte_dt, d2_tte_dt))

pipeline <- Pipeline$new(tte = tte_dt)

#=================================================================================
# Tests
#=================================================================================

describe("initialize", {
  it("doesn't allow empty arguments", {
    expect_error(Pipeline$new())
  })

  it("doesn't allow wrong tte type", {
    expect_error(Pipeline$new(
      tte = "hello"
    ))

    expect_error(Pipeline$new(
      tte = 21
    ))

    expect_error(Pipeline$new(
      tte = FALSE
    ))
  })

  it("doesn't allow unknown relationship kinds", {
    expect_error(Pipeline$new(
      tte = data.table(
        person_id           = c("p1", "p1"),
        born_at_year        = c(1950, 1951),
        disorder            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 10),
        relationship_kind   = c(10, 20), # Wrong type
        relatives           = c(1, 1),
        relatives_diagnosed = c(0, 0)
      )
    ))

    expect_error(Pipeline$new(
      tte = data.table(
        person_id           = c("p1", "p1"),
        born_at_year        = c(1950, 1951),
        disorder            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 10),
        relationship_kind   = c("parent-offspring", "cousins"), # Unknown value
        relatives           = c(1, 1),
        relatives_diagnosed = c(0, 0)
      )
    ))
  })

  it("doesn't allow numeric person IDs", {
    expect_error(Pipeline$new(
      tte = data.table(
        person_id           = c(1, 1),
        born_at_year        = c(1950, 1951),
        disorder            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 20),
        relationship_kind   = c("PO", "PO"),
        relatives           = c(2, 2),
        relatives_diagnosed = c(0, 1)
      )
    ))
  })

  it("allows valid tte", {
    Pipeline$new(
      tte = data.table(
        person_id           = c("p1", "p1"),
        born_at_year        = c(1950, 1951),
        disorder            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 20),
        relationship_kind   = c("PO", "PO"),
        relatives           = c(2, 2),
        relatives_diagnosed = c(0, 1)
      )
    )

    Pipeline$new(tte = tte_dt)
  })
})

describe("run_experiment", {
  it("doesn't allow empty arguments", {
    expect_error(pipeline$run())
  })

  it("fails when disorder 1 is not found", {
    expect_error(pipeline$run(
      disorder1 = list(
        id             = "unknown",
        earliest_onset = 1,
        latest_onset   = 100
      ),
      disorder2 = list(
        id             = "CAD",
        earliest_onset = 0,
        latest_onset   = 100
      ),
      relationship_kind = "PO"
    ))
  })

  it("fails when disorder 2 is not found", {
    expect_error(pipeline$run(
      disorder1 = list(
        id             = "SCZ",
        earliest_onset = 1,
        latest_onset   = 100
      ),
      disorder2 = list(
        id             = "unknown",
        earliest_onset = 0,
        latest_onset   = 100
      ),
      relationship_kind = "PO"
    ))
  })

  it("fails when relationship_kind is not found", {
    expect_error(pipeline$run(
      disorder1 = list(
        id             = "SCZ",
        earliest_onset = 1,
        latest_onset   = 100
      ),
      disorder2 = list(
        id             = "CAD",
        earliest_onset = 0,
        latest_onset   = 100
      ),
      relationship_kind = "unknown"
    ))
  })

  it("allows valid experiment selection", {

    pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO"
    )
  })
})
