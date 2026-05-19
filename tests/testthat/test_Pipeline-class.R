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
  select(-born_at_year, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(disorder = "SCZ", relationship_kind = "PO") |>
  as.data.frame()

d1_tte_dt <- data.table(d1_tte)

d2_tte <- generate_random_tte(10000)
d2_tte <- generate_failure(d2_tte, 19, 11)
d2_tte <- generate_diagnosed_relatives(d2_tte, "relatives_diagnosed") |>
  select(-born_at_year, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(disorder = "CAD", relationship_kind = "PO") |>
  as.data.frame()

d2_tte_dt <- data.table(d2_tte)

tte_dt <- rbindlist(list(d1_tte_dt, d2_tte_dt))

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

  it("doesn't allow wrong tte columns", {
    expect_error(Pipeline$new(
      tte = data.table(
        person_id = c(1, 2, 3),
      )
    ))
  })
})
