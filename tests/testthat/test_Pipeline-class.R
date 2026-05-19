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
d1_tte <- generate_diagnosed_relatives(d1_tte, "diagnosed_relatives") |>
  select(-born_at_year, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(diagnosed_relatives, .after = relatives) |>
  as.data.frame()

d1_tte_dt <- data.table(d1_tte)

d2_tte <- generate_random_tte(10000)
d2_tte <- generate_failure(d2_tte, 20, 10)
d2_tte <- generate_diagnosed_relatives(d2_tte, "diagnosed_relatives") |>
  select(-born_at_year, -dead_at_year) |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(diagnosed_relatives, .after = relatives) |>
  as.data.frame()

d2_tte_dt <- data.table(d2_tte)

print(d2_tte_dt)

#=================================================================================
# Tests
#=================================================================================

describe("initialize", {
  it("doesn't allow empty arguments", {
    expect_error(Pipeline$new())
  })
})
