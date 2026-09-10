library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Tests
#=================================================================================

describe("expect_dataframe(_not)_equal", {
  it("passes on data.table/data.frame with same rows and order", {
    a = data.table(
      id   = c(1, 3, 10),
      name = c("tester", "admin", "dev")
    )

    b = data.table(
      id   = c(1, 3, 10),
      name = c("tester", "admin", "dev")
    )

    expect_dataframe_equal(a, b)
    expect_failure(expect_dataframe_not_equal(a, b))

    a = a |> as.data.frame()
    b = b |> as.data.frame()

    expect_dataframe_equal(a, b)
    expect_failure(expect_dataframe_not_equal(a, b))
  })

  it("fails on data.tables with same rows and different order", {
    a = data.table(
      id   = c(1, 10, 3),
      name = c("tester", "dev", "admin")
    )

    b = data.table(
      id   = c(1, 3, 10),
      name = c("tester", "admin", "dev")
    )

    expect_failure(expect_dataframe_equal(a, b))
    expect_dataframe_not_equal(a, b)

    a = a |> as.data.frame()
    b = b |> as.data.frame()

    expect_failure(expect_dataframe_equal(a, b))
    expect_dataframe_not_equal(a, b)
  })

  it("ignores columns that are marked to be ingored", {
    a = data.table(
      id   = c(1, 3, 10),
      name = c("tester", "scientist", "dev")
    )

    b = data.table(
      id   = c(1, 3, 10),
      name = c("tester", "admin", "dev")
    )

    expect_dataframe_equal(a, b, c("name"))
    expect_failure(expect_dataframe_not_equal(a, b, c("name")))

    a = a |> as.data.frame()
    b = b |> as.data.frame()

    expect_dataframe_equal(a, b, c("name"))
    expect_failure(expect_dataframe_not_equal(a, b, c("name")))
  })
})
