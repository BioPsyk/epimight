library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

pool_tte <- read_csv(
  "../data/pipeline-tte.csv",
  show_col_type = FALSE,
  col_types=cols(person_id = col_character()),
) |> as.data.table()

pipeline <- Pipeline$new(pool = pool_tte)

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
      pool = data.table(
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
      pool = data.table(
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
      pool = data.table(
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
      pool = data.table(
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
  })
})

describe("get_tte", {
  it("fails on unknown disorders", {
    expect_error(
      pipeline$get_tte("PO", "unknown", "CAD", list("born_at_year"))
    )

    expect_error(
      pipeline$get_tte("PO", "SCZ", "unknown", list("born_at_year"))
    )
  })

  it("fails on unknown relationship kind", {
    expect_error(
      pipeline$get_tte("unknown", "SCZ", "CAD", list("born_at_year"))
    )
  })

  it("fails on invalid stratify column", {
    expect_error(
      pipeline$get_tte("PO", "SCZ", "CAD", list(123))
    )

    expect_error(
      pipeline$get_tte("PO", "SCZ", "CAD", list("unknown"))
    )
  })

  it("doesn't change the output when stratify columns are given", {
    tte_no_strat <- pipeline$get_tte("PO", "SCZ", "CAD")
    tte_strat    <- pipeline$get_tte("PO", "SCZ", "CAD", list("born_at_year"))

    expect_equal(tte_no_strat, tte_strat)
  })
})

describe("run_cif", {
  it("allows valid input", {
    stratify_cols <- list("born_at_year")

    tte <- pipeline$get_tte("PO", "SCZ", "CAD", stratify_cols)
    cif <- pipeline$run_cif(tte, "d1", "c1", stratify_cols, 1, 100)

    print(cif)
  })
})

#describe("run_cif", {
#  it("doesn't change the output when stratify columns are given", {
#    tte_no_strat <- pipeline$get_tte("PO", "SCZ", "CAD")
#    tte_strat    <- pipeline$get_tte("PO", "SCZ", "CAD", list("born_at_year"))
#
#    expect_equal(tte_no_strat, tte_strat)
#  })
#})

#describe("run", {
#  it("doesn't allow empty arguments", {
#    expect_error(pipeline$run())
#  })
#
#  it("fails when disorder 1 is not found", {
#    expect_error(pipeline$run(
#      disorder1 = list(
#        id             = "unknown",
#        earliest_onset = 1,
#        latest_onset   = 100
#      ),
#      disorder2 = list(
#        id             = "CAD",
#        earliest_onset = 0,
#        latest_onset   = 100
#      ),
#      relationship_kind = "PO"
#    ))
#  })
#
#  it("fails when disorder 2 is not found", {
#    expect_error(pipeline$run(
#      disorder1 = list(
#        id             = "SCZ",
#        earliest_onset = 1,
#        latest_onset   = 100
#      ),
#      disorder2 = list(
#        id             = "unknown",
#        earliest_onset = 0,
#        latest_onset   = 100
#      ),
#      relationship_kind = "PO"
#    ))
#  })
#
#  it("fails when relationship_kind is not found", {
#    expect_error(pipeline$run(
#      disorder1 = list(
#        id             = "SCZ",
#        earliest_onset = 1,
#        latest_onset   = 100
#      ),
#      disorder2 = list(
#        id             = "CAD",
#        earliest_onset = 0,
#        latest_onset   = 100
#      ),
#      relationship_kind = "unknown"
#    ))
#  })
#
#  it("fails when group column cannot be found in TTE dataset", {
#    expect_error(pipeline$run(
#      disorder1 = list(
#        id = "SCZ"
#      ),
#      disorder2 = list(
#        id = "CAD"
#      ),
#      relationship_kind = "PO",
#      draws = 2,
#      stratify_columns = list("born_at_year", "unknown")
#    ))
#  })
#
#  it("allows valid experiment selection", {
#    results <- pipeline$run(
#      disorder1 = list(
#        id = "SCZ"
#      ),
#      disorder2 = list(
#        id = "CAD"
#      ),
#      relationship_kind = "PO",
#      draws = 2,
#      stratify_columns = list("born_at_year")
#    )
#
#    print(results)
#  })
#})
