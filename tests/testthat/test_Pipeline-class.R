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

#describe("initialize", {
#  it("doesn't allow empty arguments", {
#    expect_error(Pipeline$new())
#  })
#
#  it("doesn't allow wrong tte type", {
#    expect_error(Pipeline$new(
#      tte = "hello"
#    ))
#
#    expect_error(Pipeline$new(
#      tte = 21
#    ))
#
#    expect_error(Pipeline$new(
#      tte = FALSE
#    ))
#  })
#
#  it("doesn't allow unknown relationship kinds", {
#    expect_error(Pipeline$new(
#      pool = data.table(
#        person_id           = c("p1", "p1"),
#        born_at_year        = c(1950, 1951),
#        disorder            = c("SCZ", "CAD"),
#        failure_status      = c(0, 1),
#        failure_time        = c(10, 10),
#        relationship_kind   = c(10, 20), # Wrong type
#        relatives           = c(1, 1),
#        relatives_diagnosed = c(0, 0)
#      )
#    ))
#
#    expect_error(Pipeline$new(
#      pool = data.table(
#        person_id           = c("p1", "p1"),
#        born_at_year        = c(1950, 1951),
#        disorder            = c("SCZ", "CAD"),
#        failure_status      = c(0, 1),
#        failure_time        = c(10, 10),
#        relationship_kind   = c("parent-offspring", "cousins"), # Unknown value
#        relatives           = c(1, 1),
#        relatives_diagnosed = c(0, 0)
#      )
#    ))
#  })
#
#  it("doesn't allow numeric person IDs", {
#    expect_error(Pipeline$new(
#      pool = data.table(
#        person_id           = c(1, 1),
#        born_at_year        = c(1950, 1951),
#        disorder            = c("SCZ", "CAD"),
#        failure_status      = c(0, 1),
#        failure_time        = c(10, 20),
#        relationship_kind   = c("PO", "PO"),
#        relatives           = c(2, 2),
#        relatives_diagnosed = c(0, 1)
#      )
#    ))
#  })
#
#  it("allows valid tte", {
#    Pipeline$new(
#      pool = data.table(
#        person_id           = c("p1", "p1"),
#        born_at_year        = c(1950, 1951),
#        disorder            = c("SCZ", "CAD"),
#        failure_status      = c(0, 1),
#        failure_time        = c(10, 20),
#        relationship_kind   = c("PO", "PO"),
#        relatives           = c(2, 2),
#        relatives_diagnosed = c(0, 1)
#      )
#    )
#  })
#})
#
#describe("get_tte", {
#  it("fails on unknown disorders", {
#    expect_error(
#      pipeline$get_tte("PO", "unknown", "CAD", list("born_at_year"))
#    )
#
#    expect_error(
#      pipeline$get_tte("PO", "SCZ", "unknown", list("born_at_year"))
#    )
#  })
#
#  it("fails on unknown relationship kind", {
#    expect_error(
#      pipeline$get_tte("unknown", "SCZ", "CAD", list("born_at_year"))
#    )
#  })
#
#  it("fails on invalid stratify column", {
#    expect_error(
#      pipeline$get_tte("PO", "SCZ", "CAD", list(123))
#    )
#
#    expect_error(
#      pipeline$get_tte("PO", "SCZ", "CAD", list("unknown"))
#    )
#  })
#
#  it("doesn't change the output when stratify columns are given", {
#    tte_no_strat <- pipeline$get_tte("PO", "SCZ", "CAD")
#    tte_strat    <- pipeline$get_tte("PO", "SCZ", "CAD", list("born_at_year"))
#
#    expect_equal(tte_no_strat, tte_strat)
#  })
#})
#
#describe("run_cif", {
#  it("produces right amount of rows", {
#    stratify_cols <- list("born_at_year")
#
#    tte <- pipeline$get_tte("PO", "SCZ", "CAD", stratify_cols)
#    cif <- pipeline$run_cif(tte, "d1", "c1", stratify_cols, 1, 100)
#
#    print(cif)
#  })
#})

#describe("run_h2", {
#  it("allows valid input", {
#    relkind       <- "PO"
#    stratify_cols <- list("born_at_year")
#
#    tte    <- pipeline$get_tte(relkind, "SCZ", "CAD", stratify_cols)
#    cif_c1 <- pipeline$run_cif(tte, "d1", "c1", stratify_cols, 1, 100)
#    cif_c2 <- pipeline$run_cif(tte, "d1", "c2", stratify_cols, 1, 100)
#    h2     <- pipeline$run_h2("d1", cif_c1, cif_c2, relkind, stratify_cols)
#  })
#})

describe("run", {
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

  it("fails when group column cannot be found in TTE dataset", {
    expect_error(pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO",
      draws = 2,
      stratify_columns = list("born_at_year", "unknown")
    ))
  })

  it("allows valid arguments", {
    results <- pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO",
      draws = 2,
      stratify_columns = list("born_at_year")
    )

    print(results)
  })
})

describe("get_tte exposes per-disorder relatives columns", {
  it("returns d1_relatives and d2_relatives so run_draw can downsample", {
    # Regression: previously `get_tte` left `relatives` bare in tte_d1 and
    # dropped it from tte_d2 entirely. `run_draw` then read
    # tmp_tte$d1_relatives / $d2_relatives → NULL, silently bypassing
    # downsample_relatives_diagnosed.
    tte_c1 <- pipeline$get_tte("PO", "SCZ", "CAD", list("born_at_year"))

    expect_true("d1_relatives" %in% names(tte_c1))
    expect_true("d2_relatives" %in% names(tte_c1))
  })
})

describe("downsample_relatives_diagnosed actually downsamples", {
  it("produces a mean ~ relatives_diagnosed / relatives, not 1.0", {
    # If the `relatives` argument is silently NULL (the bug), the binomial
    # collapses to as.integer(relatives_diagnosed > 0). This test guards
    # the genuine probability weighting at p = 0.25.
    set.seed(42)
    relatives           <- as.integer(rep(4L, 10))
    relatives_diagnosed <- as.integer(rep(1L, 10))

    means <- replicate(200, mean(pipeline$downsample_relatives_diagnosed(
      relatives_diagnosed, relatives
    )))
    expect_lt(abs(mean(means) - 0.25), 0.02)
    expect_lt(mean(means), 0.5)
  })

  it("returns 0 for rows with relatives == 0", {
    expect_equal(
      pipeline$downsample_relatives_diagnosed(
        relatives_diagnosed = as.integer(c(0L, 1L, 2L)),
        relatives           = as.integer(c(0L, 0L, 0L))
      ),
      as.integer(c(0L, 0L, 0L))
    )
  })
})
