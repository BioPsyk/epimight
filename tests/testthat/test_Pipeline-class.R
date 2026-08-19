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
      pool = "hello"
    ))

    expect_error(Pipeline$new(
      pool = 21
    ))

    expect_error(Pipeline$new(
      pool = FALSE
    ))
  })

  it("doesn't allow wrong typed relative kinds", {
    expect_error(Pipeline$new(
      pool = data.table(
        person_id           = c("p1", "p1"),
        born_at_year        = c(1950, 1951),
        trait            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 10),
        relatives           = c(1, 1),
        relatives_diagnosed = c(0, 0),
        relatives_kind      = c(10, 20) # Wrong type
      )
    ))
  })

  it("doesn't allow numeric person IDs", {
    expect_error(Pipeline$new(
      pool = data.table(
        person_id           = c(1, 1),
        born_at_year        = c(1950, 1951),
        trait            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 20),
        relatives           = c(2, 2),
        relatives_diagnosed = c(0, 1),
        relatives_kind      = c("parents", "parents")
      )
    ))
  })

  it("allows valid tte", {
    Pipeline$new(
      pool = data.table(
        person_id           = c("p1", "p1"),
        born_at_year        = c(1950, 1951),
        trait            = c("SCZ", "CAD"),
        failure_status      = c(0, 1),
        failure_time        = c(10, 20),
        relatives           = c(2, 2),
        relatives_diagnosed = c(0, 1),
        relatives_kind      = c("parents", "parents")
      )
    )
  })
})

describe("get_tte", {
  it("fails on unknown stratify_columns", {
    expect_error(pipeline$get_tte(
      list(id = "SCZ"),
      NA,
      list("unknown")
    ))
  })

  it("fails on unknown relatives trait", {
    expect_error(pipeline$get_tte(
      list(id = "SCZ"),
      list(
        id             = "unknown",
        relatives_kind = "full_siblings"
      ),
      list("born_at_year")
    ))
  })

  it("fails on unknown family history relationship kind", {
    expect_error(pipeline$get_tte(
      list(id = "SCZ"),
      list(
        id             = "CAD",
        relatives_kind = "unknown"
      ),
      list("born_at_year")
    ))
  })

  it("doesn't add relatives columns when population is requested", {
    tte <- pipeline$get_tte(
      list(id = "SCZ"),
      NA,
      list("born_at_year")
    )

    expect_true(!("relatives" %in% colnames(tte)))
    expect_true(!("relatives_diagnosed" %in% colnames(tte)))
    expect_true(!("weight" %in% colnames(tte)))
  })

  it("adds relatives columns when relatives are requested", {
    tte <- pipeline$get_tte(
      list(id = "SCZ"),
      list(
        id             = "SCZ",
        relatives_kind = "parents"
      ),
      list("born_at_year"),
      TRUE
    )

    expect_true("relatives" %in% colnames(tte))
    expect_true("relatives_diagnosed" %in% colnames(tte))
    expect_true("weight" %in% colnames(tte))
  })
})

describe("run", {
  it("doesn't allow empty arguments", {
    expect_error(pipeline$run())
  })

  it("fails when traits are not found", {
    expect_error(pipeline$run(
      analysis1 = list(
        id                    = "unknown",
        earliest_onset        = 1,
        latest_onset          = 100,
        relatives_kind        = "parents",
        relatives_coefficient = 0.5
      ),
      analysis2 = list(
        id                    = "CAD",
        earliest_onset        = 0,
        latest_onset          = 100,
        relatives_kind        = "parents",
        relatives_coefficient = 0.5
      ),
    ))

    expect_error(pipeline$run(
      analysis1 = list(
        id                    = "SCZ",
        earliest_onset        = 1,
        latest_onset          = 100,
        relatives_kind        = "parents",
        relatives_coefficient = 0.5
      ),
      analysis2 = list(
        id                    = "unknown",
        earliest_onset        = 0,
        latest_onset          = 100,
        relatives_kind        = "parents",
        relatives_coefficient = 0.5
      ),
    ))
  })

  it("fails when stratify column cannot be found in TTE dataset", {
    expect_error(pipeline$run(
      analysis1 = list(
        trait       = "SCZ",
        relatives   = "parents",
        relatedness = 0.5
      ),
      analysis2 = list(
        trait       = "CAD",
        relatives   = "parents",
        relatedness = 0.5
      ),
      draws = 2,
      stratify_columns = list("born_at_year", "unknown")
    ))
  })

  it("produces different result when using weighted cif", {
    results <- pipeline$run(
      analysis1 = list(
        trait       = "SCZ",
        relatives   = "parents",
        relatedness = 0.5
      ),
      analysis2 = list(
        trait       = "CAD",
        relatives   = "parents",
        relatedness = 0.5
      ),
      stratify_columns = list("born_at_year"),
      use_weighted_cif = FALSE
    )

    weighted_results <- pipeline$run(
      analysis1 = list(
        trait       = "SCZ",
        relatives   = "parents",
        relatedness = 0.5
      ),
      analysis2 = list(
        trait       = "CAD",
        relatives   = "parents",
        relatedness = 0.5
      ),
      stratify_columns = list("born_at_year"),
      use_weighted_cif = TRUE
    )

    expect_dataframe_not_equal(results$rg, weighted_results$rg)
  })
})
