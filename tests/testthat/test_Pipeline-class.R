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
        person_id         = c("p1", "p1"),
        birth_year        = c(1950, 1951),
        trait             = c("SCZ", "CAD"),
        trait_status      = c(0, 1),
        trait_age        = c(10, 10),
        relatives_kind    = c(10, 20), # Wrong type
        relatives_n       = c(1, 1),
        relatives_n_trait = c(0, 0)
      )
    ))
  })

  it("doesn't allow numeric person IDs", {
    expect_error(Pipeline$new(
      pool = data.table(
        person_id         = c(1, 1),
        birth_year        = c(1950, 1951),
        trait             = c("SCZ", "CAD"),
        trait_status      = c(0, 1),
        trait_age       = c(10, 20),
        relatives_kind    = c("parents", "parents"),
        relatives_n       = c(2, 2),
        relatives_n_trait = c(0, 1)
      )
    ))
  })

  it("allows valid tte", {
    Pipeline$new(
      pool = data.table(
        person_id         = c("p1", "p1"),
        birth_year        = c(1950, 1951),
        trait             = c("SCZ", "CAD"),
        trait_status      = c(0, 1),
        trait_age       = c(10, 20),
        relatives_kind    = c("parents", "parents"),
        relatives_n       = c(2, 2),
        relatives_n_trait = c(0, 1)
      )
    )
  })
})

#describe("get_tte", {
#  it("fails on unknown traits", {
#    expect_error(pipeline$get_tte(
#      index_trait     = "unknown",
#      relatives_trait = "SCZ",
#      relatives_kind  = "half_siblings"
#    ))
#
#    expect_error(pipeline$get_tte(
#      index_trait     = "CAD",
#      relatives_trait = "unknown",
#      relatives_kind  = "half_siblings"
#    ))
#  })
#
#  it("fails on unknown relatives kind", {
#    expect_error(pipeline$get_tte(
#      index_trait     = "CAD",
#      relatives_trait = "SCZ",
#      relatives_kind  = "unknown"
#    ))
#  })
#
#  it("doesn't add relatives columns when population is requested", {
#    tte <- pipeline$get_tte(
#      index_trait = "CAD"
#    )
#
#    expect_true(!("relatives_kind" %in% colnames(tte)))
#    expect_true(!("relatives_n" %in% colnames(tte)))
#    expect_true(!("relatives_n_trait" %in% colnames(tte)))
#    expect_true(!("weight" %in% colnames(tte)))
#  })
#
#  it("adds relatives columns when relatives are requested", {
#    tte <- pipeline$get_tte(
#      index_trait     = "CAD",
#      relatives_trait = "SCZ",
#      relatives_kind  = "half_siblings"
#    )
#
#    expect_true("relatives_kind" %in% colnames(tte))
#    expect_true("relatives_n" %in% colnames(tte))
#    expect_true("relatives_n_trait" %in% colnames(tte))
#    expect_true("weight" %in% colnames(tte))
#  })
#})

describe("get_results", {
  it("successfully retrieves results using same type and arguments as when added", {
    pipeline$clear_results()

    results <- data.table(
      age = c(1, 2, 1, 2),
      cif = c(0.3, 0.31, 0.21, 0.25),
      se  = c(0.3, 0.31, 0.21, 0.25),
      l95 = c(0.3, 0.31, 0.21, 0.25),
      u95 = c(0.3, 0.31, 0.21, 0.25)
    )

    args <- list(
      index_trait     = "SCZ",
      relatives_trait = "CAD",
      relatives_kind  = "parents"
    )

    pipeline$add_results("cif", results, args)
  })
})

#describe("run_cif", {
#  it("adds produced result to cache", {
#    results <- pipeline$run_cif(
#      index_trait     = "SCZ",
#      relatives_trait = "CAD",
#      relatives_kind  = "parents"
#    )
#
#    print(results)
#  })
#})

#
#describe("run", {
#  it("doesn't allow empty arguments", {
#    expect_error(pipeline$run())
#  })
#
#  it("fails when traits are not found", {
#    expect_error(pipeline$run(
#      heritability1 = list(
#        trait          = "unknown",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      heritability2 = list(
#        trait          = "CAD",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      genetic_correlation = list(
#        trait          = "CAD",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      )
#    ))
#
#    expect_error(pipeline$run(
#      heritability1 = list(
#        trait          = "SCZ",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      heritability2 = list(
#        trait          = "unknown",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      genetic_correlation = list(
#        trait          = "CAD",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      )
#    ))
#  })
#
#  it("fails when stratify column cannot be found in TTE dataset", {
#    expect_error(pipeline$run(
#      heritability1 = list(
#        trait          = "SCZ",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      heritability2 = list(
#        trait          = "CAD",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      genetic_correlation = list(
#        trait          = "CAD",
#        relatives_kind = "parents",
#        relatedness    = 0.5
#      ),
#      stratify_columns = list("birth_year", "unknown")
#    ))
#  })
#
#  it("produces different result when using weighted cif", {
#    results <- pipeline$run(
#      heritability1 = list(
#        index_trait     = "SCZ",
#        relatives_kind  = "parents",
#        relatedness     = 0.5
#      ),
#      heritability2 = list(
#        index_trait     = "CAD",
#        relatives_kind  = "half_siblings",
#        relatedness     = 0.25
#      ),
#      use_weighted_cif = FALSE
#    )
#
#    weighted_results <- pipeline$run(
#      heritability1 = list(
#        index_trait     = "SCZ",
#        relatives_kind  = "parents",
#        relatedness     = 0.5
#      ),
#      heritability2 = list(
#        index_trait     = "CAD",
#        relatives_kind  = "half_siblings",
#        relatedness     = 0.25
#      ),
#      use_weighted_cif = TRUE
#    )
#
#    expect_dataframe_not_equal(results$rg, weighted_results$rg)
#
#    results <- pipeline$run(
#      heritability1 = list(
#        index_trait     = "SCZ",
#        relatives_kind  = "parents",
#        relatedness     = 0.5
#      ),
#      heritability2 = list(
#        index_trait     = "CAD",
#        relatives_kind  = "half_siblings",
#        relatedness     = 0.25
#      ),
#      stratify_columns = list("birth_year"),
#      use_weighted_cif = FALSE
#    )
#
#    weighted_results <- pipeline$run(
#      heritability1 = list(
#        index_trait     = "SCZ",
#        relatives_kind  = "parents",
#        relatedness     = 0.5
#      ),
#      heritability2 = list(
#        index_trait     = "CAD",
#        relatives_kind  = "half_siblings",
#        relatedness     = 0.25
#      ),
#      stratify_columns = list("birth_year"),
#      use_weighted_cif = TRUE
#    )
#
#    expect_dataframe_not_equal(results$rg, weighted_results$rg)
#  })
#})
