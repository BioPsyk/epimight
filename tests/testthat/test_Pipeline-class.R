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

describe("get_tte", {
  it("fails on unknown stratify_columns", {
    expect_error(pipeline$get_tte(
      list("unknown"),
      FALSE,
      "CAD",
      "SCZ",
      "half_siblings"
    ))
  })

  it("fails on unknown traits", {
    expect_error(pipeline$get_tte(
      list(),
      FALSE,
      "CAD",
      "unknown",
      "half_siblings"
    ))

    expect_error(pipeline$get_tte(
      list("birth_year"),
      TRUE,
      "unknown",
      "SCZ",
      "half_siblings"
    ))
  })

  it("fails on unknown family history relationship kind", {
    expect_error(pipeline$get_tte(
      list("birth_year"),
      TRUE,
      "CAD",
      "SCZ",
      "unknown"
    ))
  })

  it("doesn't add relatives columns when population is requested", {
    tte <- pipeline$get_tte(
      list("birth_year"),
      TRUE,
      "CAD"
    )

    expect_true(!("relatives_kind" %in% colnames(tte)))
    expect_true(!("relatives_n" %in% colnames(tte)))
    expect_true(!("relatives_n_trait" %in% colnames(tte)))
    expect_true(!("weight" %in% colnames(tte)))
  })

  it("adds relatives columns when relatives are requested", {
    tte <- pipeline$get_tte(
      list("birth_year"),
      TRUE,
      "CAD",
      "SCZ",
      "parents"
    )

    expect_true("relatives_kind" %in% colnames(tte))
    expect_true("relatives_n" %in% colnames(tte))
    expect_true("relatives_n_trait" %in% colnames(tte))
    expect_true("weight" %in% colnames(tte))
  })
})

describe("cache", {
  it("fails when the given results is not a data.table", {
    expect_error(pipeline$add_to_cache(12, "cif", "SCZ"))
    expect_error(pipeline$add_to_cache("Hello", "cif", "SCZ"))
    expect_error(pipeline$add_to_cache(NA, "cif", "SCZ"))
    expect_error(pipeline$add_to_cache(NULL, "cif", "SCZ"))
  })

  it("successfully finds cached results by correct path", {
    cif <- data.table(
      cif   = c(0.5, 0.4),
      se    = c(0.2, 0.3),
      cases = c(1, 2)
    )

    pipeline$add_to_cache(cif, "cif", "SCZ")
    expect_dataframe_equal(pipeline$get_from_cache("cif", "SCZ"), cif)
  })

  it("ignores NULL steps when adding to cache", {
    cif <- data.table(
      cif   = c(0.5, 0.4),
      se    = c(0.2, 0.3),
      cases = c(1, 2)
    )

    pipeline$add_to_cache(cif, "cif", "SCZ", NULL, NULL)
    expect_dataframe_equal(pipeline$get_from_cache("cif", "SCZ"), cif)

    pipeline$add_to_cache(cif, "cif", NULL, "SCZ2", NULL)
    expect_dataframe_equal(pipeline$get_from_cache("cif", "SCZ2"), cif)
  })

  it("handles unknown paths gracefully", {
    cif <- data.table(
      cif   = c(0.5, 0.4),
      se    = c(0.2, 0.3),
      cases = c(1, 2)
    )

    pipeline$add_to_cache(cif, "cif", "SCZ")
    expect_equal(pipeline$get_from_cache("cif", "SCZ", "unknown"), NULL)
    expect_equal(pipeline$get_from_cache("rg"), NULL)
    expect_equal(pipeline$get_from_cache(NULL), NULL)
  })

  it("returns NULL when a partial path is given", {
    cif <- data.table(
      cif   = c(0.5, 0.4),
      se    = c(0.2, 0.3),
      cases = c(1, 2)
    )

    pipeline$add_to_cache(cif, "cif", "SCZ")
    expect_equal(pipeline$get_from_cache("cif"), NULL)
  })
})

describe("run", {
  it("doesn't allow empty arguments", {
    expect_error(pipeline$run())
  })

  it("fails when traits are not found", {
    expect_error(pipeline$run(
      heritability1 = list(
        trait          = "unknown",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      genetic_correlation = list(
        trait          = "CAD",
        relatives_kind = "parents",
        relatedness    = 0.5
      )
    ))

    expect_error(pipeline$run(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "unknown",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      genetic_correlation = list(
        trait          = "CAD",
        relatives_kind = "parents",
        relatedness    = 0.5
      )
    ))
  })

  it("fails when stratify column cannot be found in TTE dataset", {
    expect_error(pipeline$run(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      genetic_correlation = list(
        trait          = "CAD",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      stratify_columns = list("birth_year", "unknown")
    ))
  })

  it("produces different result when using weighted cif", {
    results <- pipeline$run(
      heritability1 = list(
        index_trait     = "SCZ",
        relatives_kind  = "parents",
        relatedness     = 0.5
      ),
      heritability2 = list(
        index_trait     = "CAD",
        relatives_kind  = "half_siblings",
        relatedness     = 0.25
      ),
      use_weighted_cif = FALSE
    )

    weighted_results <- pipeline$run(
      heritability1 = list(
        index_trait     = "SCZ",
        relatives_kind  = "parents",
        relatedness     = 0.5
      ),
      heritability2 = list(
        index_trait     = "CAD",
        relatives_kind  = "half_siblings",
        relatedness     = 0.25
      ),
      use_weighted_cif = TRUE
    )

    expect_dataframe_not_equal(results$rg, weighted_results$rg)

    results <- pipeline$run(
      heritability1 = list(
        index_trait     = "SCZ",
        relatives_kind  = "parents",
        relatedness     = 0.5
      ),
      heritability2 = list(
        index_trait     = "CAD",
        relatives_kind  = "half_siblings",
        relatedness     = 0.25
      ),
      stratify_columns = list("birth_year"),
      use_weighted_cif = FALSE
    )

    weighted_results <- pipeline$run(
      heritability1 = list(
        index_trait     = "SCZ",
        relatives_kind  = "parents",
        relatedness     = 0.5
      ),
      heritability2 = list(
        index_trait     = "CAD",
        relatives_kind  = "half_siblings",
        relatedness     = 0.25
      ),
      stratify_columns = list("birth_year"),
      use_weighted_cif = TRUE
    )

    expect_dataframe_not_equal(results$rg, weighted_results$rg)
  })
})
