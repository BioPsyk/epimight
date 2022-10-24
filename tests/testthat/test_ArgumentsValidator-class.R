library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Preparation
#=================================================================================

gen_pop_risk_validator <- ArgumentsValidator$new(
  phenotype_icd_codes = list(
    required = TRUE,
    type = "list",
    items = list(
      type = "string"
    )
  ),
  born_at_min = list(
    type = "date"
  ),
  born_at_max = list(
    type = "date"
  ),
  study_end_at = list(
    required = TRUE,
    type = "date"
  ),
  gender = list(
    type = "string",
    enum = IbpRiskEstimations:::genders
  ),
  diagnosis_kind = list(
    type = "list",
    items = list(
      type = "string",
      enum = IbpRiskEstimations:::diagnosis_kinds
    )
  ),
  earliest_onset = list(
    type = "integer",
    default = 1,
    minimum = 1
  ),
  latest_onset = list(
    type = "integer",
    default = 100,
    minimum = 1
  )
)

gen_pop_risk_validator$add_post_validation(function(args, ...) {
  if (args$earliest_onset > args$latest_onset) {
    stop("Argument 'earliest_onset' was larger than 'latest_onset'")
  }

  if (is.null(args$born_at_max)) return()

  if (args$study_end_at <= args$born_at_max) {
    stop("Argument 'study_end_at' was earlier than 'born_at_max'")
  }

  if (is.null(args$born_at_min)) return()

  if (args$born_at_min >= args$born_at_max) {
    stop("Argument 'born_at_min' was same or later than 'born_at_max'")
  }
})

#=================================================================================
# Tests
#=================================================================================

test_that("Supplying correct values is successful", {
  gen_pop_risk_validator$run(
    phenotype_icd_codes = list("F20", "F30"),
    born_at_min = as.Date("1980-12-01"),
    born_at_max = as.Date("2020-12-01"),
    study_end_at = as.Date("2020-12-02"),
    gender = "male"
  )

  expect_no_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      study_end_at = as.Date("2020-12-01"),
      gender = "male"
    )
  )

  expect_no_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      born_at_min = as.Date("1980-12-01"),
      born_at_max = as.Date("2020-12-01"),
      study_end_at = as.Date("2020-12-02"),
      gender = "male"
    )
  )

  expect_no_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      born_at_min = as.Date("1980-12-01"),
      born_at_max = as.Date("2020-12-01"),
      study_end_at = as.Date("2020-12-02"),
      gender = "male",
      earliest_onset = 2,
      latest_onset = 80
    )
  )
})

test_that("Not supplying required arguments fails", {
  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30")
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      study_end_at = as.Date("2020-12-01")
    )
  )

  expect_error(
    gen_pop_risk_validator$run()
  )
})

test_that("Supplying incorrect types fails", {
  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = "^F20|F30$",
      study_end_at = as.Date("2020-12-01")
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      study_end_at = "2020-12-01"
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      study_end_at = as.Date("2020-12-01"),
      gender = 120
    )
  )
})

test_that("Supplying incorrect values fails", {
  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      study_end_at = as.Date("2020-12-01"),
      gender = "both"
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      born_at_max = as.Date("2020-12-01"),
      study_end_at = as.Date("2020-12-01")
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      born_at_min = as.Date("1980-12-01"),
      born_at_max = as.Date("1980-12-01"),
      study_end_at = as.Date("2020-12-02")
    )
  )

  expect_error(
    gen_pop_risk_validator$run(
      phenotype_icd_codes = list("F20", "F30"),
      born_at_min = as.Date("1980-12-01"),
      born_at_max = as.Date("2020-12-01"),
      study_end_at = as.Date("2020-12-02"),
      gender = "male",
      earliest_onset = 2,
      latest_onset = 1
    )
  )
})

describe("heritability validator", {
  h2_analysis <- HeritabilityAnalysis$new()

  heritability_validator <- ArgumentsValidator$new(
    relationship_kind = list(
      required = TRUE,
      type = "string",
      enum = names(IbpRiskEstimations:::relationship_kinds)
    ),
    cohort1 = list(
      required = TRUE,
      type = "data.table",
      columns = list(
        estimate = list(
          type     = "numeric",
          required = TRUE
        ),
        cases = list(
          type     = "integer",
          required = TRUE
        ),
        stuff = list(
          type = "integer"
        )
      )
    ),
    cohort2 = list(
      required = TRUE,
      type = "data.table",
      columns = list(
        estimate = list(
          type     = "numeric",
          required = TRUE
        ),
        cases = list(
          type     = "integer",
          required = TRUE
        )
      )
    )
  )

  dummy_cohort <- data.table(
    estimate = c(0.2, 0.3, 0.4),
    cases = c(10, 5, 2)
  )

  it("works as expected when given valid arguments", {
    heritability_validator$run(
      relationship_kind = "PO",
      cohort1 = dummy_cohort,
      cohort2 = dummy_cohort
    )
  })

  it("fails using invalid relationship kinds", {
    expect_error(
      heritability_validator$run(
        relationship_kind = "GA",
        cohort1 = dummy_cohort,
        cohort2 = dummy_cohort
      )
    )

    expect_error(
      heritability_validator$run(
        relationship_kind = "GA",
        cohort1 = dummy_cohort,
        cohort2 = dummy_cohort
      )
    )
  })

  it("fails not supplying required data.table columns", {
    expect_error(
      heritability_validator$run(
        relationship_kind = "PO",
        cohort1 = dummy_cohort,
        cohort2 = data.table(
          cases = c(23.2, 0.6, 0.12)
        )
      )
    )
  })

  it("fails using invalid data.table properties", {
    expect_error(
      heritability_validator$run(
        relationship_kind = "PO",
        cohort1 = dummy_cohort,
        cohort2 = data.table(
          estimate = c(10, 2, 5),
          cases = c(23.2, 0.6, 0.12)
        )
      )
    )
  })
})

describe("named list type", {
  validator <- ArgumentsValidator$new(
    population_filter = list(
      required = TRUE,
      type = "named_list",
      properties = list(
        study_end_at = list(
          required = TRUE,
          type = "date"
        ),
        born_at_min = list(type = "date"),
        born_at_max = list(type = "date")
      )
    ),
    status = list(
      type = "string"
    )
  )

  it("fails when required properties are missing", {
    expect_error(
      validator$run(
        population_filter = list(
          born_at_min = as.Date("2020-12-02")
        )
      )
    )
  })

  it("fails when property is of wrong type", {
    expect_error(
      validator$run(
        population_filter = list(
          study_end_at = 20,
          born_at_min = as.Date("2020-12-02")
        )
      )
    )
  })

  it("fails when regular list is given", {
    expect_error(
      validator$run(
        population_filter = list(
          as.Date("2020-12-02"),
          as.Date("1985-01-01"),
          as.Date("2020-01-01")
        )
      )
    )
  })

  it("succeeds when only required properties are given", {
    validator$run(
      population_filter = list(
        study_end_at = as.Date("2020-12-02")
      )
    )
  })

  it("succeeds when all properties are given", {
    validator$run(
      population_filter = list(
        study_end_at = as.Date("2020-12-02"),
        born_at_min  = as.Date("1985-01-01"),
        born_at_max  = as.Date("2010-01-01")
      ),
      status = "dead"
    )
  })
})
