library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Tests
#=================================================================================

describe("gen_pop_risk_validator", {
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
      enum = epimight:::genders
    ),
    diagnosis_kind = list(
      type = "list",
      items = list(
        type = "string",
        enum = epimight:::diagnosis_kinds
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

  it("Supplying correct values is successful", {
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

  it("Not supplying required arguments fails", {
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

  it("Supplying incorrect types fails", {
    expect_error(
      gen_pop_risk_validator$run(
        phenotype_icd_codes = "^F20|F30$",
        study_end_at = as.Date("2020-12-01")
      )
    )

    expect_error(
      gen_pop_risk_validator$run(
        phenotype_icd_codes = list("F20", "F30"),
        study_end_at = "a date/2020"
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

  it("Supplying incorrect values fails", {
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
})

describe("heritability validator", {
  h2_analysis <- HeritabilityAnalysis$new()

  heritability_validator <- ArgumentsValidator$new(
    relationship_kind = list(
      required = TRUE,
      type = "string",
      enum = names(epimight:::relationship_kinds)
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

describe("generic named list type", {
  validator <- ArgumentsValidator$new(
    diagnosis_filters = list(
      required = TRUE,
      type = "generic_named_list",
      minimum_length = 1,
      maximum_length = 2,
      items = list(
        type = "string"
      )
    )
  )

  it("fails when too few elements are given", {
    expect_error(
      validator$run(diagnosis_filters = list())
    )
  })

  it("fails when too many elements are given", {
    expect_error(
      validator$run(
        diagnosis_filters = list(
          target1 = "SCZ",
          target2 = "MDD",
          excl = "CHD"
        )
      )
    )
  })

  it("succeeds when all properties are given", {
    validator$run(
      diagnosis_filters = list(
        target = "SCZ",
        excl = "CHD"
      )
    )
  })
})

describe("list type minimum length", {
  validator <- ArgumentsValidator$new(
    genders = list(
      required = TRUE,
      type = "list",
      minimum_length = 1,
      maximum_length = 2,
      items = list(
        type = "string"
      )
    )
  )

  it("fails when too few elements are given", {
    expect_error(
      validator$run(genders = list())
    )
  })

  it("fails when too many elements are given", {
    expect_error(
      validator$run(genders = list("male", "female", "male"))
    )
  })

  it("works as expected when same or more elements given", {
    validator$run(genders = list("male"))
    validator$run(genders = list("male", "female"))
  })
})

describe("date type", {
  validator <- ArgumentsValidator$new(
    born_at = list(
      required = TRUE,
      type = "date"
    )
  )

  it("succeeds on Date type", {
    validator$run(born_at = as.Date("2020-12-03"))
    validator$run(born_at = as.Date("1800-03-28"))
  })

  it("succeeds on strings with right format", {
    validator$run(born_at = "2020-12-03")
    validator$run(born_at = "1800-03-28")
  })

  it("fails on incorrect formats", {
    expect_error(
      validator$run(born_at = "a2020-12-03")
    )
    expect_error(
      validator$run(born_at = "180003/28")
    )
  })
})

describe("integer enum", {
  validator <- ArgumentsValidator$new(
    failure_status = list(
      type     = "integer",
      enum     = list(0, 1, 2),
      required = TRUE
    )
  )

  it("succeeds on correct values", {
    validator$run(failure_status = 0)
    validator$run(failure_status = 1)
    validator$run(failure_status = 2)
  })

  it("fails on unknown enums", {
    expect_error(
      validator$run(failure_status = 3)
    )
    expect_error(
      validator$run(failure_status = -1)
    )
  })

  it("fails on incorrect types", {
    expect_error(
      validator$run(failure_status = "asd")
    )
    expect_error(
      validator$run(failure_status = FALSE)
    )
  })
})

describe("data.table integer enum", {
  validator <- ArgumentsValidator$new(
    tte = list(
      required = TRUE,
      type = "data.table",
      columns = list(
        failure_status = list(
          type     = "integer",
          enum     = list(0, 1, 2),
          required = TRUE
        )
      )
    )
  )

  it("succeeds on correct values", {
    validator$run(tte = data.table(failure_status = c(0)))
    validator$run(tte = data.table(failure_status = c(0, 1)))
    validator$run(tte = data.table(failure_status = c(0, 1, 2)))
    validator$run(tte = data.table(failure_status = c(0, 1, 2, 1, 0, 2)))
  })

  it("fails on unknown enums", {
    expect_error(
      validator$run(tte = data.table(failure_status = c(3)))
    )
    expect_error(
      validator$run(tte = data.table(failure_status = c(3, 4)))
    )
    expect_error(
      validator$run(tte = data.table(failure_status = c(0, 1, -1)))
    )
  })
})
