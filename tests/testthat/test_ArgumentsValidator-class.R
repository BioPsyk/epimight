library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Tests
#=================================================================================

describe("defaults", {
  it("adds defaults if no value was found", {
    cif_rules <- list(
      required   = TRUE,
      type       = "named_list",
      properties = list(
        index_trait = list(
          required = TRUE,
          type     = "string"
        ),
        relatives_trait = list(
          required = FALSE,
          type     = "string"
        ),
        relatives_kind = list(
          required = FALSE,
          type     = "string"
        ),
        stratify_columns = list(
          type    = "list",
          items   = list(type = "string"),
          default = list()
        ),
        use_weighted = list(
          type    = "logical",
          default = TRUE
        )
      )
    )

    h2_rules <- list(
      required   = TRUE,
      type       = "named_list",
      properties = list(
        cif_pop = cif_rules
      )
    )

    args <- list(
      cif_pop = list(
        index_trait = "SCZ"
      )
    )

    validator      <- do.call(ArgumentsValidator$new, h2_rules$properties)
    validated_args <- do.call(validator$run, args)

    message(rjson::toJSON(validated_args, indent = 2))

    expect_true("use_weighted" %in% validated_args$cif_pop)
    expect_true("stratify_columns" %in% validated_args$cif_pop)
  })
})

#describe("non-named lists arguments", {
#  validator <- ArgumentsValidator$new(
#    list(
#      required   = TRUE,
#      type       = "named_list",
#      properties = list(
#        id = list(
#          required = TRUE,
#          type     = "string"
#        ),
#        earliest_onset = list(
#          type    = "integer",
#          minimum = 1,
#          default = 1
#        ),
#        latest_onset = list(
#          type    = "integer",
#          minimum = 1
#        ),
#        relatives_kind = list(
#          required = TRUE,
#          type     = "string",
#          enum     = list("parents", "full_siblings")
#        )
#      )
#    ),
#    list(
#      required   = FALSE,
#      type       = "named_list",
#      properties = list(
#        id = list(
#          required = TRUE,
#          type     = "string"
#        ),
#        earliest_onset = list(
#          type    = "integer",
#          minimum = 1,
#          default = 1
#        ),
#        latest_onset = list(
#          type    = "integer",
#          minimum = 1
#        ),
#        relatives_kind = list(
#          required = TRUE,
#          type     = "string",
#          enum     = list("parents", "full_siblings")
#        )
#      )
#    ),
#    list(
#      type    = "list",
#      items   = list(type = "string"),
#      default = list()
#    )
#  )
#
#  it("handles all values given", {
#    d1_id      <- "SCZ"
#    d1_relkind <- "full_siblings"
#    d2_id      <- "CAD"
#    d2_relkind <- "parents"
#
#    args <- validator$run(
#      list(
#        id             = d1_id,
#        relatives_kind = d1_relkind
#      ),
#      list(
#        id             = d2_id,
#        relatives_kind = d2_relkind
#      ),
#      list("birth_date")
#    )
#
#    proband_trait   <- args[[1]]
#    relatives_trait <- args[[2]]
#    stratify_columns   <- args[[3]]
#
#    expect_equal(proband_trait$id, d1_id)
#    expect_equal(proband_trait$relatives_kind, d1_relkind)
#    expect_equal(relatives_trait$id, d2_id)
#    expect_equal(relatives_trait$relatives_kind, d2_relkind)
#  })
#
#  it("handles missing values in the middle", {
#    d1_id      <- "SCZ"
#    d1_relkind <- "full_siblings"
#    d2_id      <- "CAD"
#    d2_relkind <- "parents"
#
#    args <- validator$run(
#      list(
#        id             = d1_id,
#        relatives_kind = d1_relkind
#      ),
#      NA,
#      list("birth_date")
#    )
#
#    expect_true(is.list(args[[1]]))
#    expect_true(is.na(args[[2]]))
#    expect_true(is.list(args[[3]]))
#  })
#})
#
#describe("gen_pop_risk_validator", {
#  gen_pop_risk_validator <- ArgumentsValidator$new(
#    phenotype_icd_codes = list(
#      required = TRUE,
#      type = "list",
#      items = list(
#        type = "string"
#      )
#    ),
#    birth_date_min = list(
#      type = "date"
#    ),
#    birth_date_max = list(
#      type = "date"
#    ),
#    study_end_at = list(
#      required = TRUE,
#      type = "date"
#    ),
#    sex = list(
#      type = "string",
#      enum = list("male", "female")
#    ),
#    diagnosis_kind = list(
#      type = "list",
#      items = list(
#        type = "string",
#        enum = epimight:::diagnosis_kinds
#      )
#    ),
#    earliest_onset = list(
#      type = "integer",
#      default = 1,
#      minimum = 1
#    ),
#    latest_onset = list(
#      type = "integer",
#      default = 100,
#      minimum = 1
#    )
#  )
#
#  gen_pop_risk_validator$add_post_validation(function(args, ...) {
#    if (args$earliest_onset > args$latest_onset) {
#      stop("Argument 'earliest_onset' was larger than 'latest_onset'")
#    }
#
#    if (is.null(args$birth_date_max)) return()
#
#    if (args$study_end_at <= args$birth_date_max) {
#      stop("Argument 'study_end_at' was earlier than 'birth_date_max'")
#    }
#
#    if (is.null(args$birth_date_min)) return()
#
#    if (args$birth_date_min >= args$birth_date_max) {
#      stop("Argument 'birth_date_min' was same or later than 'birth_date_max'")
#    }
#  })
#
#  #=================================================================================
#  # Tests
#  #=================================================================================
#
#  it("Supplying correct values is successful", {
#    gen_pop_risk_validator$run(
#      phenotype_icd_codes = list("F20", "F30"),
#      birth_date_min = as.Date("1980-12-01"),
#      birth_date_max = as.Date("2020-12-01"),
#      study_end_at = as.Date("2020-12-02"),
#      sex = "male"
#    )
#
#    expect_no_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        study_end_at = as.Date("2020-12-01"),
#        sex = "male"
#      )
#    )
#
#    expect_no_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        birth_date_min = as.Date("1980-12-01"),
#        birth_date_max = as.Date("2020-12-01"),
#        study_end_at = as.Date("2020-12-02"),
#        sex = "male"
#      )
#    )
#
#    expect_no_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        birth_date_min = as.Date("1980-12-01"),
#        birth_date_max = as.Date("2020-12-01"),
#        study_end_at = as.Date("2020-12-02"),
#        sex = "male",
#        earliest_onset = 2,
#        latest_onset = 80
#      )
#    )
#  })
#
#  it("Not supplying required arguments fails", {
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30")
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        study_end_at = as.Date("2020-12-01")
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run()
#    )
#  })
#
#  it("Supplying incorrect types fails", {
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = "^F20|F30$",
#        study_end_at = as.Date("2020-12-01")
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        study_end_at = "a date/2020"
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        study_end_at = as.Date("2020-12-01"),
#        sex = 120
#      )
#    )
#  })
#
#  it("Supplying incorrect values fails", {
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        study_end_at = as.Date("2020-12-01"),
#        sex = "both"
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        birth_date_max = as.Date("2020-12-01"),
#        study_end_at = as.Date("2020-12-01")
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        birth_date_min = as.Date("1980-12-01"),
#        birth_date_max = as.Date("1980-12-01"),
#        study_end_at = as.Date("2020-12-02")
#      )
#    )
#
#    expect_error(
#      gen_pop_risk_validator$run(
#        phenotype_icd_codes = list("F20", "F30"),
#        birth_date_min = as.Date("1980-12-01"),
#        birth_date_max = as.Date("2020-12-01"),
#        study_end_at = as.Date("2020-12-02"),
#        sex = "male",
#        earliest_onset = 2,
#        latest_onset = 1
#      )
#    )
#  })
#})
#
#describe("heritability validator", {
#  h2_analysis <- HeritabilityAnalysis$new()
#
#  heritability_validator <- ArgumentsValidator$new(
#    relationship_kind = list(
#      required = TRUE,
#      type = "string",
#      enum = names(epimight:::relationship_kinds)
#    ),
#    cohort1 = list(
#      required = TRUE,
#      type = "data.table",
#      columns = list(
#        estimate = list(
#          type     = "numeric",
#          required = TRUE
#        ),
#        cases = list(
#          type     = "integer",
#          required = TRUE
#        ),
#        stuff = list(
#          type = "integer"
#        )
#      )
#    ),
#    cohort2 = list(
#      required = TRUE,
#      type = "data.table",
#      columns = list(
#        estimate = list(
#          type     = "numeric",
#          required = TRUE
#        ),
#        cases = list(
#          type     = "integer",
#          required = TRUE
#        )
#      )
#    )
#  )
#
#  dummy_cohort <- data.table(
#    estimate = c(0.2, 0.3, 0.4),
#    cases = c(10, 5, 2)
#  )
#
#  it("works as expected when given valid arguments", {
#    heritability_validator$run(
#      relationship_kind = "PO",
#      cohort1 = dummy_cohort,
#      cohort2 = dummy_cohort
#    )
#  })
#
#  it("fails using invalid relationship kinds", {
#    expect_error(
#      heritability_validator$run(
#        relationship_kind = "GA",
#        cohort1 = dummy_cohort,
#        cohort2 = dummy_cohort
#      )
#    )
#
#    expect_error(
#      heritability_validator$run(
#        relationship_kind = "GA",
#        cohort1 = dummy_cohort,
#        cohort2 = dummy_cohort
#      )
#    )
#  })
#
#  it("fails not supplying required data.table columns", {
#    expect_error(
#      heritability_validator$run(
#        relationship_kind = "PO",
#        cohort1 = dummy_cohort,
#        cohort2 = data.table(
#          cases = c(23.2, 0.6, 0.12)
#        )
#      )
#    )
#  })
#
#  it("fails using invalid data.table properties", {
#    expect_error(
#      heritability_validator$run(
#        relationship_kind = "PO",
#        cohort1 = dummy_cohort,
#        cohort2 = data.table(
#          estimate = c(10, 2, 5),
#          cases = c(23.2, 0.6, 0.12)
#        )
#      )
#    )
#  })
#})
#
#describe("named list type", {
#  validator <- ArgumentsValidator$new(
#    population_filter = list(
#      required = TRUE,
#      type = "named_list",
#      properties = list(
#        study_end_at = list(
#          required = TRUE,
#          type = "date"
#        ),
#        birth_date_min = list(type = "date"),
#        birth_date_max = list(type = "date")
#      )
#    ),
#    status = list(
#      type = "string"
#    )
#  )
#
#  it("fails when required properties are missing", {
#    expect_error(
#      validator$run(
#        population_filter = list(
#          birth_date_min = as.Date("2020-12-02")
#        )
#      )
#    )
#  })
#
#  it("fails when property is of wrong type", {
#    expect_error(
#      validator$run(
#        population_filter = list(
#          study_end_at = 20,
#          birth_date_min = as.Date("2020-12-02")
#        )
#      )
#    )
#  })
#
#  it("fails when regular list is given", {
#    expect_error(
#      validator$run(
#        population_filter = list(
#          as.Date("2020-12-02"),
#          as.Date("1985-01-01"),
#          as.Date("2020-01-01")
#        )
#      )
#    )
#  })
#
#  it("succeeds when only required properties are given", {
#    validator$run(
#      population_filter = list(
#        study_end_at = as.Date("2020-12-02")
#      )
#    )
#  })
#
#  it("succeeds when all properties are given", {
#    validator$run(
#      population_filter = list(
#        study_end_at = as.Date("2020-12-02"),
#        birth_date_min  = as.Date("1985-01-01"),
#        birth_date_max  = as.Date("2010-01-01")
#      ),
#      status = "dead"
#    )
#  })
#})
#
#describe("generic named list type", {
#  validator <- ArgumentsValidator$new(
#    diagnosis_filters = list(
#      required = TRUE,
#      type = "generic_named_list",
#      minimum_length = 1,
#      maximum_length = 2,
#      items = list(
#        type = "string"
#      )
#    )
#  )
#
#  it("fails when too few elements are given", {
#    expect_error(
#      validator$run(diagnosis_filters = list())
#    )
#  })
#
#  it("fails when too many elements are given", {
#    expect_error(
#      validator$run(
#        diagnosis_filters = list(
#          target1 = "SCZ",
#          target2 = "MDD",
#          excl = "CHD"
#        )
#      )
#    )
#  })
#
#  it("succeeds when all properties are given", {
#    validator$run(
#      diagnosis_filters = list(
#        target = "SCZ",
#        excl = "CHD"
#      )
#    )
#  })
#})
#
#describe("list type minimum length", {
#  validator <- ArgumentsValidator$new(
#    sex = list(
#      required = TRUE,
#      type = "list",
#      minimum_length = 1,
#      maximum_length = 2,
#      items = list(
#        type = "string"
#      )
#    )
#  )
#
#  it("fails when too few elements are given", {
#    expect_error(
#      validator$run(sex = list())
#    )
#  })
#
#  it("fails when too many elements are given", {
#    expect_error(
#      validator$run(sex = list("male", "female", "male"))
#    )
#  })
#
#  it("works as expected when same or more elements given", {
#    validator$run(sex = list("male"))
#    validator$run(sex = list("male", "female"))
#  })
#})
#
#describe("date type", {
#  validator <- ArgumentsValidator$new(
#    birth_date = list(
#      required = TRUE,
#      type = "date"
#    )
#  )
#
#  it("succeeds on Date type", {
#    validator$run(birth_date = as.Date("2020-12-03"))
#    validator$run(birth_date = as.Date("1800-03-28"))
#  })
#
#  it("succeeds on strings with right format", {
#    validator$run(birth_date = "2020-12-03")
#    validator$run(birth_date = "1800-03-28")
#  })
#
#  it("fails on incorrect formats", {
#    expect_error(
#      validator$run(birth_date = "a2020-12-03")
#    )
#    expect_error(
#      validator$run(birth_date = "180003/28")
#    )
#  })
#})
#
#describe("integer enum", {
#  validator <- ArgumentsValidator$new(
#    failure_status = list(
#      type     = "integer",
#      enum     = list(0, 1, 2),
#      required = TRUE
#    )
#  )
#
#  it("succeeds on correct values", {
#    validator$run(failure_status = 0)
#    validator$run(failure_status = 1)
#    validator$run(failure_status = 2)
#  })
#
#  it("fails on unknown enums", {
#    expect_error(
#      validator$run(failure_status = 3)
#    )
#    expect_error(
#      validator$run(failure_status = -1)
#    )
#  })
#
#  it("fails on incorrect types", {
#    expect_error(
#      validator$run(failure_status = "asd")
#    )
#    expect_error(
#      validator$run(failure_status = FALSE)
#    )
#  })
#})
#
#describe("data.table integer enum", {
#  validator <- ArgumentsValidator$new(
#    tte = list(
#      required = TRUE,
#      type = "data.table",
#      columns = list(
#        failure_status = list(
#          type     = "integer",
#          enum     = list(0, 1, 2),
#          required = TRUE
#        )
#      )
#    )
#  )
#
#  it("succeeds on correct values", {
#    validator$run(tte = data.table(failure_status = c(0)))
#    validator$run(tte = data.table(failure_status = c(0, 1)))
#    validator$run(tte = data.table(failure_status = c(0, 1, 2)))
#    validator$run(tte = data.table(failure_status = c(0, 1, 2, 1, 0, 2)))
#  })
#
#  it("fails on unknown enums", {
#    expect_error(
#      validator$run(tte = data.table(failure_status = c(3)))
#    )
#    expect_error(
#      validator$run(tte = data.table(failure_status = c(3, 4)))
#    )
#    expect_error(
#      validator$run(tte = data.table(failure_status = c(0, 1, -1)))
#    )
#  })
#})
