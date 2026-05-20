library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

output_directory <- "../../tmp"
output_prefix    <- "tte_retriever_test"
output_path      <- file.path(output_directory, output_prefix)
hostname         <- "localhost"
username         <- "postgres"
password         <- "devpass"
csv_path         <- paste0(output_path, ".csv")
tte_retriever    <- TTERetriever$new("../../tmp", hostname, username, password)
qg               <- QueryGenerator$new()

#=================================================================================
# Tests
#=================================================================================


compare_queries <- function(old_query, new_query) {
  qg$execute_query(old_query, csv_path, hostname, username, password)
  old_results <- read_csv(csv_path, show_col_types = FALSE) |>
    arrange(person_id) |>
    select(person_id, failure_status, failure_time) |>
    as.data.frame()

  tte_retriever$execute_query(output_path, new_query)
  new_results <- read_csv(csv_path, show_col_types = FALSE) |>
    arrange(person_id) |>
    select(person_id, fract_failure_status, fract_failure_time) |>
    rename(
      failure_status = fract_failure_status,
      failure_time   = fract_failure_time
    ) |>
    mutate(failure_time = round(failure_time)) |>
    as.data.frame()

  expect_dataframe_equal(old_results, new_results)
}

describe("run", {
  valid_args <- list(
    samples = list(
      diagnosis_filters = list(
        fract = list(
          icd_codes_regexp = "^80"
        )
      )
    ),
    study_end_at = as.Date("2016-12-31")
  )

  it("fails when invalid argument types are given", {
    expect_error(
      tte_retriever$run(FALSE, "my_args")
    )

    expect_error(
      tte_retriever$run(FALSE, valid_args)
    )
  })

  it("fails when output_prefix contains illegal characters", {
    expect_error(
      tte_retriever$run("my_invalid/prefix", valid_args)
    )

    expect_error(
      tte_retriever$run("prefix!", valid_args)
    )

    expect_error(
      tte_retriever$run("", valid_args)
    )
  })
})

describe("run_from_file", {
  args_path <- paste0(output_path, "_input.yaml")

  it("it works with the minimum required args", {
    args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = "^80"
          )
        )
      ),
      study_end_at = "2016-12-31"
    )

    tte_retriever$write_args(args, args_path)

    paths <- tte_retriever$run_from_file(output_prefix, args_path)
  })

  it("works with more advanced args", {
    args <- list(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        )
      ),
      relatives = list(
        relationship_filters = list(
          kind = "PO"
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("gender", "incl_diagnosed_at")
    )

    tte_retriever$write_args(args, args_path)

    paths <- tte_retriever$run_from_file(output_prefix, args_path)
  })
})

describe("single disorder", {
  it("produces a valid query with only the requirements", {
    old_args <- list(
      icd_codes_regexp = "^80",
      study_end_at = "2016-12-31"
    )
    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = "^80"
          )
        )
      ),
      study_end_at = as.Date("2016-12-31")
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })

  it("produces a valid query with all sample_filters", {
    old_args <- list(
      icd_codes_regexp = "^80",
      birth_date_min = "1800-01-01",
      birth_date_max = "2010-01-01",
      gender = "female",
      status = list(
        "danish-resident",
        "danish-resident-special-address",
        "greenlandic-resident",
        "greenlandic-resident-special-address",
        "annulled-cpr-number",
        "emigrated",
        "dead"
      ),
      study_end_at = "2016-04-01"
    )

    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = old_args$icd_codes_regexp
          )
        ),
        individual_filters = list(
          born_at_min = as.Date(old_args$birth_date_min),
          born_at_max = as.Date(old_args$birth_date_max),
          gender = old_args$gender,
          status = old_args$status
        )
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })

  it("produces a valid query with all diagnosis_filters", {
    old_args <- list(
      icd_codes_regexp = "^80",
      diagnosis_kind = list(
        "main",
        "auxiliary"
      ),
      record_origin = "pcrr",
      study_end_at = "2016-04-01"
    )

    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = old_args$icd_codes_regexp,
            diagnosis_kinds = old_args$diagnosis_kind,
            record_origin = old_args$record_origin
          )
        )
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })

  it("produces a valid query with all filters", {
    old_args <- list(
      icd_codes_regexp = "^80",
      diagnosis_kind = list(
        "main",
        "auxiliary"
      ),
      birth_date_min = "1800-01-01",
      birth_date_max = "2010-01-01",
      gender = "female",
      status = list(
        "danish-resident",
        "danish-resident-special-address",
        "greenlandic-resident",
        "greenlandic-resident-special-address",
        "annulled-cpr-number",
        "emigrated",
        "dead"
      ),
      study_end_at = "2016-04-01"
    )

    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
          icd_codes_regexp = old_args$icd_codes_regexp,
          diagnosis_kinds = old_args$diagnosis_kind
          )
        ),
        individual_filters = list(
          born_at_min = as.Date(old_args$birth_date_min),
          born_at_max = as.Date(old_args$birth_date_max),
          gender = old_args$gender,
          status = old_args$status
        )
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })
})

describe("single disorder with relatives", {
  it("produces a valid query with only the requirements", {
    old_args <- list(
      icd_codes_regexp = ".*",
      study_end_at = "2016-12-31",
      relationship_kind = "PO"
    )

    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = old_args$icd_codes_regexp
          )
        )
      ),
      relatives = list(
        relationship_filters = list(
          kind = old_args$relationship_kind,
          component = "pedigree1"
        )
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$family_survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })

  it("produces a valid query with all filters", {
    old_args <- list(
      icd_codes_regexp = "^80",
      diagnosis_kind = list(
        "main",
        "auxiliary"
      ),
      birth_date_min = "1800-01-01",
      birth_date_max = "2010-01-01",
      gender = "female",
      status = list(
        "danish-resident",
        "danish-resident-special-address",
        "greenlandic-resident",
        "greenlandic-resident-special-address",
        "annulled-cpr-number",
        "emigrated",
        "dead"
      ),
      study_end_at = "2016-04-01",
      relationship_kind = "PO"
    )

    new_args <- list(
      samples = list(
        diagnosis_filters = list(
          fract = list(
            icd_codes_regexp = old_args$icd_codes_regexp,
            diagnosis_kinds = old_args$diagnosis_kind
          )
        ),
        individual_filters = list(
          born_at_min = as.Date(old_args$birth_date_min),
          born_at_max = as.Date(old_args$birth_date_max),
          gender = old_args$gender,
          status = old_args$status
        )
      ),
      relatives = list(
        relationship_filters = list(
          kind = old_args$relationship_kind,
          component = "pedigree1"
        )
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$family_survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$generate_query, !!!new_args)
    )
  })
})

describe("two disorders", {
  it("produces a valid query with only the requirements", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        )
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })
})

describe("two disorders with relatives", {
  it("produces a valid query with only the requirements", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        )
      ),
      relatives = list(
        relationship_filters = list(
          component = "pedigree1",
          kind = "PO"
        )
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })
})

describe("custom filters", {
  it("fails when filter is not a string", {
    expect_error(
      tte_retriever$generate_query(
        samples = list(
          diagnosis_filters = list(
            incl = list(
              icd_codes_regexp = "^78890"
            )
          ),
          individual_filters = list(
            custom = 1201
          )
        ),
        study_end_at = as.Date("2016-12-31")
      )
    )
  })

  it("produces a valid query with only the requirements", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        ),
        individual_filters = list(
          custom = "
             incl_diagnosed_at IS NULL
          OR excl_diagnosed_at IS NULL
          OR incl_diagnosed_at < excl_diagnosed_at
          "
        )
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })

  it("produces query that removes age of onset outliers", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          )
        ),
        individual_filters = list(
          custom = "
          incl_fail.failure_status != 1
          OR
          (     incl_fail.failure_status = 1
            AND incl_fail.failure_time >= 10
            AND incl_fail.failure_time <= 100
          )
          "
        )
      ),
      relatives = list(
        relationship_filters = list(
          kind = "PO"
        ),
        individual_filters = list(
          custom = "
          incl_fail.failure_status != 1
          OR
          (     incl_fail.failure_status = 1
            AND incl_fail.failure_time >= 10
            AND incl_fail.failure_time <= 100
          )
          "
        )
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })
})

describe("output columns", {
  it("only output requested columns", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("incl_diagnosed_at")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      as.data.table()

    # person_id
    # incl_failure_status
    # incl_failure_at
    # incl_failure_time
    # excl_failure_status
    # excl_failure_at
    # excl_failure_time
    # incl_diagnosed_at

    expect_equal(length(names(results)), 8)
  })

  it("regression: using relatives crashes", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890"
          ),
          excl = list(
            icd_codes_regexp = "^78809"
          )
        )
      ),
      relatives = list(
        relationship_filters = list(
          kind = "PO"
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("gender", "incl_diagnosed_at")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      as.data.table()

    # person_id
    # gender
    # incl_failure_status
    # incl_failure_at
    # incl_failure_time
    # excl_failure_status
    # excl_failure_at
    # excl_failure_time
    # incl_diagnosed_at
    # relatives
    # incl_affected_relatives
    # excl_affected_relatives

    expect_equal(length(names(results)), 12)
  })
})

describe("patient kind", {
  it("only retrieve medical records for the specific patient kind", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^78890",
            patient_kinds = list("inpatient-full-day", "inpatient-half-day")
          )
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("incl_diagnosed_at", "incl_record_patient_kind")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      as.data.table()

    expect_equal(length(names(results)), 6)
  })
})

describe("ICD editions", {
  it("only retrieves medical records with ICD-8 diagnoses", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^.*",
            icd_editions = list("icd8")
          )
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("incl_diagnosis_icd_id", "incl_diagnosis_icd_edition", "incl_failure_status", "incl_diagnosed_at", "incl_record_patient_kind")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      as.data.table()

    icd8_diagnoses <- results |> filter(incl_diagnosis_icd_edition == "icd8")
    expect_equal(nrow(icd8_diagnoses), 123)
  })

  it("only retrieves medical records with ICD-7 diagnoses", {
    query <- tte_retriever$generate_query(
      samples = list(
        diagnosis_filters = list(
          incl = list(
            icd_codes_regexp = "^.*",
            icd_editions = list("icd7"),
            record_origin = "cr"
          )
        )
      ),
      study_end_at = as.Date("2016-12-31"),
      extra_columns = list("incl_diagnosis_icd_id", "incl_diagnosis_icd_edition", "incl_failure_status", "incl_diagnosed_at", "incl_record_patient_kind")
    )

    tte_retriever$execute_query(output_path, query)
    results <- read_csv(csv_path, show_col_types = FALSE) |>
      as.data.table()

    # There are 2 icd7 diagnoses in the dummy data of ibp-registry.
    # One of the persons has a "annulled-cpr-number" as status, and should therefor be removed automatically.
    icd7_diagnoses <- results |> filter(incl_diagnosis_icd_edition == "icd7")
    expect_equal(nrow(icd7_diagnoses), 0)
  })
})
