library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")
source("../legacy.R")

#=================================================================================
# Preparation
#=================================================================================

tte_retriever <- TTERetriever$new()
qg            <- QueryGenerator$new()

#=================================================================================
# Tests
#=================================================================================

output_path <- "../../tmp/test_output.csv"

compare_queries <- function(old_query, new_query) {
  tte_retriever$execute_query(old_query, output_path, "localhost", "postgres", "devpass")
  old_results <- read_csv(output_path, show_col_types = FALSE) |>
    arrange(person_id) |>
    as.data.frame()

  tte_retriever$execute_query(new_query, output_path, "localhost", "postgres", "devpass")
  new_results <- read_csv(output_path, show_col_types = FALSE) |>
    arrange(person_id) |>
    as.data.frame()

  expect_dataframe_equal(old_results, new_results, c("diagnosed_at"))
}

describe("single_disorder", {
  it("produces a valid query with only the requirements", {
    old_args <- list(
      icd_codes_regexp = "^80",
      study_end_at = "2016-12-31"
    )

    new_args <- list(
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder, !!!new_args)
    )
  })

  it("produces a valid query with all sample_filters", {
    old_args <- list(
      icd_codes_regexp = "^80",
      birth_date_min = as.Date("1800-01-01"),
      birth_date_max = as.Date("2010-01-01"),
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
      earliest_onset = 1,
      latest_onset = 100,
      study_end_at = as.Date("2016-04-01")
    )

    new_args <- list(
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp
      ),
      sample_filters = list(
        born_at_min = as.Date(old_args$birth_date_min),
        born_at_max = as.Date(old_args$birth_date_max),
        gender = old_args$gender,
        status = old_args$status,
        diagnosis_earliest_onset = old_args$earliest_onset,
        diagnosis_latest_onset = old_args$latest_onset
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder, !!!new_args)
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
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp,
        diagnosis_kind = old_args$diagnosis_kind,
        record_origin = old_args$record_origin
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder, !!!new_args)
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
      earliest_onset = 1,
      latest_onset = 100,
      study_end_at = "2016-04-01"
    )

    new_args <- list(
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp,
        diagnosis_kind = old_args$diagnosis_kind
      ),
      sample_filters = list(
        born_at_min = as.Date(old_args$birth_date_min),
        born_at_max = as.Date(old_args$birth_date_max),
        gender = old_args$gender,
        status = old_args$status,
        diagnosis_earliest_onset = old_args$earliest_onset,
        diagnosis_latest_onset = old_args$latest_onset
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder, !!!new_args)
    )
  })
})

describe("single_disorder_with_relatives", {
  it("produces a valid query with only the requirements", {
    old_args <- list(
      icd_codes_regexp = ".*",
      study_end_at = "2016-12-31",
      relationship_kind = "PO"
    )

    new_args <- list(
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp
      ),
      relationship_filters = list(
        component = "pedigree1",
        kind = old_args$relationship_kind
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$family_survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder_with_relatives, !!!new_args)
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
      earliest_onset = 1,
      latest_onset = 100,
      study_end_at = "2016-04-01",
      relationship_kind = "PO"
    )

    new_args <- list(
      diagnosis_filters = list(
        icd_codes_regexp = old_args$icd_codes_regexp,
        diagnosis_kind = old_args$diagnosis_kind
      ),
      sample_filters = list(
        born_at_min = as.Date(old_args$birth_date_min),
        born_at_max = as.Date(old_args$birth_date_max),
        gender = old_args$gender,
        status = old_args$status,
        diagnosis_earliest_onset = old_args$earliest_onset,
        diagnosis_latest_onset = old_args$latest_onset
      ),
      relationship_filters = list(
        component = "pedigree1",
        kind = old_args$relationship_kind
      ),
      study_end_at = as.Date(old_args$study_end_at)
    )

    compare_queries(
      rlang::exec(qg$family_survival_by_icd_codes, !!!old_args),
      rlang::exec(tte_retriever$single_disorder_with_relatives, !!!new_args)
    )
  })
})

describe("two_disorders_exclusive", {
  it("produces a valid query with only the requirements", {
    query <- tte_retriever$two_disorders_exclusion(
      diagnosis_filters = list(
        icd_codes_regexp = "^78890"
      ),
      exclusion_diagnosis_filters = list(
        icd_codes_regexp = "^78809"
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(query, output_path, "localhost", "postgres", "devpass")
    results <- read_csv(output_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })
})

describe("two_disorders_exclusive_with_relatives", {
  it("produces a valid query with only the requirements", {
    query <- tte_retriever$two_disorders_exclusion_with_relatives(
      diagnosis_filters = list(
        icd_codes_regexp = "^78890"
      ),
      exclusion_diagnosis_filters = list(
        icd_codes_regexp = "^78809"
      ),
      relationship_filters = list(
        component = "pedigree1",
        kind = "PO"
      ),
      study_end_at = as.Date("2016-12-31")
    )

    tte_retriever$execute_query(query, output_path, "localhost", "postgres", "devpass")
    results <- read_csv(output_path, show_col_types = FALSE) |>
      arrange(person_id) |>
      as.data.frame()

    expect_gte(nrow(results), 78)
  })
})
