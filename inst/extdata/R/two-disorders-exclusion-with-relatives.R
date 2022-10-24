#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

args <- commandArgs(trailingOnly=TRUE)

d1_icd_code_regexp <- args[1]
d2_icd_code_regexp <- args[2]
relationship_kind  <- args[3]

output_path <- paste0(file.path(getwd(), "tmp/test_output.csv"))

if (file.exists(output_path)) {
  query_file <- paste0(output_path, ".sql")

  file.remove(output_path)
  file.remove(query_file)
}

tte_retriever <- TTERetriever$new()

query <- tte_retriever$two_disorders_exclusion_with_relatives(
  diagnosis_filters = list(
    icd_codes_regexp = d1_icd_code_regexp
  ),
  exclusion_diagnosis_filters = list(
    icd_codes_regexp = d2_icd_code_regexp
  ),
  relationship_filters = list(
    component = "pedigree1",
    kind = relationship_kind
  ),
  study_end_at = as.Date("2016-12-31")
)

tte_retriever$execute_query(query, output_path, "localhost", "postgres", "devpass")
