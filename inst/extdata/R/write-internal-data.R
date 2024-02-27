#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

args <- commandArgs(trailingOnly=TRUE)

constants_path <- args[1]
constants      <- rjson::fromJSON(file = constants_path)

tte_retriever_rules_path <- args[2]
tte_retriever_rules      <- rjson::fromJSON(file = tte_retriever_rules_path)

relationship_kinds          <- constants$relationship_kinds
vertical_relationship_kinds <- constants$vertical_relationship_kinds
civil_statuses              <- constants$civil_statuses
diagnosis_kinds             <- constants$diagnosis_kinds
genders                     <- constants$genders

usethis::use_data(
  relationship_kinds,
  vertical_relationship_kinds,
  civil_statuses,
  diagnosis_kinds,
  genders,
  tte_retriever_rules,
  internal  = TRUE,
  overwrite = TRUE
)
