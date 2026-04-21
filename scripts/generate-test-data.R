#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

raw_args    <- commandArgs()
project_dir <- NULL

for (arg in raw_args) {
  match       <- str_match(arg, "^--file=(.*)")
  script_path <- match[,2]

  if (is.na(script_path)) next

  scripts_dir <- normalizePath(file.path(getwd(), dirname(script_path)))
  project_dir <- dirname(scripts_dir)
  break
}

setwd(project_dir)
source("./tests/utils.R")

args        <- commandArgs(trailingOnly = TRUE)
seed        <- args[1]
n_count     <- args[2]
output_path <- args[3]

set.seed(seed)

message("Generating test data using seed ", seed, " for ", n_count, " individuals")

tte <- generate_pipeline_tte(n_count)

message("- Outputting results into ", output_path)

write_csv(tte, output_path)
