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

message("- Generating d1_fs_tte")

d1_fs_tte <- generate_random_tte(n_count)
d1_fs_tte <- generate_failure(d1_fs_tte, 20, 10)
d1_fs_tte <- generate_diagnosed_relatives(d1_fs_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "FS") |>
  as.data.table()

message("- Generating d2_fs_tte")

d2_fs_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d2_fs_tte <- generate_failure(d2_fs_tte, 19, 11)
d2_fs_tte <- generate_diagnosed_relatives(d2_fs_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "FS") |>
  as.data.table()

message("- Generating d1_po_tte")

d1_po_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d1_po_tte <- generate_failure(d2_fs_tte, 20, 10)
d1_po_tte <- generate_diagnosed_relatives(d2_fs_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "PO") |>
  as.data.table()

message("- Generating d2_po_tte")

d2_po_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d2_po_tte <- generate_failure(d2_fs_tte, 19, 11)
d2_po_tte <- generate_diagnosed_relatives(d2_fs_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "PO") |>
  as.data.table()

message("- Joining")

tte <- rbindlist(list(d1_fs_tte, d2_fs_tte, d1_po_tte, d2_po_tte)) |> select(-born_at, -dead_at_year) |>
  arrange(person_id, disorder, relationship_kind) |>
  select(person_id, born_at_year, disorder, failure_status, failure_time, relationship_kind, relatives, relatives_diagnosed)

message("- Outputting results into ", output_path)

write_csv(tte, output_path)
