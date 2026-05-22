#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

raw_args    <- commandArgs()
project_dir <- NULL

for (arg in raw_args) {
  match       <- str_match(arg, "^--file=(.*)")
  script_path <- match[,2]

  if (is.na(script_path)) next

  project_dir <- normalizePath(file.path(getwd(), dirname(script_path)))
  break
}

args    <- commandArgs(trailinOnly = TRUE)
seed    <- args[1]
n_count <- args[2]

set.seed(seed)

d1_tte <- generate_random_tte(n_count)
d1_tte <- generate_failure(d1_tte, 20, 10)
d1_tte <- generate_diagnosed_relatives(d1_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "FS") |>
  as.data.table()

d2_tte <- copy(d1_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d2_tte <- generate_failure(d2_tte, 19, 11)
d2_tte <- generate_diagnosed_relatives(d2_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "FS") |>
  as.data.table()

d3_tte <- copy(d1_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d3_tte <- generate_failure(d2_tte, 20, 10)
d3_tte <- generate_diagnosed_relatives(d2_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "PO") |>
  as.data.table()

d4_tte <- copy(d1_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
d4_tte <- generate_failure(d2_tte, 19, 11)
d4_tte <- generate_diagnosed_relatives(d2_tte, "relatives_diagnosed") |>
  relocate(failure_time, .after = person_id) |>
  relocate(failure_status, .after = failure_time) |>
  relocate(relatives, .after = failure_status) |>
  relocate(relatives_diagnosed, .after = relatives) |>
  mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "PO") |>
  as.data.table()

tte <- rbindlist(list(d1_tte, d2_tte, d3_tte, d4_tte)) |> select(-born_at, -dead_at_year) |>
  arrange(person_id, disorder, relationship_kind) |>
  select(person_id, born_at_year, disorder, failure_status, failure_time, relationship_kind, relatives, relatives_diagnosed)

write_csv(tte, "./guides/data/pipeline-tte.csv")
