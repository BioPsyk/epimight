#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(data.table)
library(epimight)

tte <- read_csv(
  "./test-tte.csv",
  show_col_type = FALSE,
  col_types = cols(person_id = col_character()),
) |> as.data.table()

pipeline <- Pipeline$new(pool = tte)

results <- pipeline$run(
  disorder1 = list(
    id             = "SCZ",
    earliest_onset = 1,
    latest_onset   = 100
  ),
  disorder2 = list(
    id             = "CAD",
    earliest_onset = 1,
    latest_onset   = 100
  ),
  relationship_kind = "FS",
  stratify_columns = list("born_at_year")
)

meta <- pipeline$run_meta(results)

print(meta$rg)
