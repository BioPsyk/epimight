#!/usr/bin/env Rscript
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(epimight, quietly = TRUE, warn.conflicts = FALSE)

tte <- read_csv(
  "./pipeline-tte.csv",
  show_col_type = FALSE,
  col_types = cols(person_id = col_character()),
) |> as.data.table()

pipeline <- Pipeline$new(pool = tte)

out <- pipeline$run_default_rg(
  heritability1 = list(
    trait          = "SCZ",
    relatives_kind = "parents",
    relatedness    = 0.5
  ),
  heritability2 = list(
    trait          = "CAD",
    relatives_kind = "half_siblings",
    relatedness    = 0.25
  ),
  stratify_columns = list("birth_year")
)

meta <- pipeline$run_meta(
  metadata = out$metadata,
  results  = out$results
)

meta |> mutate_if(is.numeric, round, 4) |> print()
