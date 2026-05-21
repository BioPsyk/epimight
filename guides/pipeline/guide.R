#!/usr/bin/env Rscript
library(epimight)

pipeline <- Pipeline$new(pool = tte)

trail1 <- pipeline$run_trail(
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
  draws = 2,
  group_columns = list("born_at_year", "gender")
)
