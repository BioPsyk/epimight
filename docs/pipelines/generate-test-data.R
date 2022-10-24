#!/usr/bin/env Rscript

library(data.table)
library(dplyr)
library(dtplyr)
library(readr)

devtools::load_all(".")
source("./quality-assurance/utils.R")

n_count <- 100000

d1_tte <- generate_random_tte(n_count)
d1_tte <- generate_failure(d1_tte, 20, 10)
d1_tte <- generate_diagnosed_relatives(d1_tte, "diagnosed_relatives")
d1_tte <- generate_diagnosed_relatives(d1_tte, "diagnosed_relatives") |>
  relocate(relatives, .after = failure_time)

d2_tte <- d1_tte
d2_tte <- generate_failure(d2_tte, 25, 12)
d2_tte <- generate_diagnosed_relatives(d2_tte, "diagnosed_relatives")
d2_tte <- generate_failure(d2_tte, 25, 10)
d2_tte <- generate_diagnosed_relatives(d2_tte, "diagnosed_relatives") |>
  relocate(relatives, .after = failure_time)

unlink("./tmp/data", recursive = TRUE)
dir.create("./tmp/data")

write_csv(d1_tte, "./tmp/data/tte_SCZ_FS.csv", append = FALSE)
write_csv(d2_tte, "./tmp/data/tte_CAD_FS.csv", append = FALSE)
