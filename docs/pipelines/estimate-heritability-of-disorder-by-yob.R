#!/usr/bin/env Rscript
library(data.table)
library(dplyr)
library(dtplyr)
library(readr)
library(IbpRiskEstimations)

study_period_start <- as.Date("1981-01-01")
study_period_end   <- as.Date("2016-12-31")

icd_codes_regexp <- "^F20|^295[^7]9"
earliest_onset_age <- 10

born_at_start <- as.Date("1981-01-01")
born_at_end   <- as.Date("1996-01-01")

relationship_kind <- "FS"

tte <- read_csv("./tte_SCZ_FS.csv") |> as.data.table()

tte <- tte |>
  filter(
    born_at >= born_at_start,
    born_at <= born_at_end
  )

print(tte)

cif_analysis <- CumulativeIncidenceAnalysis$new()

tte <- tte |>
  mutate(
    born_at_year = as.character(format(born_at, "%Y"))
  )

tte <- tte |>
  mutate(cohort = "all") |>
  union_all(
    tte |>
      filter(diagnosed_relatives > 0) |>
      mutate(cohort = "affected_relatives")
  )

print(tte)

estimates <- cif_analysis$run(
  tte = tte,
  earliest_onset_age = earliest_onset_age,
  group_columns = list("born_at_year", "cohort")
)

print(estimates)

cohort1 <- estimates |> filter(cohort == "all")
cohort2 <- estimates |> filter(cohort == "affected_relatives")

combined <- cohort1 |>
  inner_join(cohort2, by = join_by(time, born_at_year)) |>
  rename(
    cohort1_estimates = estimate.x,
    cohort1_cases     = cases.x,
    cohort2_estimates = estimate.y,
    cohort2_cases     = cases.y
  ) |>
  select(time, born_at_year, cohort1_estimates, cohort1_cases, cohort2_estimates, cohort2_cases)

combined <- combined |>
  group_by(born_at_year) |>
  arrange(desc(time)) |>
  filter(row_number() == 1) |>
  as.data.table()

print(combined |> arrange(born_at_year))

h2_analysis <- HeritabilityAnalysis$new()

results <- h2_analysis$run(
  relationship_kind = relationship_kind,
  estimates         = combined
)

print(results)

meta <- h2_analysis$run_meta(results)

print(meta)
