#!/usr/bin/env Rscript
library(data.table)
library(dplyr)
library(dtplyr)
library(readr)
library(IbpRiskEstimations)

# Arguments for research question:
# What is the lifetime risk of schizophrenia in millenials (people born from 1981 to 1996)?

study_period_start <- as.Date("1981-01-01")
study_period_end   <- as.Date("2016-12-31")

earliest_onset_age <- 10

born_at_start <- as.Date("1981-01-01")
born_at_end   <- as.Date("1996-01-01")

tte <- read_csv("./tte_SCZ.csv") |> as.data.table()

tte <- tte |>
  filter(
    born_at >= born_at_start,
    born_at <= born_at_end
  )

print(tte)

cif_analysis <- CumulativeIncidenceAnalysis$new()

estimates <- cif_analysis$run(
  tte = tte,
  earliest_onset_age=earliest_onset_age
)

print(estimates)
