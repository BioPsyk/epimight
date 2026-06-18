#!/usr/bin/env Rscript
library(data.table)
library(dplyr)
library(dtplyr)
library(readr)
library(epimight)

study_period_start <- as.Date("1981-01-01")
study_period_end   <- as.Date("2016-12-31")

d1_earliest_onset_age <- 10
d2_earliest_onset_age <- 1

born_at_start <- as.Date("1981-01-01")
born_at_end   <- as.Date("1996-01-01")

relationship_kind <- "FS"

d1_tte <- read_csv("./tte_SCZ_FS.csv") |> as.data.table()
d2_tte <- read_csv("./tte_CAD_FS.csv") |> as.data.table()

d1_tte <- d1_tte |>
  filter(
    born_at >= born_at_start,
    born_at <= born_at_end
  )

d2_tte <- d2_tte |>
  filter(
    born_at >= born_at_start,
    born_at <= born_at_end
  )

d1_tte <- d1_tte |>
  mutate(
    born_at_year = as.character(format(born_at, "%Y"))
  )

d2_tte <- d2_tte |>
  mutate(
    born_at_year = as.character(format(born_at, "%Y"))
  )

print("Disorder 1 survival data:")
print(d1_tte)
print("Disorder 2 survival data:")
print(d2_tte)

c1_tte <- inner_join(d1_tte, d2_tte, by = join_by(person_id)) |>
  rename(
    born_at_year           = born_at_year.x,
    relatives              = relatives.x,
    d1_failure_status      = failure_status.x,
    d1_failure_time        = failure_time.x,
    d1_diagnosed_relatives = diagnosed_relatives.x,
    d2_failure_status      = failure_status.y,
    d2_failure_time        = failure_time.y,
    d2_diagnosed_relatives = diagnosed_relatives.y
  ) |>
  select(person_id, relatives, born_at_year, starts_with("d1_"), starts_with("d2_"))

c2_tte <- c1_tte |> filter(d1_diagnosed_relatives > 0)
c3_tte <- c1_tte |> filter(d2_diagnosed_relatives > 0)

print("c1_tte:")
print(c1_tte)
print("c2_tte:")
print(c2_tte)
print("c3_tte:")
print(c3_tte)

cif_analysis <- CumulativeIncidenceAnalysis$new()

run_cif_stratified <- function(disorder, cohort) {
  onset <- get(paste0(disorder, "_earliest_onset_age"))
  tte   <- get(paste0(cohort, "_tte")) |>
    rename(
      failure_status = !!as.name(paste0(disorder, "_failure_status")),
      failure_time   = !!as.name(paste0(disorder, "_failure_time"))
    )

  cif_analysis$run(
    tte = tte,
    earliest_onset = onset,
    stratify_columns = list("born_at_year")
  )
}

re_d1_c1 <- run_cif_stratified("d1", "c1")
re_d1_c2 <- run_cif_stratified("d1", "c2")
re_d1_c3 <- run_cif_stratified("d1", "c3")
re_d2_c1 <- run_cif_stratified("d2", "c1")
re_d2_c3 <- run_cif_stratified("d2", "c3")

print("re_d1_c1:")
print(re_d1_c1)
print("re_d1_c2:")
print(re_d1_c2)
print("re_d1_c3:")
print(re_d1_c3)
print("re_d2_c1:")
print(re_d2_c1)
print("re_d2_c3:")
print(re_d2_c3)

h2_analysis <- HeritabilityAnalysis$new()

run_h2_stratified <- function(c1_estimates, c2_estimates) {
  combined_estimates <- c1_estimates |>
    inner_join(c2_estimates, by = join_by(time, born_at_year)) |>
    rename(
      cohort1_estimates = estimate.x,
      cohort1_cases     = cases.x,
      cohort2_estimates = estimate.y,
      cohort2_cases     = cases.y
    ) |>
    select(time, born_at_year, starts_with("cohort"))

  h2_analysis$run(
    relationship_kind = relationship_kind,
    estimates         = combined_estimates
  )
}

h2_d1 <- run_h2_stratified(re_d1_c1, re_d1_c2)
h2_d2 <- run_h2_stratified(re_d2_c1, re_d2_c3)

print("h2_d1:")
print(h2_d1)
print("h2_d2:")
print(h2_d2)

re_combined <- inner_join(re_d1_c1, re_d1_c3, by = join_by(time, born_at_year)) |>
  rename(
    re_d1_c1_estimates = estimate.x,
    re_d1_c1_cases     = cases.x,
    re_d1_c3_estimates = estimate.y,
    re_d1_c3_cases     = cases.y,
  ) |>
  select(time, born_at_year, starts_with("re_")) |>
  inner_join(re_d2_c1, by = join_by(time, born_at_year)) |>
  rename(
    re_d2_c1_estimates = estimate,
    re_d2_c1_cases     = cases
  ) |>
  select(time, born_at_year, starts_with("re_"))

# Join both h2 estimates into a single estimate
h2_combined <- inner_join(h2_d1, h2_d2, by = join_by(time, born_at_year)) |>
  rename(
    h2_d1 = h2.x,
    h2_d2 = h2.y
  ) |>
  select(time, born_at_year, starts_with("h2_"))

# Join cumulative incidence and heritability estimates into a single dataset
combined <- inner_join(re_combined, h2_combined, by = join_by(time, born_at_year)) |>
  group_by(born_at_year) |>
  arrange(desc(time)) |>
  filter(row_number() == 1) |>
  arrange(born_at_year) |>
  as.data.table()

print(combined)

gc_analysis <- GeneticCorrelationAnalysis$new()

gc_d1_d2 <- gc_analysis$run(
  relationship_kind = relationship_kind,
  estimates         = combined
)

print(gc_d1_d2 |> select(born_at_year, rhh, rhog, se, l95, u95) |> arrange(born_at_year))

meta <- gc_analysis$run_meta(gc_d1_d2)

print(meta)
