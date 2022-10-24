options(warn = -1)

library(microbenchmark, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(dtplyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")
source("../legacy.R")

#=================================================================================
# Benchmarks
#=================================================================================

args        <- commandArgs(trailingOnly = TRUE)
samples     <- args[1]
iterations  <- args[2]
output_path <- args[3]

cat("  samples:", samples, "iterations:", iterations, "\n")

survival_data     <- as.data.table(generate_random_tte(samples))
relationship_kind <- "1C"
h2_analysis       <- HeritabilityAnalysis$new()
cif_analysis      <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# run
#=================================================================================

survival_data2 <- survival_data |>
  mutate(
    born_at_year = as.character(format(born_at, "%Y"))
  )

survival_data2 <- survival_data2 |>
  mutate(cohort = "all") |>
  union_all(
    survival_data2 |>
    filter(diagnosed_relatives > 0) |>
    mutate(cohort = "affected_relatives")
  )

estimates <- cif_analysis$run(survival_data2, list("born_at_year", "cohort"))

vec_analysis <- function() {
  cohort1 <- estimates |> filter(cohort == "all")
  cohort2 <- estimates |> filter(cohort == "affected_relatives")

  combined <- cohort1 |>
    inner_join(cohort2, by = join_by(time, born_at_year)) |>
    rename(
      cohort1_estimate = estimate.x,
      cohort1_cases    = cases.x,
      cohort2_estimate = estimate.y,
      cohort2_cases    = cases.y
    ) |>
    select(time, born_at_year, cohort1_estimate, cohort1_cases, cohort2_estimate, cohort2_cases) |>
    group_by(born_at_year) |>
    arrange(desc(time)) |>
    filter(row_number()==1)

  results1 <- h2_analysis$calculate_h2(
                            combined$cohort1_estimate,
                            combined$cohort2_estimate,
                            combined$cohort1_cases,
                            combined$cohort2_cases,
                            h2_analysis$relationship_coefficient_from_kind(relationship_kind)
                          )

  results1$born_at_year <- combined$born_at_year

  results1 <- results1 |> arrange(born_at_year)
}

non_vec_analysis <- function() {

  all_years <- estimates |> distinct(born_at_year) |> pull(born_at_year)

  results2    <- NULL
  missing_years <- c()

  for (year in all_years) {
    cohort1 <- estimates |>
      filter(born_at_year == year & cohort == "all")

    cohort2 <- estimates |>
      filter(born_at_year == year & cohort == "affected_relatives")

    if(nrow(cohort1) == 0 || nrow(cohort2) == 0) {
      missing_years <- c(missing_years, c(year))
      next
    }

    year_h2 <- h2_analysis$run(relationship_kind, cohort1, cohort2)

    if (!is.data.table(year_h2)) {
      missing_years <- c(missing_years, c(year))
      next
    }

    results2 <- rbind(results2, c(year, year_h2))
  }

  results2 <- data.table(results2) |>
    rename(
      born_at_year = V1
    )

  results2 <- results2 |> arrange(born_at_year)
}

results <- microbenchmark::microbenchmark(
  vec_analysis(),
  non_vec_analysis(),
  times = iterations
)

sumstats <- summary(results, unit = "s") |>
  mutate(n = samples) |>
  as.data.frame()

write_csv(sumstats, output_path, append=TRUE)
