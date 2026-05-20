options(warn = -1)

library(microbenchmark, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(dtplyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Setup
#=================================================================================

args        <- commandArgs(trailingOnly = TRUE)
samples     <- args[1]
iterations  <- args[2]
output_path <- args[3]

cat("  samples:", samples, "iterations:", iterations, "\n")

tte <- as.data.frame(generate_random_tte(samples))

make_tte_groups <- function() {
  tte_year <- tte |>
    mutate(
      born_at_year = as.character(format(born_at, "%Y"))
    )

  tte_year_dt    <- data.table(tte_year)
  all_group      <- copy(tte_year_dt)[, diagnosed_relatives := as.character(diagnosed_relatives)]
  any_group      <- copy(all_group)[diagnosed_relatives > 0][, diagnosed_relatives := "Any"]
  none_group     <- copy(all_group)[relatives == 0,][, diagnosed_relatives := "NoFamilyMembers"]
  results        <- funion(all_group, any_group, all = TRUE)
  results        <- funion(results, none_group, all = TRUE)
  all_born_group <- copy(results)[, born_at_year := "all"]
  results        <- funion(results, all_born_group, all = TRUE)

  return(results)
}

rename_group_results <- function(results) {
  results <- results |>
    select(-cases) |>
    relocate(diagnosed_relatives, .after = variance) |>
    arrange(
      match(
        diagnosed_relatives,
        c("Any", "NoFamilyMembers", as.character(c(0:20)))
      ),
      match(
        born_at_year,
        c("all", as.character(c(0:2023)))
      )
    ) |>
    rename(
      "N Affected Family Members" = diagnosed_relatives,
      "Year"                      = born_at_year
    ) |>
    rename_with(capitalize)

  return(results)
}

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Benchmarks
#=================================================================================

old_analysis <- function() {
  results <- CumulativeIncidence_familial_withinDisorder_byYOB(
    tte,
    earliest_onset=1,
    latest_onset=100,
    nFamMember="a1"
  )

  if (nrow(results) == 0) {
    stop("No results returned")
  }
}

new_analysis <- function() {
  group_columns <- c(
    "born_at_year",
    "diagnosed_relatives"
  )

  group_tte <- make_tte_groups()

  results <- analysis$run(group_tte, group_columns) |>
    rename_group_results() |>
    as.data.frame()

  if (nrow(results) == 0) {
    stop("No results returned")
  }
}

results <- microbenchmark::microbenchmark(
  old_analysis(),
  new_analysis(),
  times = iterations
)

sumstats <- summary(results, unit = "s") |>
  mutate(n = samples) |>
  as.data.frame()

write_csv(sumstats, output_path, append=TRUE)
