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

tte    <- as.data.frame(generate_random_tte(samples))
tte_dt <- data.table(tte)

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# run
#=================================================================================

old_analysis <- function() {
  results <- CIF_General_Population_Risk(tte, 1, 100)

  if (nrow(results) == 0) {
    stop("No results returned")
  }
}

new_analysis <- function() {
  results <- analysis$run(tte_dt)

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
