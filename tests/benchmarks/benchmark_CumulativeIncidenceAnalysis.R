options(warn = -1)

library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(dtplyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

devtools::load_all(".")
source("../utils.R")

#=================================================================================
# Setup
#=================================================================================


args        <- commandArgs(trailingOnly = TRUE)
samples     <- args[1]
iterations  <- args[2]
cache_dir   <- args[3]
output_path <- args[4]

tte <- read_csv(
  "../data/pipeline-tte.csv",
  show_col_type = FALSE,
  col_types = cols(person_id = col_character()),
) |>
  mutate(
    weight = ifelse(relatives_diagnosed > 0.0, relatives_diagnosed / relatives, 0.0)
  ) |>
  filter(
    disorder          == "SCZ",
    relationship_kind == "PO"
  ) |>
  as.data.table()

samples <- tte |> nrow()

analysis <- CumulativeIncidenceAnalysis$new()

#=================================================================================
# Benchmarks
#=================================================================================

benchmarks <- list(
  "CIF" = function() {
    results <- analysis$run(tte = tte |> select(-weight))

    if (nrow(results) == 0) {
      stop("No results returned")
    }
  },
  "CIF (1 strat)" = function() {
    results <- analysis$run(
      tte              = tte |> select(-weight),
      stratify_columns = list("born_at_year")
    )

    if (nrow(results) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF" = function() {
    results <- analysis$run(tte = tte)

    if (nrow(results) == 0) {
      stop("No results returned")
    } else {

    }
  },
  "weighted CIF (1 strat)" = function() {
    results <- analysis$run(
      tte              = tte,
      stratify_columns = list("born_at_year")
    )

    if (nrow(results) == 0) {
      stop("No results returned")
    }
  }
)

results <- run_benchmark(samples, iterations, benchmarks)
plot_benchmark_results("Benchmark: Cumulative incidence", samples, iterations, results, output_path)
