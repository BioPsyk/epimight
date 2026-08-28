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
) |> as.data.table()

samples <- tte |> filter(trait == "SCZ", relatives_kind == "parents") |> nrow()

pipeline <- Pipeline$new(pool = tte)

#=================================================================================
# Benchmarks
#=================================================================================

benchmarks <- list(
  "CIF, H2, RG" = function() {
    results <- pipeline$run(
      heritability1 = list(
        index_trait    = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5,
      ),
      heritability2 = list(
        index_trait    = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25,
      )
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "CIF, H2, RG (1 strat)" = function() {
    results <- pipeline$run(
      heritability1 = list(
        index_trait    = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5,
      ),
      heritability2 = list(
        index_trait    = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25,
      ),
      stratify_columns = list("birth_year")
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG" = function() {
    results <- pipeline$run(
      heritability1 = list(
        index_trait    = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5,
      ),
      heritability2 = list(
        index_trait    = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25,
      ),
      use_weighted_cif = TRUE
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG (1 strat)" = function() {
    results <- pipeline$run(
      heritability1 = list(
        index_trait    = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5,
      ),
      heritability2 = list(
        index_trait    = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25,
      ),
      stratify_columns = list("birth_year"),
      use_weighted_cif = TRUE
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  }
)

results <- run_benchmark(samples, iterations, benchmarks)
plot_benchmark_results("Benchmark: Pipeline", samples, iterations, results, output_path)
