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

samples <- tte |> filter(disorder == "SCZ", relationship_kind == "PO") |> nrow()

pipeline <- Pipeline$new(pool = tte)

#=================================================================================
# Benchmarks
#=================================================================================

benchmarks <- list(
  "CIF, H2, RG" = function() {
    results <- pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO"
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "CIF, H2, RG (1 strat)" = function() {
    results <- pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO",
      stratify_columns = list("born_at_year")
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG" = function() {
    results <- pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO",
      use_weighted_cif = TRUE
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG (1 strat)" = function() {
    results <- pipeline$run(
      disorder1 = list(
        id = "SCZ"
      ),
      disorder2 = list(
        id = "CAD"
      ),
      relationship_kind = "PO",
      stratify_columns = list("born_at_year"),
      use_weighted_cif = TRUE
    )

    if (nrow(results$cif) == 0) {
      stop("No results returned")
    }
  }
)

results <- run_benchmark(samples, iterations, benchmarks)
plot_benchmark_results("Benchmark: Pipeline", samples, iterations, results, output_path)
