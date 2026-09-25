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

dataset_path <- paste0("../../tmp/benchmark_tte_", samples, ".csv")

if (!file.exists(dataset_path)) {
  message("Benchmark TTE was not found, generating")
  tte <- generate_pipeline_tte(samples)
  write_csv(tte, dataset_path)
  message("TTE generated")
} else {
  tte <- read_csv(
    dataset_path,
    show_col_type = FALSE,
    col_types = cols(person_id = col_character())
  ) |> as.data.table()
}

pipeline <- Pipeline$new(pool = tte)

#=================================================================================
# Benchmarks
#=================================================================================

benchmarks <- list(
  "CIF, H2, RG" = function() {
    pipeline$clear_results()

    out <- pipeline$run_default_rg(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25
      ),
      use_weighted_cif = FALSE
    )

    if (nrow(out$results) == 0) {
      stop("No results returned")
    }
  },
  "CIF, H2, RG (1 strat)" = function() {
    pipeline$clear_results()

    out <- pipeline$run_default_rg(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25
      ),
      stratify_columns = list("birth_year"),
      use_weighted_cif = FALSE
    )

    if (nrow(out$results) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG" = function() {
    pipeline$clear_results()

    out <- pipeline$run_default_rg(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25
      ),
      use_weighted_cif = TRUE
    )

    if (nrow(out$results) == 0) {
      stop("No results returned")
    }
  },
  "weighted CIF, H2, RG (1 strat)" = function() {
    pipeline$clear_results()

    out <- pipeline$run_default_rg(
      heritability1 = list(
        trait          = "SCZ",
        relatives_kind = "parents",
        relatedness    = 0.5
      ),
      heritability2 = list(
        trait          = "CAD",
        relatives_kind = "half_siblings",
        relatedness    = 0.25
      ),
      stratify_columns = list("birth_year"),
      use_weighted_cif = TRUE
    )

    if (nrow(out$results) == 0) {
      stop("No results returned")
    }
  }
)

out <- run_benchmark(samples, iterations, benchmarks)
plot_benchmark_results("Benchmark: Pipeline", samples, iterations, out, output_path)
