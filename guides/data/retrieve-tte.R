#!/usr/bin/env Rscript
library(data.table)
library(dplyr)
library(dtplyr)
library(epimight)

tte_retriever <- TTERetriever$new("./tmp", "localhost", "postgres", "devpass")

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_genpop", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      ),
      mdd = list(
        icd_codes_regexp = "^F3[23]"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_mdd_genpop", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    individual_filters = list(
      born_at_min = "1981-01-01",
      born_at_max = "1996-12-31"
    ),
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_millenials", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    individual_filters = list(
      born_at_min = "1981-01-01",
      born_at_max = "1996-12-31",
      gender = "female",
      status = list(
        "danish-resident",
        "danish-resident-special-address",
        "emigrated",
        "dead"
      )
    ),
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_millenials", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

for (kind in epimight:::diagnosis_kinds) {
  cat(sprintf("- ~%s~\n", kind))
}

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295",
        diagnosis_kinds = list("main", "auxiliary"),
        record_origin = "pcrr"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_genpop", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      pcrr_scz = list(
        icd_codes_regexp = "^F20|^295",
        record_origin = "pcrr"
      ),
      npr_scz = list(
        icd_codes_regexp = "^F20|^295",
        record_origin = "npr"
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_two_registers", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      main_scz = list(
        icd_codes_regexp = "^F20|^295",
        diagnosis_kinds = list("main")
      ),
      auxi_scz = list(
        icd_codes_regexp = "^F20|^295",
        diagnosis_kinds = list("auxiliary")
      )
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_two_kinds", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      )
    )
  ),
  relatives = list(
    relationship_filters = list(
      kind = "PO"
    )
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_FS", tte_args)

results <- read_csv(paths$data, show_col_types = FALSE, progress = FALSE) |>
  as.data.table()

print(results)
