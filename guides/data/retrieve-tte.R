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

paths <- tte_retriever$run("scz_PO", tte_args)

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
    ),
    individual_filters = list( #  <---- here are the added filters
      gender = "female"        #
    )                          #
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_PO", tte_args)

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
    ),
    diagnosis_filters = list(           # <--- here are the added diagnosis filters
      scz = list(                       #
        icd_codes_regexp = "^F20|^295"  # We use the same definition for schizophrenia as the samples
      ),                                #
      bpd = list(                       #
        icd_codes_regexp = "^F31"       # But we also include bipolar disorder
      )                                 #
    )                                   #
  ),
  study_end_at = "2016-12-31"
)

paths <- tte_retriever$run("scz_PO", tte_args)

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
  study_end_at = "2016-12-31",
  extra_columns = list(
    "gender",
    "born_at",
    "father_id",
    "mother_id",
    "status",
    "status_changed",
    "birthplace_id",
    "scz_diagnosed_at",
    "scz_diagnosis_kind",
    "scz_diagnosis_icd_edition",
    "scz_diagnosis_icd_id",
    "scz_record_patient_kind"
  )
)

paths <- tte_retriever$run("scz_PO", tte_args)

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

tte_args <- list(
  samples = list(
    diagnosis_filters = list(
      scz = list(
        icd_codes_regexp = "^F20|^295"
      )
    ),
    individual_filters = list(                # <--- here's the custom filter
      custom = "dia.scz_icd_edition = 'icd8'" #
    )                                         #
  ),
  relatives = list(
    relationship_filters = list(
      kind = "PO"
    )
  ),
  study_end_at = "2016-12-31"
)
