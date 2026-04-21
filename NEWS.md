# IbpRegistryRiskEstimations 1.0.0

## Changed

- `CumulativeIncidenceAnalysis-class`
  - `run` function argument `group_columns` renamed to `stratify_columns`
  - `run` function argument `earliest_onset` minimum value allowed changed `0` (was `1` before)
- `HeritabilityAnalysis-class`
  - `run` function argument `estimates$cohort1_estimate` renamed to `estimates$c1_cif`
  - `run` function argument `estimates$cohort1_cases` renamed to `estimates$c1_cif_cases`
  - `run` function argument `estimates$cohort2_estimate` renamed to `estimates$c2_cif`
  - `run` function argument `estimates$cohort2_cases` renamed to `estimates$c2_cif_cases`
- `GeneticCorrelationAnalysis-class`
  - `run` function argument `estimates$re_d1_c1_estimates` renamed to `estimates$d1_c1_cif`
  - `run` function argument `estimates$re_d1_c3_estimates` renamed to `estimates$d1_c3_cif`
  - `run` function argument `estimates$re_d2_c1_estimates` renamed to `estimates$d2_c1_cif`
  - `run` function argument `estimates$re_d1_c1_cases` renamed to `estimates$d1_c1_cif_cases`
  - `run` function argument `estimates$re_d1_c3_cases` renamed to `estimates$d1_c3_cif_cases`
  - `run` function argument `estimates$re_d2_c1_cases` renamed to `estimates$d2_c1_cif_cases`
  - `run` function argument `estimates$h2_d1` renamed to `estimates$d1_h2`
  - `run` function argument `estimates$h2_d2` renamed to `estimates$d2_h2`
  - `run` function argument `estimates` now require that all columns are specified

## Added

- `Pipeline-class` that takes care of data processing and running all analyses

# IbpRegistryRiskEstimations 0.6.0

## Fixed

- `TTERetriever-class`
  - When joining diagnoses with ICD-codes only `id` is used, when `id` AND `edition` should be used

# IbpRegistryRiskEstimations 0.5.0

## Changed

- `TTERetriever-class`
  - Adds `icd_edition` as argument to `diagnosis_filters`
  - Adds `cr` valid medical record register origin

# IbpRegistryRiskEstimations 0.4.1

## Changed

- `TTERetriever-class`
  - Writes arguments and query file before executing the query in the database

# IbpRegistryRiskEstimations 0.4.0

## Added

- `TTERetriever-class`
  - Patient kind filtering on diagnoses using the new argument `diagnosis_filters.{{disorder}}.patient_kind`

# IbpRegistryRiskEstimations 0.3.0

## Changed

- `TTERetriever-class`
  - Argument `output_columns` changed into `extra_columns`. Now you can only supply a list of extra columns
    to output, on top of the default ones: `person_id` + `_failure_status`, `_failure_time` and `_failure_at`
    for each disorder in `samples.diagnosis_filters`. When relatives are supplied, it always includes
    `relatives` and `_affected_relatives` for each disorder.

# IbpRegistryRiskEstimations 0.2.0

## Added

- `TTERetriever-class`
  - New argument `individual_filters.custom` (for samples and relatives) that can be used to
    provide raw SQL for advanced filtering scenarios.
  - New argument `output_columns` which can be used to select which columns to output.
    Defaults to `person_id` + `_failure_status` and `_failure_time` for each disorder
    in `samples.diagnosis_filters`. When relatives are supplied, it always includes
    `relatives` and `_affected_relatives` for each disorder.

# IbpRegistryRiskEstimations 0.1.0

## Added

- Re-implemented analyses
  - Cumulative Incidence
  - Genetic Correlation
  - Heritability Analysis
  - Meta analysis (random and fixed)
- Re-implemented data processors
  - TTE retriever
- Benchmarks
- Unit tests
- System tests

# IbpRegistryRiskEstimations 0.0.1

## Added

- Initial project files and infrastructure
