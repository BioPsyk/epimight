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
