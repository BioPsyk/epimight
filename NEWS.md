# epimight 2.0.0

With this release the following items has changes that are not backwards-compatible:

- Terminology
- Time-to-event data format
- Pipeline interface

## Terminology

In earlier versions the terminology was centered around disorders and failure status/time.
Now the terminology is more generic and is centered around "traits", since `epimight` can
be used for any type of trait, not only disorders/diseases. This is reflected in source code,
documentation and data.

## Time-to-event data format

In the previous version, you used the following time-to-event data format:

```
| person_id | disorder | failure_status | failure_time | relationship_kind | relatives | relatives_diagnosed |
|-----------+----------+----------------+--------------+-------------------+-----------+---------------------|
|     99996 | CAD      |              2 |           69 | FS                |         5 |                   0 |
|     99996 | CAD      |              2 |           69 | PO                |         5 |                   2 |
|     99996 | SCZ      |              2 |           69 | FS                |         5 |                   0 |
|     99996 | SCZ      |              2 |           69 | PO                |         5 |                   0 |
```

Now you use this format instead:

```
| person_id | trait | trait_status | trait_age | relatives_kind | relatives_n | relatives_n_trait |
|-----------+-------+--------------+-----------+----------------+-------------+-------------------|
|     99997 | SCZ   |            0 |        34 | half_siblings  |           2 |                 1 |
|     99997 | SCZ   |            0 |        34 | parents        |           2 |                 0 |
|     99998 | CAD   |            2 |        67 | half_siblings  |           4 |                 0 |
|     99998 | CAD   |            2 |        67 | parents        |           4 |                 1 |
```

You can use this code to change the column names into the new format:

```R
tte <- tte |>
  rename(
    trait             = disorder,
    trait_status      = failure_status,
    trait_age         = failure_time,
    relatives_kind    = relationship_kind,
    relatives_n       = relatives,
    relatives_n_trait = relatives_diagnosed
  )
```

Before `relationship_kind` referred to the **relationship** between the proband and relative (parent/offspring, cousins, etc.)
That's a problem, because it doesn't say whether the relative is the "parent" or the "offspring" in a "parent/offspring" relationship.

To avoid that ambiguity, you now specify the **relative** kind explicitly (parent, cousing, etc.)

For more details on the new format see: [Time-to-event input format](/guides/pipeline/guide.org#time-to-event-input-format)

## Pipeline interface

In the previous version, you used the `run` function produce the genetic correlations and the `run_meta`
function to meta-analyze the results:

```R
pipeline <- Pipeline$new(pool = tte_data)

results <- pipeline$run(
  disorder1 = list(
    id             = "SCZ",
    earliest_onset = 1,
    latest_onset   = 100
  ),
  disorder2 = list(
    id             = "CAD",
    earliest_onset = 1,
    latest_onset   = 100
  ),
  relationship_kind = "FS",
  stratify_columns = list("born_at_year")
)

meta_out <- pipeline$run_meta(results)
```

The problem with this interface was that:

- There wasn't anyway to only run `cif` or `h2`, you had to run everything
- Intermediate results weren't cached or reused
- You could not finely control how the intermediate results were used in the genetic correlation

Now, instead:

- You can run each analysis (`cif`, `h2`, `rg`) independently
- All results are cached/reused in the `Pipeline` instance
- You can either:
  - Use the simple interface, which produces genetic correlation as the previous version
  - Use the advanced interface, which allows you to produce genetic correlations exactly as you want

The example above for the previous version, would look like this with the new simple interface:

```R
pipeline <- Pipeline$new(pool = tte_data)

out <- pipeline$run_default_rg(
  heritability1 = list(
    trait          = "SCZ",
    relatives_kind = "full_siblings",
    relatedness    = 0.5
  ),
  heritability2 = list(
    trait          = "CAD",
    relatives_kind = "full_siblings",
    relatedness    = 0.5
  ),
  stratify_columns = list("birth_year")
)

meta <- pipeline$run_meta(
  metadata = out$metadata,
  results  = out$results
)

# To avoid having to type out each argument, you can call the function like this:
meta <- do.call(pipeline$run_meta, out)
```

The advanced interface and results caching described in the [pipeline guide](./guides/pipeline/guide.org).

# epimight 1.0.2

## Fixed

- `Pipeline-class$run` weighted CIF method uses weights for wrong set of relatives

# epimight 1.0.1

## Changed

- `Pipeline-class$run` now defaults to using weighted CIF method

# epimight 1.0.0

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

# epimight 0.6.0

## Fixed

- `TTERetriever-class`
  - When joining diagnoses with ICD-codes only `id` is used, when `id` AND `edition` should be used

# epimight 0.5.0

## Changed

- `TTERetriever-class`
  - Adds `icd_edition` as argument to `diagnosis_filters`
  - Adds `cr` valid medical record register origin

# epimight 0.4.1

## Changed

- `TTERetriever-class`
  - Writes arguments and query file before executing the query in the database

# epimight 0.4.0

## Added

- `TTERetriever-class`
  - Patient kind filtering on diagnoses using the new argument `diagnosis_filters.{{disorder}}.patient_kind`

# epimight 0.3.0

## Changed

- `TTERetriever-class`
  - Argument `output_columns` changed into `extra_columns`. Now you can only supply a list of extra columns
    to output, on top of the default ones: `person_id` + `_failure_status`, `_failure_time` and `_failure_at`
    for each disorder in `samples.diagnosis_filters`. When relatives are supplied, it always includes
    `relatives` and `_affected_relatives` for each disorder.

# epimight 0.2.0

## Added

- `TTERetriever-class`
  - New argument `individual_filters.custom` (for samples and relatives) that can be used to
    provide raw SQL for advanced filtering scenarios.
  - New argument `output_columns` which can be used to select which columns to output.
    Defaults to `person_id` + `_failure_status` and `_failure_time` for each disorder
    in `samples.diagnosis_filters`. When relatives are supplied, it always includes
    `relatives` and `_affected_relatives` for each disorder.

# epimight 0.1.0

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

# epimight 0.0.1

## Added

- Initial project files and infrastructure
