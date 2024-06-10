#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

source("tests/legacy.R")
source("tests/utils.R")

#=================================================================================
# Constants
#=================================================================================

DISORDERS <- list(
  SUD = list("^F1[0-9]|^291[0-9]9|^29439|^303[0-9]9|^30320|^30328|^30390|^304[0-9]9", 10),
  MD  = list("^F3[0-9]|^296[0-79]9|^298[01]9|^300[14]9", 10),
  AD  = list("^F4[0-8]|^300[0-35-9]9|^305[0-9]9|^30568|^30799", 5),
  ED  = list("^F50|^30560|^30650|^30658|^30659", 1),
  PD  = list("^F60|^301[02-9]9|^3018[0-24]", 10),
  ID  = list("^F7[0-9]|^31[1-5]", 1),
  DD  = list("^F8[0-4]|^2990[0-3]", 1),
  BD  = list("^F9[0-8]|^306[0-9]9|^3080", 1)
)

#=================================================================================
# Helpers
#=================================================================================

log <- function(...) {
  timestamp <- paste0("[", format(Sys.time(), "%Y-%d-%m %H:%M:%S"), "]")

  cat(timestamp, ..., "\n")
}

#=================================================================================
# Old runners
#=================================================================================

calculate_old_gc <- function(relationship_kind, d1_key, d2_key, tte) {
  d1_earliest_onset_age <- DISORDERS[[d1_key]][[2]]
  d2_earliest_onset_age <- DISORDERS[[d2_key]][[2]]

  tte_df <- tte |>
    mutate(
      d1_failure_time = round(d1_failure_time),
      d2_failure_time = round(d2_failure_time)
    ) |>
    rename(
      d1_diagnosed_relatives = d1_affected_relatives,
      d2_diagnosed_relatives = d2_affected_relatives
    ) |>
    as.data.frame()

  suppressWarnings({
    results <- dk_rg_byYOB(
      tte_df,
      d1_earliest_onset_age,
      d2_earliest_onset_age,
      "a1",
      relationship_kind,
      "fixed"
    ) |>
      select(
        Year, rhh, se, L95, U95
      ) |>
      rename(
        born_at_year = Year,
        l95          = L95,
        u95          = U95
      ) |>
      filter_all(
        all_vars(!is.infinite(.) & !is.na(.))
      )
  })

  if (nrow(results) == 0) {
    stop("No genetic correlation results found")
  }

  results <- results |>
    arrange(desc(born_at_year))

  return(results)
}

#=================================================================================
# New runners
#=================================================================================

calculate_new_gc <- function(relationship_kind, d1_key, d2_key, tte) {
  cif_analysis <- CumulativeIncidenceAnalysis$new()
  h2_analysis  <- HeritabilityAnalysis$new()
  gc_analysis  <- GeneticCorrelationAnalysis$new()

  d1_earliest_onset_age <- DISORDERS[[d1_key]][[2]]
  d2_earliest_onset_age <- DISORDERS[[d2_key]][[2]]

  c1_tte <- tte |>
    mutate(
      born_at_year = as.numeric(format(born_at, "%Y")),
      d1_failure_time = round(d1_failure_time),
      d2_failure_time = round(d2_failure_time)
    )

  c2_tte <- c1_tte |> filter(d1_affected_relatives > 0)
  c3_tte <- c1_tte |> filter(d2_affected_relatives > 0)

  get_last_time_stratified <- function(estimates) {
    estimates |>
      group_by(born_at_year) |>
      arrange(desc(time)) |>
      filter(row_number() == 1) |>
      arrange(born_at_year) |>
      as.data.table()
  }

  run_cif_stratified <- function(disorder, cohort) {
    log("Running CIF on", disorder, " ", cohort)

    onset <- get(paste0(disorder, "_earliest_onset_age"))
    tte   <- get(paste0(cohort, "_tte")) |>
      rename(
        failure_status = !!as.name(paste0(disorder, "_failure_status")),
        failure_time   = !!as.name(paste0(disorder, "_failure_time"))
      )

    cif_analysis$run(
      tte = tte,
      earliest_onset = onset,
      group_columns = list("born_at_year")
    )
  }

  run_h2_stratified <- function(c1_estimates, c2_estimates) {
    if (nrow(c1_estimates) == 0) {
      stop("c1_estimates had 0 rows")
    }

    if (nrow(c2_estimates) == 0) {
      stop("c2_estimates had 0 rows")
    }

    combined_estimates <- c1_estimates |>
      inner_join(c2_estimates, by = join_by(time, born_at_year)) |>
      rename(
        cohort1_estimates = estimate.x,
        cohort1_cases     = cases.x,
        cohort2_estimates = estimate.y,
        cohort2_cases     = cases.y
      ) |>
      select(time, born_at_year, starts_with("cohort")) |>
      get_last_time_stratified()

    h2_analysis$run(
      relationship_kind = relationship_kind,
      estimates         = combined_estimates
    )
  }

  re_d1_c1 <- run_cif_stratified("d1", "c1")
  re_d1_c2 <- run_cif_stratified("d1", "c2")
  re_d1_c3 <- run_cif_stratified("d1", "c3")
  re_d2_c1 <- run_cif_stratified("d2", "c1")
  re_d2_c3 <- run_cif_stratified("d2", "c3")

  log("Running h2 on d1_c1 vs d1_c2")
  h2_d1 <- run_h2_stratified(re_d1_c1, re_d1_c2)
  log("Running h2 on d2_c1 vs d2_c3")
  h2_d2 <- run_h2_stratified(re_d2_c1, re_d2_c3)

  re_d1_c1 <- get_last_time_stratified(re_d1_c1)
  re_d1_c3 <- get_last_time_stratified(re_d1_c3)
  re_d2_c1 <- get_last_time_stratified(re_d2_c1)

  log("Combining all CIF and h2 results")
  re_combined <- inner_join(re_d1_c1, re_d1_c3, by = join_by(time, born_at_year)) |>
    rename(
      re_d1_c1_estimates = estimate.x,
      re_d1_c1_cases     = cases.x,
      re_d1_c3_estimates = estimate.y,
      re_d1_c3_cases     = cases.y,
    ) |>
    select(time, born_at_year, starts_with("re_")) |>
    inner_join(re_d2_c1, by = join_by(time, born_at_year)) |>
    rename(
      re_d2_c1_estimates = estimate,
      re_d2_c1_cases     = cases
    ) |>
    select(time, born_at_year, starts_with("re_"))

  h_combined <- inner_join(h2_d1, h2_d2, by = join_by(time, born_at_year)) |>
    rename(
      h2_d1 = h2.x,
      h2_d2 = h2.y
    ) |>
    select(time, born_at_year, starts_with("h2_"))

  combined <- inner_join(re_combined, h_combined, by = join_by(time, born_at_year)) |>
    as.data.table()

  log("Running GC analysis")
  gc_d1_d2 <- gc_analysis$run(
    relationship_kind      = relationship_kind,
    estimates              = combined
  )

  if (nrow(gc_d1_d2) == 0) {
    stop("No genetic correlation results found")
  }

  log("Running meta analysis")
  gc_meta <- gc_analysis$run_meta(gc_d1_d2)

  fixed_meta <- gc_meta |>
    select(starts_with("fixed_")) |>
    mutate(born_at_year = "Meta_fixed") |>
    rename(
      rhh = fixed_meta,
      se  = fixed_se,
      l95 = fixed_l95,
      u95 = fixed_u95
    )

  random_meta <- gc_meta |>
    select(starts_with("rand_")) |>
    mutate(born_at_year = "Meta_random") |>
    rename(
      rhh = rand_meta,
      se  = rand_se,
      l95 = rand_l95,
      u95 = rand_u95
    )

  new_results <- gc_d1_d2 |>
    select(born_at_year, rhh, se, l95, u95) |>
    rbind(fixed_meta) |>
    rbind(random_meta) |>
    arrange(desc(born_at_year)) |>
    filter_all(
      all_vars(!is.infinite(.) & !is.na(.))
    )

  return(new_results)
}

#=================================================================================
# Data processing
#=================================================================================

retrieve_tte <- function(relationship_kind, d1_key, d2_key) {
  d1_regexp <- DISORDERS[[d1_key]][[1]]
  d2_regexp <- DISORDERS[[d2_key]][[1]]

  output_prefix <- paste0(d1_key, "_", d2_key, "_", relationship_kind)
  csv_path      <- paste0(file.path(output_directory, output_prefix), ".csv")

  if (file.exists(csv_path)) {
    log("TTE already retrieved, skipping retrieval from database")
    return(csv_path)
  }

  tte_arguments <- list(
    samples = list(
      diagnosis_filters = list(
        d1 = list(
          icd_codes_regexp = d1_regexp,
          kinds = list("main", "auxiliary")
        ),
        d2 = list(
          icd_codes_regexp = d2_regexp,
          kinds = list("main", "auxiliary")
        )
      )
    ),
    relatives = list(
      relationship_filters = list(
        kind = relationship_kind
      )
    ),
    extra_columns = list("born_at"),
    study_end_at = "2016-12-31"
  )

  tte_retriever$run(output_prefix, tte_arguments)

  return(csv_path)
}

#=================================================================================
# Constants
#=================================================================================

args <- commandArgs(trailingOnly=TRUE)

output_directory  <- args[[1]]
relationship_kind <- args[[2]]

tte_retriever <- TTERetriever$new(output_directory, "localhost", NULL, NULL)

log("Storing comparisons in output directory:", output_directory)

for (d1_key in names(DISORDERS)) {
  for (d2_key in names(DISORDERS)) {
    if (d1_key == d2_key) {
      next
    }

    log("Retrieving TTE from database")
    csv_path <- retrieve_tte(relationship_kind, d1_key, d2_key)

    log("Reading TTE to memory")
    tte <- read_csv(csv_path) |> as.data.table()

    log("Starting to calculate old GC")
    old_gc <- calculate_old_gc(relationship_kind, d1_key, d2_key, tte)

    log("Starting to calculate new GC")
    new_gc <- calculate_new_gc(relationship_kind, d1_key, d2_key, tte)

    log("Comparin gc results")
    expect_dataframe_equal(old_gc, new_gc)
  }
}
