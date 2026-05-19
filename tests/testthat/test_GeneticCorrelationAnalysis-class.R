library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../legacy.R")
source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

cif_analysis <- CumulativeIncidenceAnalysis$new()
h2_analysis  <- HeritabilityAnalysis$new()
gc_analysis  <- GeneticCorrelationAnalysis$new()

set.seed(6)

d1_tte <- generate_random_tte(10000)
d1_tte <- generate_failure(d1_tte, 20, 10)
d1_tte <- generate_diagnosed_relatives(d1_tte, "diagnosed_relatives") |>
  as.data.table()

d2_tte <- d1_tte
d2_tte <- generate_failure(d2_tte, 25, 10)
d2_tte <- generate_diagnosed_relatives(d2_tte, "diagnosed_relatives") |>
  as.data.table()

d1_earliest_onset_age <- 10
d2_earliest_onset_age <- 1

relationship_kind <- "PO"

c1_tte <- inner_join(d1_tte, d2_tte, by = join_by(person_id)) |>
  rename(
    born_at_year           = born_at_year.x,
    relatives              = relatives.x,
    d1_failure_status      = failure_status.x,
    d1_failure_time        = failure_time.x,
    d1_diagnosed_relatives = diagnosed_relatives.x,
    d2_failure_status      = failure_status.y,
    d2_failure_time        = failure_time.y,
    d2_diagnosed_relatives = diagnosed_relatives.y
  ) |>
  select(person_id, relatives, born_at_year, starts_with("d1_"), starts_with("d2_"))

c2_tte <- c1_tte |> filter(d1_diagnosed_relatives > 0)
c3_tte <- c1_tte |> filter(d2_diagnosed_relatives > 0)

#=================================================================================
# Tests
#=================================================================================

get_last_time_stratified <- function(estimates) {
  estimates |>
    group_by(born_at_year) |>
    arrange(desc(time)) |>
    filter(row_number() == 1) |>
    arrange(born_at_year) |>
    as.data.table()
}

run_cif_stratified <- function(disorder, cohort) {
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
  combined_estimates <- c1_estimates |>
    inner_join(c2_estimates, by = join_by(time, born_at_year)) |>
    rename(
      cohort1_estimate = estimate.x,
      cohort1_cases    = cases.x,
      cohort2_estimate = estimate.y,
      cohort2_cases    = cases.y
    ) |>
    select(time, born_at_year, starts_with("cohort")) |>
    get_last_time_stratified()

  h2_analysis$run(
    relationship_kind = relationship_kind,
    estimates         = combined_estimates
  )
}

describe("run", {
  it("Produces same results as dk_rg_byYOB", {
    #=================================================================================
    # Old results
    #=================================================================================

    c1_tte_df <- inner_join(d1_tte, d2_tte, by = join_by(person_id)) |>
      rename(
        born_at                = born_at.x,
        relatives              = relatives.x,
        d1_failure_status      = failure_status.x,
        d1_failure_time        = failure_time.x,
        d1_diagnosed_relatives = diagnosed_relatives.x,
        d2_failure_status      = failure_status.y,
        d2_failure_time        = failure_time.y,
        d2_diagnosed_relatives = diagnosed_relatives.y
      ) |>
      select(person_id, relatives, born_at, starts_with("d1_"), starts_with("d2_")) |>
      as.data.frame()

    suppressWarnings({
      old_results <- dk_rg_byYOB(
        c1_tte_df,
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

    #=================================================================================
    # New results
    #=================================================================================

    re_d1_c1 <- run_cif_stratified("d1", "c1")
    re_d1_c2 <- run_cif_stratified("d1", "c2")
    re_d1_c3 <- run_cif_stratified("d1", "c3")
    re_d2_c1 <- run_cif_stratified("d2", "c1")
    re_d2_c3 <- run_cif_stratified("d2", "c3")

    h2_d1 <- run_h2_stratified(re_d1_c1, re_d1_c2)
    h2_d2 <- run_h2_stratified(re_d2_c1, re_d2_c3)

    re_d1_c1 <- get_last_time_stratified(re_d1_c1)
    re_d1_c3 <- get_last_time_stratified(re_d1_c3)
    re_d2_c1 <- get_last_time_stratified(re_d2_c1)

    re_combined <- inner_join(re_d1_c1, re_d1_c3, by = join_by(time, born_at_year)) |>
      rename(
        re_d1_c1_estimate = estimate.x,
        re_d1_c1_cases    = cases.x,
        re_d1_c3_estimate = estimate.y,
        re_d1_c3_cases    = cases.y,
      ) |>
      select(time, born_at_year, starts_with("re_")) |>
      inner_join(re_d2_c1, by = join_by(time, born_at_year)) |>
      rename(
        re_d2_c1_estimate = estimate,
        re_d2_c1_cases    = cases
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

    gc_d1_d2 <- gc_analysis$run(
      relationship_kind      = relationship_kind,
      estimates              = combined
    )

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

    old_results <- old_results |>
      arrange(desc(born_at_year))

    expect_dataframe_equal(old_results, new_results)
  })
})
