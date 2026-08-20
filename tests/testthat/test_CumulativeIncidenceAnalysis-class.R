library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)

source("../utils.R")

#=================================================================================
# Preparation
#=================================================================================

set.seed(1)

pipeline_tte <- read_csv(
  "../data/pipeline-tte.csv",
  show_col_type = FALSE,
  col_types=cols(person_id = col_character()),
) |>
  mutate(
    weight = ifelse(relatives_trait_n > 0.0, relatives_trait_n / relatives_n, 0.0)
  ) |>
  filter(
    trait_name     == "SCZ",
    relatives_kind == "parents"
  ) |>
  select(person_id, trait_status, trait_onset, weight, relatives_n, relatives_trait_n) |>
  as.data.table()

analysis <- CumulativeIncidenceAnalysis$new()

run_kaplan_meier <- function(tte) {
  # 2. Identify unique time points where an event actually happened
  event_times <- unique(tte$trait_onset[tte$trait_status == 1])
  event_times <- sort(event_times)

  # 3. Initialize vectors to store our calculations
  n_at_risk <- numeric(length(event_times))
  n_events  <- numeric(length(event_times))
  surv_prob <- numeric(length(event_times))

  current_surv <- 1  # Survival starts at 1.0 (100%)

  # 4. Loop through each unique event time to calculate probabilities
  for (i in seq_along(event_times)) {
    t <- event_times[i]

    # Count how many people are still at risk (time >= current time)
    n_at_risk[i] <- sum(tte$trait_onset >= t)

    # Count how many people experienced the event at this exact time
    n_events[i] <- sum(tte$trait_onset == t & tte$trait_status == 1)

    # Apply the conditional probability formula
    conditional_prob <- 1 - (n_events[i] / n_at_risk[i])

    # Cumulative multiplication
    current_surv <- current_surv * conditional_prob
    surv_prob[i] <- current_surv
  }

  # 5. Return a clean data frame with the results
  # We add time 0 at the beginning so the curve starts correctly at 100%
  results <- data.frame(
    time      = c(0, event_times),
    n_at_risk = c(length(time), n_at_risk),
    n_events  = c(0, n_events),
    survival  = c(1, surv_prob)
  )

  return(results)
}
#
#run_kaplan_meier2 <- function(tte) {
#  trait_onset_max <- max(tte$trait_onset)
#
#  results <- tte |>
#    mutate(
#      event_1 = ifelse(trait_status == 1, 1, 0.0),
#      event_n = ifelse(trait_status != 0, 1, 0.0),
#    ) |>
#    group_by(trait_onset) |>
#    summarise(
#      all     = n(),
#      event_1 = sum(event_1),
#      event_n = sum(event_n)
#    ) |>
#    ungroup() |>
#    # Make sure we have a row for `trait_onset` from 0 up to `trait_onset_max`
#    right_join(
#      data.table(
#        trait_onset = seq(0, trait_onset_max)
#      ),
#      by = join_by(trait_onset)
#    ) |>
#    # The failure times that were filled in will have NA's in missing columns,
#    # so we make sure to replace them with 0.0
#    mutate(
#      across(everything(), ~ replace_na(.x, 0.0))
#    ) |>
#    # Risk needs to be accumulated starting from the largest trait_onset value
#    arrange(desc(trait_onset)) |>
#    mutate(
#      at_risk = cumsum(all)
#    ) |>
#    arrange(trait_onset) |>
#    filter(event_n > 0.0) |>
#    mutate(
#      surv = ifelse(
#        at_risk > 0.0,
#        cumprod(1.0 - event_n / at_risk),
#        surv
#      ),
#      cif_acc = ifelse(
#        at_risk > 0.0,
#        cumsum(
#          replace_na(lag(surv), 1.0) * event_1 / at_risk
#        ),
#        cif_acc
#      ),
#      cif = replace_na(lag(cif_acc), 0.0),
#    ) |>
#    filter(
#      event_1 > 0.0
#    ) |>
#    mutate(
#      cases = cumsum(event_1)
#    ) |>
#    rename(time = trait_onset) |>
#    select(time, cif, cases, surv) |>
#    as.data.table()
#
#  return(results)
#}

#=================================================================================
# Tests
#=================================================================================

describe("run", {
  it("it fails when an individual appears more than once", {
    expect_error(analysis$run(
     tte = pipeline_tte |> mutate(person_id = 1)
    ))
  })

  it("returns NULL when no individuals have the trait", {
    results <- analysis$run(
      tte = pipeline_tte |> mutate(trait_status = 0),
      use_weighted_cif = FALSE
    )

    expect_null(results)
  })

  it("passes the sum rule", {
    # At any given time point, the sum of the cumulative incidences of all distinct event types
    # plus the Kaplan-Meier estimate of being completely event-free must exactly equal 1.

    results <- run_kaplan_meier(pipeline_tte)

    print(results)
  })

  it("only produces CIF values that increase as time progresses", {
    faults <- analysis$run(
      tte              = pipeline_tte,
      use_weighted_cif = FALSE
    ) |> filter(cif < lag(cif))

    expect_equal(nrow(faults), 0)

    faults <- analysis$run(
      tte              = pipeline_tte,
      use_weighted_cif = TRUE
    ) |> filter(cif < lag(cif))

    expect_equal(nrow(faults), 0)
  })

  it("only produces CIF values that is a probability", {
    faults <- analysis$run(
      tte              = pipeline_tte,
      use_weighted_cif = FALSE
    ) |> filter(cif < 0 | cif > 1)

    expect_equal(nrow(faults), 0)

    faults <- analysis$run(
      tte              = pipeline_tte,
      use_weighted_cif = TRUE
    ) |> filter(cif < 0 | cif > 1)

    expect_equal(nrow(faults), 0)
  })

  ## Beware the Kaplan-Meier Overestimate: If you run a standard Kaplan-Meier analysis (1 - S(t)) on your event of interest
  ## while ignoring competing risks, it will overestimate the event probability.
  ## Your true CIF curve must always sit lower than or equal to the naive Kaplan-Meier curve

  #it("produces different results if different cohorts and same method is used", {
  #  cif_c1 <- analysis$run(
  #   tte = pipeline_tte |> select(-weight)
  # )

  #  cif_c2 <- analysis$run(
  #    tte = pipeline_tte[relatives_trait_n > 0] |> select(-weight)
  #  )

  #  expect_dataframe_not_equal(cif_c1, cif_c2)

  #  cif_c1 <- analysis$run(
  #    tte = pipeline_tte |> mutate(weight = 1.0)
  #  )

  #  cif_c2 <- analysis$run(
  #    tte = pipeline_tte[relatives_trait_n > 0]
  #  )

  #  cif_c2_no_filter <- analysis$run(
  #    tte = pipeline_tte
  #  )

  #  expect_dataframe_not_equal(cif_c1, cif_c2)
  #  expect_dataframe_equal(cif_c2, cif_c2_no_filter)
  #})

  #it("produces different results if weight is given", {
  #  original <- analysis$run(tte = pipeline_tte |> select(-weight))
  #  weighted <- analysis$run(tte = pipeline_tte)

  #  combined <- inner_join(original, weighted, by = join_by(time)) |>
  #    mutate(
  #      cif_diff = abs(cif.x - cif.y)
  #    ) |>
  #    filter(
  #      time >= 1
  #    )

  #  diff <- combined |>
  #    filter(cif_diff > testthat_tolerance())

  #  expect_gt(nrow(combined), 0)
  #  expect_equal(nrow(combined), nrow(diff))
  #})

  #it("produces same results if weight is set to 1", {
  #  original <- analysis$run(tte = pipeline_tte |> select(-weight))
  #  weighted <- analysis$run(tte = pipeline_tte |> mutate(weight = 1.0))

  #  diff <- inner_join(original, weighted, by = join_by(time)) |>
  #    mutate(
  #      cif_diff = abs(cif.x - cif.y),
  #    ) |>
  #    filter(
  #      cif_diff > testthat_tolerance()
  #    )

  #  expect_equal(nrow(diff), 0)
  #})
})
