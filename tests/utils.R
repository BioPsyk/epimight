library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Generators
#=================================================================================

trait_1_probability <- function(trait_status, relatives_n) {
  if (trait_status == 1) {
    unaffected_prob <- 0.45
  } else {
    unaffected_prob <- 0.75
  }

  rest_prob         <- 1.0 - unaffected_prob
  prob_per_relative <- rest_prob / relatives_n

  prob <- append(
    c(unaffected_prob),
    rep(prob_per_relative, relatives_n)
  )

  return(prob)
}

tte_random_relatives_n_trait <- function(tte, probs_func) {
  tte |>
    rowwise() |>
    mutate(
      relatives_n_trait := sample(
        0:relatives_n,
        1,
        replace = TRUE,
        prob = probs_func(trait_status, relatives_n)
      )
    )
}

tte_random_trait <- function(tte, trait_name, trait_prob, trait_age_mean, trait_age_sd, end_of_study) {
  if (missing(end_of_study)) {
    end_of_study <- Sys.Date()
  }

  end_of_study_year <- as.numeric(
    format(end_of_study, "%Y")
  )

  survival_data <- tte |>
    rowwise() |>
    mutate(
      max_age = ifelse(
        death_year > end_of_study_year,
        end_of_study_year - birth_year,
        death_year - birth_year
      ),
      trait = trait_name,
      trait_status = ifelse(
        death_year > end_of_study_year,
        sample(
          seq(1, 0),
          1,
          replace = TRUE,
          prob = trait_prob
        ),
        sample(
          seq(1, 2),
          1,
          replace = TRUE,
          prob = trait_prob
        )
      ),
      onset_age = round(rnorm(1, mean = trait_age_mean, sd = trait_age_sd)),
      trait_age = ifelse(
        trait_status == 1,
        case_when(
          onset_age < 0 ~ 0,
          onset_age > max_age ~ max_age,
          .default = onset_age
        ),
        max_age
      )
    ) |>
    mutate(
      trait_age = abs(trait_age) # Sometimes the onset is -0
    ) |>
    select(-max_age, -onset_age)

  return(survival_data)
}

tte_random_probands <- function(n_count, period_start, period_end) {
  if (missing(period_start)) {
    period_start <- as.Date("1950-01-01")
  }

  if (missing(period_end)) {
    period_end <- Sys.Date()
  }

  birth_dates <- seq(
    period_start,
    period_end,
    by = "day"
  )

  survival_data <- data.frame(
    person_id      = as.character(1:n_count),
    sex            = sample(c("m", "f"), n_count, replace = TRUE),
    birth_date     = sample(birth_dates, n_count, replace = TRUE),
    death_age      = round(
      rnorm(n_count, mean = 68.9, sd = 8.2)
    ),
    relatives_n = sample(
      seq(0, 9),
      n_count,
      replace = TRUE,
      prob = c(0.2, 0.22, 0.23, 0.16, 0.09, 0.05, 0.03, 0.01, 0.008, 0.002)
    )
  ) |>
    mutate(
      birth_year = as.numeric(format(birth_date, "%Y")),
      death_year = birth_year + death_age
    ) |>
    select(-death_age) |>
    distinct(person_id, .keep_all = TRUE)

  return(survival_data)
}

generate_analysis_tte <- function(n_count, trait, trait_mean, trait_sd, relkind) {
  tte_random_probands(n_count) |>
    tte_add_random_trait(trait, trait_mean, trait_sd) |>
    tte_add_random_relatives_n_trait("relatives_n_trait") |>
    relocate(trait_age, .after = person_id) |>
    relocate(trait_status, .after = trait_age) |>
    relocate(relatives_n, .after = trait_status) |>
    relocate(relatives_n_trait, .after = relatives_n) |>
    mutate(person_id = as.character(person_id), relatives_kind = relkind) |>
    as.data.table()
}

generate_pipeline_tte <- function(n_count) {
  t1_fs_tte <- generate_analysis_tte(n_count, "SCZ", 20, 10, "half_siblings")

  t2_fs_tte <- copy(t1_fs_tte |> select(-trait_age, -trait_status, -relatives_n_trait, -trait, -relatives_kind))
  t2_fs_tte <- tte_add_random_trait(t2_fs_tte, "CAD", 19, 11)
  t2_fs_tte <- tte_add_random_relatives_n_trait(t2_fs_tte, "relatives_n_trait") |>
    relocate(trait_age, .after = person_id) |>
    relocate(trait_status, .after = trait_age) |>
    relocate(relatives_n, .after = trait_status) |>
    relocate(relatives_n_trait, .after = relatives_n) |>
    mutate(person_id = as.character(person_id), relatives_kind = "half_siblings") |>
    as.data.table()

  t1_p_tte <- copy(t1_fs_tte |> select(-trait_age, -trait_status, -relatives_n_trait, -trait, -relatives_kind))
  t1_p_tte <- tte_add_random_trait(t1_p_tte, "SCZ", 20, 10)
  t1_p_tte <- tte_add_random_relatives_n_trait(t1_p_tte, "relatives_n_trait") |>
    relocate(trait_age, .after = person_id) |>
    relocate(trait_status, .after = trait_age) |>
    relocate(relatives_n, .after = trait_status) |>
    relocate(relatives_n_trait, .after = relatives_n) |>
    mutate(person_id = as.character(person_id), relatives_kind = "parents") |>
    as.data.table()

  t2_p_tte <- copy(t1_fs_tte |> select(-trait_age, -trait_status, -relatives_n_trait, -trait, -relatives_kind))
  t2_p_tte <- tte_add_random_trait(t2_p_tte, "CAD", 19, 11)
  t2_p_tte <- tte_add_random_relatives_n_trait(t2_p_tte, "relatives_n_trait") |>
    relocate(trait_age, .after = person_id) |>
    relocate(trait_status, .after = trait_age) |>
    relocate(relatives_n, .after = trait_status) |>
    relocate(relatives_n_trait, .after = relatives_n) |>
    mutate(person_id = as.character(person_id), relatives_kind = "parents") |>
    as.data.table()

  tte <- rbindlist(list(t1_fs_tte, t2_fs_tte, t1_p_tte, t2_p_tte)) |> select(-birth_date, -death_year) |>
    arrange(person_id, trait, relatives_kind) |>
    select(person_id, birth_year, trait, trait_status, trait_age, relatives_kind, relatives_n, relatives_n_trait)

  return(tte)
}

#=================================================================================
# Expect handlers
#=================================================================================

expect_dataframe_equal <- function(a, b, ignore_cols = NULL) {
  colnames_diff <- setdiff(colnames(a), colnames(b))

  if (length(colnames_diff) > 0) {
    fail(
      message = sprintf(
        "columns differ: %s",
        paste(
          colnames_diff,
          collapse = ", "
        )
      )
    )
    return()
  }

  if (nrow(a) != nrow(b)) {
    fail(
      message = sprintf(
        "numbers of rows differ: %d == %d",
        nrow(a), nrow(b)
      )
    )
    return()
  }

  if (is.null(ignore_cols)) {
    ignore_cols <- c()
  }

  failures <- list()

  for (col in colnames(a)) {
    if (col %in% ignore_cols) {
      next
    }

    col_a <- a[[col]]
    col_b <- b[[col]]

    idx <- 1
    for (row in col_a) {
      val_a <- col_a[idx]
      val_b <- col_b[idx]

      comp <- waldo::compare(val_a, val_b, tolerance = testthat_tolerance())

      if (length(comp) > 0) {
        failures <- append(
          failures,
          sprintf(
            "- row %d, column '%s': \n%s",
            idx, col, comp
          )
        )
      }

      idx <- idx + 1
    }
  }

  failures_count <- length(failures)

  if (failures_count > 0) {
    fail(
      message = sprintf(
        "found %d mismatches: \n\n%s",
        failures_count,
        paste(failures, collapse = "\n")
      )
    )
  }

  succeed()
}

expect_dataframe_not_equal <- function(a, b, ignore_cols = NULL) {
  expect_failure(
    expect_dataframe_equal(a, b, ignore_cols)
  )
}

#=================================================================================
# Helpers
#=================================================================================

capitalize <- function(cols) {
  results <- c()

  for (col in cols) {
    results <- c(
      results,
      paste(
        toupper(substring(col, 1, 1)),
        substring(col, 2),
        sep = "",
        collapse = ""
      )
    )
  }

  return(results)
}

run_benchmark <- function(samples, iterations, benchmarks) {
  results <- data.table(
    name      = c(),
    iteration = c(),
    samples   = c(),
    time      = c(),
    unit      = c()
  )

  message(">> Benchmarking started")

  for (i in seq(1, iterations)) {
    message(sprintf("-- Running iteration %s", i))
    for (b in names(benchmarks)) {
      message(sprintf("-- Running benchmark %s", b))
      func       <- benchmarks[[b]]
      start_time <- Sys.time()
      func()
      stop_time  <- Sys.time()

      results <- rbind(
        results,
        list(
          name      = b,
          iteration = i,
          samples   = samples,
          time      = as.numeric(stop_time - start_time),
          unit      = "s"
        )
      )
    }
  }

  return(results)
}

plot_benchmark_results <- function(title, samples, iterations, results, output_path) {
  ggplot(results, aes(x = name, y = time, color = name)) +
    geom_boxplot() +
    labs(
      title    = title,
      subtitle = sprintf("%s TTE rows, %s measurements per function", samples, iterations),
      x        = NULL,
      y        = "Runtime (seconds)",
      color    = "Function"
    ) +
    theme(axis.text.x = element_blank())

  ggsave(output_path)
}

plot_cif_results <- function(title, subtitle, results, group_column, output_path) {
  ggplot(results, aes(x = time, y = cif, color = !!as.symbol(group_column))) +
    geom_line() +
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = !!as.symbol(group_column)), alpha = 0.15, color = NA) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Years",
      y        = "Cumulative Incidence"
    )

  ggsave(output_path)
}
