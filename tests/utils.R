library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Generators
#=================================================================================

generate_relatives_diagnosed_prob <- function(failure_status, relatives) {
  if (failure_status == 1) {
    unaffected_prob <- 0.45
  } else {
    unaffected_prob <- 0.75
  }

  rest_prob <- 1.0 - unaffected_prob
  prob_per_relative <- rest_prob / relatives

  prob <- append(
    c(unaffected_prob),
    rep(prob_per_relative, relatives)
  )

  return(prob)
}

generate_relatives_diagnosed <- function(tte, column_name) {
  survival_data <- tte |>
    rowwise() |>
    mutate(
      !!column_name := sample(
        0:relatives,
        1,
        replace = TRUE,
        prob = generate_relatives_diagnosed_prob(failure_status, relatives)
      )
    )

  return(survival_data)
}

generate_failure <- function(tte, mean, sd, end_of_study) {
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
        dead_at_year > end_of_study_year,
        end_of_study_year - born_at_year,
        dead_at_year - born_at_year
      ),
      failure_status = ifelse(
        dead_at_year > end_of_study_year,
        sample(
          seq(0, 1),
          1,
          replace = TRUE,
          prob = c(0.9, 0.1)
        ),
        sample(
          seq(1, 2),
          1,
          replace = TRUE,
          prob = c(0.1, 0.9)
        )
      ),
      onset_age = round(rnorm(1, mean = mean, sd = sd)),
      failure_time = ifelse(
        failure_status == 1,
        case_when(
          onset_age < 0 ~ 0,
          onset_age > max_age ~ max_age,
          .default = onset_age
        ),
        max_age
      )
    ) |>
    select(-max_age, -onset_age)

  return(survival_data)
}

generate_random_tte <- function(n_count, period_start, period_end) {
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
    person_id      = 1:n_count,
    gender         = sample(c("m", "f"), n_count, replace = TRUE),
    born_at        = sample(birth_dates, n_count, replace = TRUE),
    death_age      = round(
      rnorm(n_count, mean = 68.9, sd = 8.2)
    ),
    relatives = sample(
      seq(0, 9),
      n_count,
      replace = TRUE,
      prob = c(0.2, 0.22, 0.23, 0.16, 0.09, 0.05, 0.03, 0.01, 0.008, 0.002)
    )
  ) |>
    mutate(
      born_at_year = as.numeric(format(born_at, "%Y")),
      dead_at_year = born_at_year + death_age
    ) |>
    select(-death_age) |>
    distinct(person_id, .keep_all = TRUE)

  return(survival_data)
}

generate_pipeline_tte <- function(n_count) {
  d1_fs_tte <- generate_random_tte(n_count)
  d1_fs_tte <- generate_failure(d1_fs_tte, 20, 10)
  d1_fs_tte <- generate_relatives_diagnosed(d1_fs_tte, "relatives_diagnosed") |>
    relocate(failure_time, .after = person_id) |>
    relocate(failure_status, .after = failure_time) |>
    relocate(relatives, .after = failure_status) |>
    relocate(relatives_diagnosed, .after = relatives) |>
    mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "FS") |>
    as.data.table()

  d2_fs_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
  d2_fs_tte <- generate_failure(d2_fs_tte, 19, 11)
  d2_fs_tte <- generate_relatives_diagnosed(d2_fs_tte, "relatives_diagnosed") |>
    relocate(failure_time, .after = person_id) |>
    relocate(failure_status, .after = failure_time) |>
    relocate(relatives, .after = failure_status) |>
    relocate(relatives_diagnosed, .after = relatives) |>
    mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "FS") |>
    as.data.table()

  d1_po_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
  d1_po_tte <- generate_failure(d2_fs_tte, 20, 10)
  d1_po_tte <- generate_relatives_diagnosed(d2_fs_tte, "relatives_diagnosed") |>
    relocate(failure_time, .after = person_id) |>
    relocate(failure_status, .after = failure_time) |>
    relocate(relatives, .after = failure_status) |>
    relocate(relatives_diagnosed, .after = relatives) |>
    mutate(person_id = as.character(person_id), disorder = "SCZ", relationship_kind = "PO") |>
    as.data.table()

  d2_po_tte <- copy(d1_fs_tte |> select(-failure_time, -failure_status, -relatives_diagnosed, -disorder, -relationship_kind))
  d2_po_tte <- generate_failure(d2_fs_tte, 19, 11)
  d2_po_tte <- generate_relatives_diagnosed(d2_fs_tte, "relatives_diagnosed") |>
    relocate(failure_time, .after = person_id) |>
    relocate(failure_status, .after = failure_time) |>
    relocate(relatives, .after = failure_status) |>
    relocate(relatives_diagnosed, .after = relatives) |>
    mutate(person_id = as.character(person_id), disorder = "CAD", relationship_kind = "PO") |>
    as.data.table()

  tte <- rbindlist(list(d1_fs_tte, d2_fs_tte, d1_po_tte, d2_po_tte)) |> select(-born_at, -dead_at_year) |>
    arrange(person_id, disorder, relationship_kind) |>
    select(person_id, born_at_year, disorder, failure_status, failure_time, relationship_kind, relatives, relatives_diagnosed)

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
