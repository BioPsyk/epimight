current_year <- as.numeric(
  format(Sys.Date(), "%Y")
)

#=================================================================================
# Generators
#=================================================================================

generate_diagnosed_relatives_prob <- function(failure_status, relatives) {
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

generate_diagnosed_relatives <- function(tte, column_name) {
  survival_data <- tte |>
    rowwise() |>
    mutate(
      !!column_name := sample(
        0:relatives,
        1,
        replace = TRUE,
        prob = generate_diagnosed_relatives_prob(failure_status, relatives)
      )
    )

  return(survival_data)
}

generate_failure <- function(tte, mean, sd) {
  survival_data <- tte |>
    rowwise() |>
    mutate(
      max_age = ifelse(
        dead_at_year > current_year,
        current_year - born_at_year,
        dead_at_year - born_at_year
      ),
      failure_status = ifelse(
        dead_at_year > current_year,
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

generate_random_tte <- function(n_count) {
  birth_dates <- seq(
    as.Date("1950-01-01"),
    Sys.Date(),
    by = "day"
  )

  survival_data <- data.frame(
    person_id      = 1:n_count,
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

plot_benchmark_sumstats <- function(input_path, output_path) {
  sumstats <- read_csv(input_path)

  ggplot(sumstats, aes(x = n, y = mean, color = expr)) + geom_line()
}
