#' @title Influence functions behind the pedigree-pair sandwich standard errors.
#' @description
#' Every h2 and rg estimate is a smooth function of weighted Aalen-Johansen CIF values, so
#' each has a first-order expansion `estimate - truth ~ sum_i phi_i` over the probands of
#' the cohorts it reads. These functions compute the `phi` of one CIF and the chain-rule
#' coefficients that carry it into h2, rg and their meta-analyses.
#' @name influence
NULL

# Column names the term tables use inside data.table expressions.
utils::globalVariables(c(
  ".", "age", "coef", "cohort", "cross", "fh", "from", "h2_t1_output", "h2_t2_output", "i.coef", "k",
  "k_cross", "k_fh", "k_pop", "k_pop1", "k_pop2", "output", "pop", "pop1", "pop2", "stratum", "to"
))

# Weighted sums of `w` per 1-based index `i`, as a length `n` vector.
bincount <- function(i, w, n) {
  out  <- numeric(n)
  sums <- rowsum(w, i)
  out[as.integer(rownames(sums))] <- sums[, 1]
  out
}

#' Per-row influence of the cause-1 cumulative incidence at the given ages.
#'
#' The value reported at age `a` is the left limit `F(a-) = sum_{t < a} S(t-) dN1(t) / Y(t)`,
#' with all-cause survival `S`, which is what `CumulativeIncidenceAnalysis` reports for both
#' the weighted and the `cmprsk` path. A competing event (`status == 2`) enters through `S`
#' only. `phi[i, j]` carries row `i`'s share `w_i / sum(w)` of the cohort weight, so scaling
#' row `i`'s weight by `1 + eps` moves the CIF at `at[j]` by `eps * phi[i, j]` to first order,
#' and the CIF error is `sum_i phi[i, j]` without a `1 / n`.
#'
#' @param age_at Integer event or censoring age per row.
#' @param status 0 censored, 1 the event of interest, 2 a competing event.
#' @param weight Row weight; 1 for an unweighted cohort.
#' @param at Ages to evaluate the CIF at.
#' @returns A list with `phi`, a `length(age_at)` by `length(at)` matrix, and `cif`, the
#'   plug-in CIF at each of `at`.
#' @keywords internal
aj_influence <- function(age_at, status, weight, at) {
  time <- as.integer(age_at)
  n    <- length(time)
  phi  <- matrix(0, n, length(at))
  cif  <- numeric(length(at))

  if (n == 0) return(list(phi = phi, cif = cif))

  n_age    <- max(time) + 1L
  idx      <- time + 1L
  total    <- sum(weight)
  is1      <- status == 1
  is_event <- status != 0

  wsum <- bincount(idx, weight, n_age)
  dn1  <- bincount(idx[is1], weight[is1], n_age)
  dn   <- bincount(idx[is_event], weight[is_event], n_age)

  at_risk <- rev(cumsum(rev(wsum)))
  y_tilde <- at_risk / total
  live    <- at_risk > 0
  lam     <- ifelse(live, dn / at_risk, 0)
  lam1    <- ifelse(live, dn1 / at_risk, 0)

  surv_left <- c(1, cumprod(1 - lam)[-n_age])
  inc       <- surv_left * lam1
  # The exact discrete-time factor of the hazard's derivative, 1 / (Y / W * (1 - lambda)).
  g          <- ifelse(live & lam < 1, 1 / (y_tilde * (1 - lam)), 0)
  inc_over_y <- ifelse(live, inc / y_tilde, 0)
  jump       <- ifelse(live, surv_left / y_tilde, 0)

  cum_f  <- cumsum(inc)
  cum_c  <- cumsum(lam * g)
  c_left <- c(0, cum_c[-n_age])
  cum_b  <- cumsum(inc_over_y)
  cum_e  <- cumsum(inc * c_left)
  share  <- weight / total

  for (j in seq_along(at)) {
    if (at[j] < 1) next

    # 1-based index of the last age before at[j], capped at the last observed age
    last   <- min(at[j], n_age)
    f_last <- cum_f[last]
    cap    <- pmin(idx, last)
    cap1   <- pmin(idx + 1L, last)

    term_a <- (is1 & idx <= last) * jump[idx]
    term_b <- cum_b[cap]
    term_c <- (is_event & idx < last) * g[idx] * (f_last - cum_f[cap])
    term_d <- cum_e[cap1] + cum_c[cap] * (f_last - cum_f[cap1])

    phi[, j] <- share * (term_a - term_b - term_c + term_d)
    cif[j]   <- f_last
  }

  list(phi = phi, cif = cif)
}

#' Central-difference Jacobian of a row-vectorized function.
#'
#' `f` takes a named list of equal-length numeric vectors and returns one value per row.
#' Rows are independent, so every row's partial in one input comes from a single pair of
#' calls.
#'
#' @param f Function of a named list of numeric vectors.
#' @param inputs Named list of numeric vectors, one value per row.
#' @param step Absolute step of the central difference.
#' @returns A rows by inputs matrix of partial derivatives.
#' @keywords internal
chain_jacobian <- function(f, inputs, step = 1e-6) {
  rows <- length(inputs[[1]])
  grad <- vapply(names(inputs), function(name) {
    up            <- inputs
    down          <- inputs
    up[[name]]    <- up[[name]] + step
    down[[name]]  <- down[[name]] - step
    (f(up) - f(down)) / (2 * step)
  }, numeric(rows))

  matrix(grad, nrow = rows, dimnames = list(NULL, names(inputs)))
}

#' Gradient of `HeritabilityAnalysis$calculate_h2`'s point estimate in its two CIFs.
#'
#' The case counts only enter the native SE, so they are held at 1.
#'
#' @param k_pop Population CIF per row.
#' @param k_fh Family-history CIF per row.
#' @param rc Relationship coefficient.
#' @returns A matrix with columns `pop` and `fh`.
#' @keywords internal
h2_gradient <- function(k_pop, k_fh, rc, calculator = HeritabilityAnalysis$new()) {
  ones <- rep(1, length(k_pop))
  chain_jacobian(
    function(x) suppressWarnings(calculator$calculate_h2(ones, x$pop, x$fh, ones, ones, rc)$h2),
    list(pop = k_pop, fh = k_fh)
  )
}

#' Gradient of `GeneticCorrelationAnalysis$calculate_rg`'s rg in its three CIFs and two h2s.
#'
#' The h2 inputs are free: a caller that computed them from the same CIFs chains their own
#' influence in through the `h2_t1` and `h2_t2` columns.
#'
#' @param k_pop1 Trait 1 population CIF per row.
#' @param k_cross Cross-trait CIF per row.
#' @param k_pop2 Trait 2 population CIF per row.
#' @param h2_t1 Trait 1 heritability per row.
#' @param h2_t2 Trait 2 heritability per row.
#' @param rc Relationship coefficient.
#' @returns A matrix with columns `pop1`, `cross`, `pop2`, `h2_t1` and `h2_t2`.
#' @keywords internal
rg_gradient <- function(k_pop1, k_cross, k_pop2, h2_t1, h2_t2, rc,
                        calculator = GeneticCorrelationAnalysis$new()) {
  ones <- rep(1, length(k_pop1))
  chain_jacobian(
    function(x) {
      suppressWarnings(
        calculator$calculate_rg(ones, x$pop1, x$cross, x$pop2, ones, ones, ones, x$h2_t1, x$h2_t2, rc)$rg
      )
    },
    list(pop1 = k_pop1, cross = k_cross, pop2 = k_pop2, h2_t1 = h2_t1, h2_t2 = h2_t2)
  )
}

#' Normalized meta-analysis weights, matching `Analysis$run_meta`.
#'
#' Rows with a non-finite estimate or SE are dropped before anything else. Fixed raw weights
#' are `1 / se^2`; random raw weights are `1 / (se^2 + var(estimate))` with the variance over
#' every kept row, not per group. Each kept row's share is its raw weight over its group's
#' total. The pooled point is `sum(share * estimate)` per group.
#'
#' @param estimate Estimate per row.
#' @param se Native SE per row.
#' @param group Grouping key per row.
#' @param method `"fixed"` or `"random"`.
#' @returns The share per row, `NA` for dropped rows or an undefined pooling.
#' @keywords internal
meta_shares <- function(estimate, se, group, method) {
  keep <- is.finite(estimate) & is.finite(se)
  raw  <- rep(NA_real_, length(estimate))

  raw[keep] <- switch(
    method,
    fixed  = 1 / se[keep] ^ 2,
    random = 1 / (se[keep] ^ 2 + stats::var(estimate[keep]))
  )

  total <- stats::ave(ifelse(keep, raw, 0), group, FUN = sum)

  ifelse(keep, raw / total, NA_real_)
}

#' Influence terms of per-stratum h2 values.
#'
#' A term table maps CIF influences to estimate influences: output `o`'s influence is the
#' sum over its rows of `coef * phi(cohort, stratum, age)`, where `k` is the CIF the
#' estimate read at that point.
#'
#' @param units Data.table with one row per h2 value: `output`, `stratum`, `age`, the cohort
#'   keys `pop` and `fh`, and their CIFs `k_pop` and `k_fh` at that age.
#' @param rc Relationship coefficient.
#' @returns A term table with columns `output`, `cohort`, `stratum`, `age`, `k`, `coef`.
#' @keywords internal
h2_terms <- function(units, rc) {
  grad <- h2_gradient(units$k_pop, units$k_fh, rc)

  rbind(
    units[, .(output, cohort = pop, stratum, age, k = k_pop, coef = grad[, "pop"])],
    units[, .(output, cohort = fh, stratum, age, k = k_fh, coef = grad[, "fh"])]
  )
}

#' Influence terms of linear combinations of other outputs.
#'
#' @param terms A term table.
#' @param weights Data.table with columns `from` (an output of `terms`), `to` (the new
#'   output) and `coef`.
#' @returns The term table of the `to` outputs, one row per output and CIF point.
#' @keywords internal
compose_terms <- function(terms, weights) {
  terms[
    weights, on = .(output = from), allow.cartesian = TRUE, nomatch = NULL
  ][
    , .(k = k[1], coef = sum(coef * i.coef)), by = .(output = to, cohort, stratum, age)
  ]
}

#' Influence terms of per-stratum rg values.
#'
#' The rg calculator reads three CIFs directly and two h2 values. Each h2 value is either
#' that stratum's own h2 at the rg age or a pooled h2, and its influence comes in through
#' the h2 term table and the rg gradient in that h2.
#'
#' @param units Data.table with one row per rg value: `output`, `stratum`, `age`, cohort keys
#'   `pop1`, `cross`, `pop2`, their CIFs `k_pop1`, `k_cross`, `k_pop2`, the h2 values
#'   `h2_t1`, `h2_t2` the calculator used, and `h2_t1_output`, `h2_t2_output`, the outputs
#'   of `h2_terms` behind them.
#' @param h2 Term table of every output named in `h2_t1_output` and `h2_t2_output`.
#' @param rc Relationship coefficient.
#' @returns A term table.
#' @keywords internal
rg_terms <- function(units, h2, rc) {
  grad <- rg_gradient(units$k_pop1, units$k_cross, units$k_pop2, units$h2_t1, units$h2_t2, rc)

  direct <- rbind(
    units[, .(output, cohort = pop1, stratum, age, k = k_pop1, coef = grad[, "pop1"])],
    units[, .(output, cohort = cross, stratum, age, k = k_cross, coef = grad[, "cross"])],
    units[, .(output, cohort = pop2, stratum, age, k = k_pop2, coef = grad[, "pop2"])]
  )
  through_h2 <- compose_terms(h2, rbind(
    units[, .(from = h2_t1_output, to = output, coef = grad[, "h2_t1"])],
    units[, .(from = h2_t2_output, to = output, coef = grad[, "h2_t2"])]
  ))

  rbind(direct, through_h2)[
    , .(k = k[1], coef = sum(coef)), by = .(output, cohort, stratum, age)
  ]
}

#' Complete influence vectors of the outputs of a term table, over pedigree rows.
#'
#' @param terms A term table.
#' @param cohorts Named list over cohort keys; each a named list over strata of data.tables
#'   with columns `row` (the person's pedigree row), `trait_age`, `trait_status`, `weight`.
#' @param n_rows Pedigree row count.
#' @param tolerance Largest allowed gap between a term's `k` and the plug-in CIF.
#' @returns An `n_rows` by outputs matrix, columns named by output.
#' @keywords internal
assemble_influence <- function(terms, cohorts, n_rows, tolerance = 1e-10) {
  outputs <- unique(terms$output)
  psi     <- matrix(0, n_rows, length(outputs), dimnames = list(NULL, outputs))

  for (part in split(terms, by = c("cohort", "stratum"), sorted = TRUE)) {
    cohort  <- part$cohort[1]
    stratum <- part$stratum[1]
    tte     <- cohorts[[cohort]][[stratum]]

    if (is.null(tte)) stop("No cohort rows for \"", cohort, "\" in stratum \"", stratum, "\"")

    part <- part[, .(k = k[1], coef = sum(coef)), by = .(output, age)]
    ages <- sort(unique(part$age))
    infl <- aj_influence(tte$trait_age, tte$trait_status, tte$weight, ages)
    gap  <- abs(infl$cif[match(part$age, ages)] - part$k)

    if (!all(gap <= tolerance)) {
      stop(
        "Influence plug-in CIF disagrees with the reported CIF by ", signif(max(gap), 3),
        " for \"", cohort, "\" in stratum \"", stratum, "\""
      )
    }

    cols <- unique(part$output)
    coef <- matrix(0, length(ages), length(cols))
    coef[cbind(match(part$age, ages), match(part$output, cols))] <- part$coef

    psi[tte$row, cols] <- psi[tte$row, cols, drop = FALSE] + infl$phi %*% coef
  }

  psi
}
