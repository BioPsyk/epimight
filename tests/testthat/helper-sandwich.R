library(data.table, quietly = TRUE, warn.conflicts = FALSE)

# Persons with two traits, two strata and family-history weights for each trait.
# Competing events, ties and fractional weights all occur.
sandwich_persons <- function(n = 300, strata = c("a", "b"), seed = 1) {
  set.seed(seed)
  persons <- data.table(
    person  = seq_len(n),
    stratum = rep_len(strata, n),
    w1      = ifelse(runif(n) < 0.6, sample(c(1, 2, 3), n, TRUE) / 3, 0),
    w2      = ifelse(runif(n) < 0.6, sample(c(1, 2, 4), n, TRUE) / 4, 0)
  )
  draw <- function(boost) {
    u      <- runif(n)
    status <- ifelse(u < 0.2 + boost, 1L, ifelse(u < 0.32 + boost, 2L, 0L))
    age    <- ifelse(status == 0L, 15L, sample(1:15, n, TRUE))
    list(status = status, age = as.integer(age))
  }
  t1 <- draw(0.15 * (persons$w1 > 0))
  t2 <- draw(0.15 * (persons$w2 > 0))
  persons[, `:=`(t1_status = t1$status, t1_age = t1$age, t2_status = t2$status, t2_age = t2$age)]
  persons
}

# The five cohorts of an rg analysis, with person weights scaled by `multiplier`.
sandwich_cohorts <- function(persons, multiplier = rep(1, nrow(persons))) {
  p <- copy(persons)[, m := multiplier]
  list(
    pop1  = p[, .(person, stratum, trait_age = t1_age, trait_status = t1_status, weight = m)],
    fh1   = p[w1 > 0, .(person, stratum, trait_age = t1_age, trait_status = t1_status, weight = w1 * m)],
    pop2  = p[, .(person, stratum, trait_age = t2_age, trait_status = t2_status, weight = m)],
    fh2   = p[w2 > 0, .(person, stratum, trait_age = t2_age, trait_status = t2_status, weight = w2 * m)],
    cross = p[w2 > 0, .(person, stratum, trait_age = t1_age, trait_status = t1_status, weight = w2 * m)]
  )
}

# The cohorts in the layout `assemble_influence` takes, with person i on pedigree row i.
sandwich_assembly_cohorts <- function(cohorts) {
  lapply(cohorts, function(cohort) {
    split(cohort[, .(row = person, stratum, trait_age, trait_status, weight)], by = "stratum", keep.by = FALSE)
  })
}

# EPIMIGHT's own weighted Aalen-Johansen CIF of one cohort and stratum at `age`.
shipped_weighted_cif <- function(cohort, at_stratum, at_age) {
  private <- CumulativeIncidenceAnalysis$new()$.__enclos_env__$private
  table   <- private$run_weighted_single(cohort[stratum == at_stratum, .(trait_age, trait_status, weight)])
  table[age == at_age, cif]
}

# The five CIFs of `cohorts` at `age` in `stratum`, named like the cohorts.
shipped_cifs <- function(cohorts, at_stratum, at_age) {
  vapply(cohorts, shipped_weighted_cif, numeric(1), at_stratum = at_stratum, at_age = at_age)
}

# The last age at which all five cohorts of a stratum report a CIF.
shared_cif_age <- function(cohorts, at_stratum) {
  private <- CumulativeIncidenceAnalysis$new()$.__enclos_env__$private
  ages    <- lapply(cohorts, function(cohort) {
    private$run_weighted_single(cohort[stratum == at_stratum, .(trait_age, trait_status, weight)])$age
  })
  max(Reduce(intersect, ages))
}

# Central-difference Gateaux derivative of `point(multiplier)` in person i's weight.
gateaux <- function(point, n, i, eps = 1e-5) {
  up      <- rep(1, n)
  down    <- rep(1, n)
  up[i]   <- 1 + eps
  down[i] <- 1 - eps
  (point(up) - point(down)) / (2 * eps)
}
