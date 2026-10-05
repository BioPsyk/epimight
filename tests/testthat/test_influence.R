library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

#=================================================================================
# Preparation
#=================================================================================

expect_close <- function(actual, expected, rtol, atol) {
  gap <- abs(actual - expected)
  expect_true(all(gap <= atol + rtol * abs(expected)), info = sprintf("max gap %.3g", max(gap)))
}

persons    <- sandwich_persons()
cohorts    <- sandwich_cohorts(persons)
private_ci <- CumulativeIncidenceAnalysis$new()$.__enclos_env__$private
h2_calc    <- HeritabilityAnalysis$new()
rg_calc    <- GeneticCorrelationAnalysis$new()
rc         <- 0.5

# Rows of every status, both sides of the evaluation age, in one stratum.
probe_rows <- function(cohort, age) {
  set.seed(2)
  rows <- cohort[, .I[stratum == "a"]]
  pick <- function(sel) head(sample(rows[sel[rows]]), 4)
  unique(c(
    pick(cohort$trait_status == 1 & cohort$trait_age < age),
    pick(cohort$trait_status == 1 & cohort$trait_age == age),
    pick(cohort$trait_status == 2 & cohort$trait_age < age),
    pick(cohort$trait_status == 0),
    pick(cohort$trait_age > age)
  ))
}

# Point estimates of every stratum's h2 and rg at that stratum's shared CIF age.
chain_points <- function(cohorts, ages) {
  unlist(lapply(names(ages), function(s) {
    k  <- shipped_cifs(cohorts, s, ages[[s]])
    h1 <- h2_calc$calculate_h2(1, k[["pop1"]], k[["fh1"]], 1, 1, rc)$h2
    h2 <- h2_calc$calculate_h2(1, k[["pop2"]], k[["fh2"]], 1, 1, rc)$h2
    rg <- rg_calc$calculate_rg(1, k[["pop1"]], k[["cross"]], k[["pop2"]], 1, 1, 1, h1, h2, rc)$rg
    setNames(c(h1, h2, rg), paste0(c("h2_1@", "h2_2@", "rg@"), s))
  }))
}

#=================================================================================
# Tests
#=================================================================================

describe("aj_influence", {
  fh  <- cohorts$fh1[stratum == "a"]
  pop <- cohorts$pop1[stratum == "a"]

  it("reports the weighted path's CIF at every age it reports one", {
    shipped <- private_ci$run_weighted_single(fh[, .(trait_age, trait_status, weight)])
    ours    <- aj_influence(fh$trait_age, fh$trait_status, fh$weight, shipped$age)$cif

    # The weighted path's lag() leaves its first age NA when that age is above 0.
    expect_equal(which(is.na(shipped$cif)), 1L)
    expect_lt(max(abs(ours - shipped$cif)[-1]), 1e-12)
  })

  it("reports cmprsk's CIF at every reported age", {
    tte     <- pop[, .(person_id = as.character(person), trait_status, trait_age)]
    shipped <- CumulativeIncidenceAnalysis$new()$run(tte = tte)
    ours    <- aj_influence(pop$trait_age, pop$trait_status, rep(1, nrow(pop)), shipped$age)$cif

    expect_lt(max(abs(ours - shipped$cif)), 1e-12)
  })

  it("moves the CIF by eps * phi when one row's weight scales by 1 + eps", {
    for (cohort in list(fh, pop)) {
      ages <- private_ci$run_weighted_single(cohort[, .(trait_age, trait_status, weight)])$age
      ages <- ages[c(2, length(ages) %/% 2, length(ages))]
      phi  <- aj_influence(cohort$trait_age, cohort$trait_status, cohort$weight, ages)$phi

      for (j in seq_along(ages)) {
        point <- function(m) shipped_weighted_cif(copy(cohort)[, weight := weight * m], "a", ages[j])

        for (i in probe_rows(cohort, ages[j])) {
          expect_close(gateaux(point, nrow(cohort), i), phi[i, j], rtol = 1e-6, atol = 1e-9)
        }
      }
    }
  })

  it("sums to zero over the rows", {
    phi <- aj_influence(fh$trait_age, fh$trait_status, fh$weight, c(3, 8, 15))$phi

    expect_lt(max(abs(colSums(phi))), 1e-15)
  })

  it("is zero at age 0", {
    infl <- aj_influence(fh$trait_age, fh$trait_status, fh$weight, 0)

    expect_equal(infl$cif, 0)
    expect_true(all(infl$phi == 0))
  })
})

describe("per-stratum h2 and rg influence", {
  ages <- c(a = shared_cif_age(cohorts, "a"), b = shared_cif_age(cohorts, "b"))

  units <- rbindlist(lapply(names(ages), function(s) {
    k <- shipped_cifs(cohorts, s, ages[[s]])
    data.table(stratum = s, age = ages[[s]], k_pop1 = k[["pop1"]], k_fh1 = k[["fh1"]], k_pop2 = k[["pop2"]],
               k_fh2 = k[["fh2"]], k_cross = k[["cross"]])
  }))
  units[, `:=`(
    h2_t1 = h2_calc$calculate_h2(1, k_pop1, k_fh1, 1, 1, rc)$h2,
    h2_t2 = h2_calc$calculate_h2(1, k_pop2, k_fh2, 1, 1, rc)$h2
  )]

  h2 <- rbind(
    h2_terms(units[, .(output = paste0("h2_1@", stratum), stratum, age, pop = "pop1", fh = "fh1",
                       k_pop = k_pop1, k_fh = k_fh1)], rc),
    h2_terms(units[, .(output = paste0("h2_2@", stratum), stratum, age, pop = "pop2", fh = "fh2",
                       k_pop = k_pop2, k_fh = k_fh2)], rc)
  )
  rg <- rg_terms(
    units[, .(output = paste0("rg@", stratum), stratum, age, pop1 = "pop1", cross = "cross", pop2 = "pop2",
              k_pop1, k_cross, k_pop2, h2_t1, h2_t2,
              h2_t1_output = paste0("h2_1@", stratum), h2_t2_output = paste0("h2_2@", stratum))],
    h2, rc
  )
  n_rows <- nrow(persons) + 5
  psi    <- assemble_influence(rbind(h2, rg), sandwich_assembly_cohorts(cohorts), n_rows)

  it("matches the Gateaux derivative of every output in every person's weight", {
    set.seed(3)
    probes <- c(sample(persons[stratum == "a" & w1 > 0 & w2 > 0, person], 6),
                sample(persons[stratum == "b" & w1 == 0, person], 4),
                sample(persons[stratum == "b" & w2 > 0, person], 4))

    for (i in probes) {
      derivative <- gateaux(function(m) chain_points(sandwich_cohorts(persons, m), ages), nrow(persons), i)
      expect_close(psi[i, names(derivative)], derivative, rtol = 1e-6, atol = 1e-9)
    }
  })

  it("is zero on pedigree rows outside the cohorts", {
    expect_true(all(psi[(nrow(persons) + 1):n_rows, ] == 0))
  })

  it("refuses a CIF that disagrees with the plug-in", {
    bad <- copy(h2)[1, k := k + 1e-6]

    expect_error(assemble_influence(bad, sandwich_assembly_cohorts(cohorts), n_rows), "disagrees")
  })
})

describe("meta_shares", {
  set.seed(4)
  estimates <- data.table(
    index_trait = "t",
    age         = rep(c(10, 11, 12), each = 4),
    h2          = c(runif(11, 0.2, 0.6), Inf),
    se          = c(runif(10, 0.02, 0.1), NA, 0.05)
  )
  meta <- Analysis$new()$run_meta(
    estimates = estimates, estimate_column = "h2", se_column = "se", group_columns = list("index_trait", "age")
  )

  for (method in c("fixed", "random")) {
    it(sprintf("pools to run_meta's %s point", method), {
      shared <- copy(estimates)[, share := meta_shares(h2, se, age, method)]
      pooled <- shared[!is.na(share), .(pooled = sum(share * h2)), by = age]

      expect_equal(pooled$pooled, meta[[paste0(method, "_meta")]], tolerance = 1e-14)
      expect_true(all(is.na(shared$share[11:12])))
    })
  }

  it("does not change when a group's raw weights share a common factor", {
    share  <- meta_shares(estimates$h2, estimates$se, estimates$age, "fixed")
    scaled <- meta_shares(estimates$h2, estimates$se * rep(c(1, 3, 1), each = 4), estimates$age, "fixed")

    expect_equal(scaled, share, tolerance = 1e-14)
  })

  it("leaves a random pool undefined when one row has no variance", {
    expect_true(is.na(meta_shares(0.3, 0.05, 1, "random")))
  })
})
