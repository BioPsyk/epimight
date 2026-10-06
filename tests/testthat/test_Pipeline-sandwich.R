library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

skip_if_not_installed("pedigreegraph")

#=================================================================================
# Preparation
#=================================================================================

options(pedigreegraph.progress = FALSE)

golden   <- test_path("..", "data", "sandwich-golden")
pedigree <- fread(file.path(golden, "pedigree.csv"), colClasses = "character", na.strings = "")
pool     <- family_pool(pedigree)
pipeline <- Pipeline$new(pool = pool, pedigree = pedigree)
private  <- pipeline_private(pipeline)
ids      <- unique(pool$person_id)
h2_calc  <- HeritabilityAnalysis$new()
rg_calc  <- GeneticCorrelationAnalysis$new()
weighted <- CumulativeIncidenceAnalysis$new()$.__enclos_env__$private$run_weighted_single
key      <- function(cif_args) private$cache_key("cif", cif_args)
base     <- sandwich_rg_args()
pop      <- c(key(base$h2_t1$cif_pop), key(base$h2_t2$cif_pop))
fh       <- c(key(base$h2_t1$cif_fh), key(base$h2_t2$cif_fh))
cross    <- key(base$cif_cross)

expect_close <- function(actual, expected, rtol, atol) {
  gap <- abs(actual - expected)
  expect_true(all(gap <= atol + rtol * abs(expected)), info = sprintf("max gap %.3g", max(gap)))
}

# Every cohort's CIF table per stratum, each person's weight scaled by `m`, from EPIMIGHT's
# own weighted Aalen-Johansen estimator.
oracle_tables <- function(m) {
  lapply(plan_cohorts(pipeline, base), function(tte) {
    tte <- copy(tte)[, weight := weight * m[chmatch(person_id, ids)]]
    rbindlist(lapply(split(tte, by = "stratum"), function(part) {
      weighted(part[, .(trait_age, trait_status, weight)])[, .(stratum = part$stratum[1], age, cif)]
    }))
  })
}

cif_at <- function(tables, cohort, s, a) tables[[cohort]][stratum == s & age == a, cif]

h2_at <- function(tables, k, s, a) {
  h2_calc$calculate_h2(1, cif_at(tables, pop[k], s, a), cif_at(tables, fh[k], s, a), 1, 1, 0.5)$h2
}

# Shares, rg ages and the headline age, all frozen at the unperturbed fit.
local_h2 <- lapply(c("trait1", "trait2"), function(trait) {
  local <- copy(as.data.table(do.call(pipeline$run_h2, sandwich_h2_args(trait))$results))
  local[, stratum := as.character(born_at_year)]
  shares <- SandwichAnalysis$new()$calculate_meta_shares
  local[, `:=`(fixed = shares(h2, se, age, "fixed"), random = shares(h2, se, age, "random"))]
})

pooled_at <- function(tables, k, method, a) {
  rows <- local_h2[[k]][age == a & !is.na(get(method))]
  sum(rows[[method]] * vapply(rows$stratum, function(s) h2_at(tables, k, s, a), numeric(1)))
}

# One functional per rg argument set: per-stratum rg with local or pooled h2, or their meta.
rg_variants <- list(
  t1_fixed      = sandwich_rg_args(meta_t1 = "fixed"),
  t2_random     = sandwich_rg_args(meta_t2 = "random"),
  both_random   = sandwich_rg_args(meta_t1 = "random", meta_t2 = "random"),
  meta_fixed    = sandwich_rg_args(meta_t1 = "fixed", meta_t2 = "fixed", meta = "fixed"),
  meta_random   = sandwich_rg_args(meta = "random")
)

rg_frozen <- lapply(rg_variants, function(args) {
  sub              <- args
  sub$meta_analyze <- NULL
  estimates        <- as.data.table(private$rg_estimates(sub))
  estimates[, stratum := as.character(born_at_year)]
  if (!is.null(args$meta_analyze)) {
    estimates[, share := SandwichAnalysis$new()$calculate_meta_shares(rg, rg_se, 1, args$meta_analyze)]
  }
  estimates
})

rg_points <- function(tables, args, frozen) {
  h2_value <- function(k, s, a) {
    method <- args[[paste0("h2_t", k)]]$meta_analyze
    if (is.null(method)) h2_at(tables, k, s, a) else pooled_at(tables, k, method, a)
  }
  rg <- mapply(function(s, a) {
    rg_calc$calculate_rg(
      1, cif_at(tables, pop[1], s, a), cif_at(tables, cross, s, a), cif_at(tables, pop[2], s, a), 1, 1, 1,
      h2_value(1, s, a), h2_value(2, s, a), 0.5
    )$rg
  }, frozen$stratum, frozen$age)

  if (is.null(args$meta_analyze)) setNames(rg, paste0("rg|", frozen$stratum)) else c(`rg|meta` = sum(frozen$share * rg))
}

h2_meta <- lapply(c("fixed", "random"), function(method) {
  args <- sandwich_h2_args("trait1", method)
  list(args = args, results = do.call(pipeline$run_h2, args)$results)
})

# Every functional's value with person weights scaled by `m`, named "<variant>/<output>".
oracle_points <- function(m) {
  tables <- oracle_tables(m)
  h2     <- vapply(h2_meta, function(x) pooled_at(tables, 1, x$args$meta_analyze, max(x$results$age)), numeric(1))
  rg     <- Map(function(args, frozen, name) {
    points <- rg_points(tables, args, frozen)
    setNames(points, paste0(name, "/", names(points)))
  }, rg_variants, rg_frozen, names(rg_variants))

  c(setNames(h2, c("h2_fixed/meta", "h2_random/meta")), unlist(unname(rg)))
}

# The production influence of the same functionals, columns named like oracle_points().
production_influence <- function() {
  h2 <- lapply(seq_along(h2_meta), function(j) {
    plan <- private$h2_plan(h2_meta[[j]]$args, h2_meta[[j]]$results)
    psi  <- plan_influence(pipeline, base, plan, ids)
    matrix(psi[, plan$output[!is.na(plan$output)]], ncol = 1,
           dimnames = list(NULL, paste0(c("h2_fixed", "h2_random")[j], "/meta")))
  })
  rg <- Map(function(args, name) {
    plan <- if (is.null(args$meta_analyze)) {
      private$rg_plan(args, private$rg_estimates(args))
    } else {
      private$meta_rg_plan(args)
    }
    psi <- plan_influence(pipeline, base, plan, ids)
    colnames(psi) <- paste0(name, "/", colnames(psi))
    psi
  }, rg_variants, names(rg_variants))

  do.call(cbind, c(h2, unname(rg)))
}

#=================================================================================
# Tests
#=================================================================================

describe("meta-analyzed sandwich influence", {
  points <- oracle_points(rep(1, length(ids)))
  psi    <- production_influence()

  it("covers strata with different rg ages and pooled h2 with dropped strata", {
    expect_gt(uniqueN(rg_frozen$t1_fixed$age), 1)
    a_star <- max(h2_meta[[1]]$results$age)
    expect_lt(nrow(local_h2[[1]][age == a_star]), uniqueN(local_h2[[1]]$stratum))
  })

  it("reproduces the pipeline's points with the frozen shares and ages", {
    expect_equal(points[["h2_fixed/meta"]], tail(h2_meta[[1]]$results[order(age)], 1)$h2, tolerance = 1e-12)
    expect_equal(points[["h2_random/meta"]], tail(h2_meta[[2]]$results[order(age)], 1)$h2, tolerance = 1e-12)

    for (name in c("t1_fixed", "t2_random", "both_random")) {
      expect_equal(unname(points[paste0(name, "/rg|", rg_frozen[[name]]$stratum)]), rg_frozen[[name]]$rg,
                   tolerance = 1e-12)
    }
    for (name in c("meta_fixed", "meta_random")) {
      expect_equal(points[[paste0(name, "/rg|meta")]], do.call(pipeline$run_rg, rg_variants[[name]])$results$rg,
                   tolerance = 1e-12)
    }
  })

  it("matches the Gateaux derivative of every functional in every person's weight", {
    expect_setequal(colnames(psi), names(points))
    set.seed(12)
    by_stratum <- split(ids, pool[trait == "trait1"][chmatch(ids, person_id), born_at_year])
    probes     <- unlist(lapply(by_stratum, function(x) sample(x, 3)))

    for (person in probes) {
      i          <- chmatch(person, ids)
      derivative <- gateaux(oracle_points, length(ids), i)
      expect_close(psi[i, names(derivative)], derivative, rtol = 1e-6, atol = 1e-9)
    }
  })
})

describe("sandwich variances of many outputs", {
  args    <- sandwich_h2_args("trait1")
  local   <- as.data.table(do.call(pipeline$run_h2, args)$results)
  many    <- private$h2_inputs(args, data.table(stratum = as.character(local$born_at_year), age = local$age), "h2")
  both    <- rg_variants$both_random
  terms   <- rbind(
    many$terms,
    private$rg_plan(both, private$rg_estimates(both))$terms,
    private$meta_rg_plan(rg_variants$meta_fixed)$terms
  )
  outputs <- unique(terms$output)
  cohorts <- plan_cohorts(pipeline, base)
  graph   <- private$sandwich$graph()
  n_rows  <- graph$graph$n

  it("equals the dense covariance of the CIF influences and the chain coefficients", {
    expect_gt(length(outputs), 32)

    components <- unique(terms[, .(cohort, stratum, age)])
    phi        <- matrix(0, n_rows, nrow(components))
    for (j in seq_len(nrow(components))) {
      comp <- components[j]
      tte  <- cohorts[[comp$cohort]][stratum == comp$stratum]
      phi[chmatch(tte$person_id, graph$person_id), j] <-
        SandwichAnalysis$new()$calculate_cif_influence(tte$trait_age, tte$trait_status, tte$weight, comp$age)$phi
    }
    coef <- matrix(0, nrow(components), length(outputs))
    hit  <- terms[components[, j := .I], on = .(cohort, stratum, age)]
    coef[cbind(hit$j, match(hit$output, outputs))] <- hit$coef

    pairs  <- pedigreegraph::relationship_pairs(graph$graph, max_degree = 3, ids = FALSE, progress = FALSE)
    kernel <- diag(n_rows)
    kernel[cbind(pairs$first, pairs$second)] <- 1
    kernel[cbind(pairs$second, pairs$first)] <- 1
    sigma  <- crossprod(phi, kernel %*% phi)
    dense  <- colSums(coef * (sigma %*% coef))

    fit <- private$sandwich$run(terms, cohorts)
    expect_close(fit$variance$variance[match(outputs, fit$variance$output)], dense, rtol = 1e-10, atol = 1e-12)
  })

  it("gives identical variances for every batch size, in ceil(Q / B) passes", {
    runs <- lapply(c(1, 7, 32), function(size) {
      old <- options(epimight.sandwich_batch_bytes = 16 * n_rows * size)
      on.exit(options(old))
      private$sandwich$run(terms, cohorts)
    })

    for (run in runs[-1]) expect_identical(run$variance, runs[[1]]$variance)
    expect_equal(vapply(runs, `[[`, numeric(1), "batch_size"), c(1, 7, 32))
    expect_equal(vapply(runs, `[[`, numeric(1), "passes"), ceiling(length(outputs) / c(1, 7, 32)))
  })
})

describe("with_sandwich", {
  it("reports a negative variance as NA with a warning", {
    fit <- list(variance = data.table(output = c("a", "b"), variance = c(0.0004, -1e-9)))

    expect_warning(
      results <- private$with_sandwich(data.table(h2 = c(0.3, 0.4, 0.5)), c("a", "b", NA), fit, "h2"),
      "1 sandwich variance"
    )
    expect_equal(results$sandwich_se, c(0.02, NA, NA))
    expect_equal(results$sandwich_l95, c(0.3 - 1.96 * 0.02, NA, NA))
  })
})
