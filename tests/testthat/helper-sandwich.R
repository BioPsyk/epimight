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

# The cohorts in the layout `SandwichAnalysis$assemble_influence` takes, with person i on pedigree row i.
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

# Four generations with random mating (full and half sibs), external mothers shared by
# half sibs, a pair of double first cousins and a pair of MZ twins. Ids are strings.
toy_pedigree <- function(seed = 5) {
  set.seed(seed)
  founders <- data.table(person_id = c(paste0("f", 1:30), paste0("m", 1:30)), mother_id = NA_character_,
                         father_id = NA_character_)
  women <- paste0("f", 1:30)
  men   <- paste0("m", 1:30)
  generations <- list(founders)

  for (g in 1:3) {
    n    <- if (g == 1) 120 else 200
    kids <- data.table(person_id = paste0("g", g, "_", seq_len(n)), mother_id = sample(women, n, TRUE),
                       father_id = sample(men, n, TRUE))
    generations[[g + 1]] <- kids
    women <- kids$person_id[seq(1, n, 2)]
    men   <- kids$person_id[seq(2, n, 2)]
  }

  specials <- data.table(
    person_id = c("ext1", "ext2", "ext3", "sis1", "sis2", "bro1", "bro2", "dc1", "dc2", "tw1", "tw2"),
    mother_id = c("xm1", "xm1", "xm2", "f1", "f1", "f2", "f2", "sis1", "sis2", "g1_1", "g1_1"),
    father_id = c(NA, NA, "", "m1", "m1", "m2", "m2", "bro1", "bro2", "g1_2", "g1_2"),
    twin      = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "tw2", "tw1")
  )

  rbind(rbindlist(generations), specials, fill = TRUE)
}

# Dense brute-force sandwich variance: every pair the engine reports up to `max_degree`
# (or in `categories`), plus the diagonal, as one n x n kernel.
brute_pair_variances <- function(graph, psi, max_degree, categories = NULL) {
  pairs  <- pedigreegraph::relationship_pairs(graph, max_degree = max_degree, ids = FALSE, progress = FALSE)
  if (!is.null(categories)) pairs <- pairs[pairs$code %in% categories, ]
  kernel <- diag(nrow(psi))
  kernel[cbind(pairs$first, pairs$second)] <- 1
  kernel[cbind(pairs$second, pairs$first)] <- 1
  diag(crossprod(psi, kernel %*% psi))
}

# A two-trait pool over `person_id` with FS family history and two birth-year strata.
toy_pool <- function(person_id, seed = 10) {
  set.seed(seed)
  n       <- length(person_id)
  persons <- data.table(person_id = person_id, born_at_year = rep_len(c(2001L, 2002L), n),
                        relatives_n = sample(1:4, n, TRUE))
  traits <- lapply(c("trait1", "trait2"), function(trait) {
    u      <- runif(n)
    status <- ifelse(u < 0.3, 1L, ifelse(u < 0.42, 2L, 0L))
    persons[, .(
      person_id, trait, born_at_year, relatives_kind = "FS", relatives_n,
      relatives_n_trait = as.integer(rbinom(n, relatives_n, 0.35 + 0.2 * (status == 1L))),
      trait_status = status,
      trait_age = as.numeric(ifelse(status == 0L, 20L, sample(1:20, n, TRUE)))
    )]
  })
  rbindlist(traits)
}

# A two-trait FS pool over the probands of `pedigree` (rows with a known father) in three
# birth-year strata of unequal size, censored at ages 20, 17 and 14, with a familial
# liability that raises both traits and the relatives' case counts.
family_pool <- function(pedigree, seed = 11) {
  set.seed(seed)
  persons <- pedigree[!is.na(father_id), .(person_id)]
  n       <- nrow(persons)
  persons[, `:=`(
    born_at_year = sample(2001:2003, n, TRUE, prob = c(0.5, 0.3, 0.2)),
    relatives_n  = sample(1:4, n, TRUE),
    familial     = runif(n)
  )]
  persons[, end := c(20L, 17L, 14L)[born_at_year - 2000L]]

  rbindlist(lapply(c("trait1", "trait2"), function(trait) {
    u      <- runif(n)
    p1     <- 0.1 + 0.5 * persons$familial
    status <- ifelse(u < p1, 1L, ifelse(u < p1 + 0.12, 2L, 0L))
    persons[, .(
      person_id, trait, born_at_year, relatives_kind = "FS", relatives_n,
      relatives_n_trait = as.integer(rbinom(n, relatives_n, 0.1 + 0.7 * familial)),
      trait_status = status,
      trait_age = as.numeric(ifelse(status == 0L, end, vapply(end, function(e) sample.int(e, 1), integer(1))))
    )]
  }))
}

# Run-path arguments in the shape run_default_rg builds, for traits "trait1" and "trait2".
sandwich_h2_args <- function(trait, meta = NULL) {
  args <- list(
    cif_pop     = list(index_trait = trait, stratify_columns = list("born_at_year")),
    cif_fh      = list(index_trait = trait, relatives_trait = trait, relatives_kind = "FS",
                       stratify_columns = list("born_at_year")),
    relatedness = 0.5
  )
  args$meta_analyze <- meta
  args
}

sandwich_rg_args <- function(meta_t1 = NULL, meta_t2 = NULL, meta = NULL) {
  args <- list(
    h2_t1       = sandwich_h2_args("trait1", meta_t1),
    h2_t2       = sandwich_h2_args("trait2", meta_t2),
    cif_cross   = list(index_trait = "trait1", relatives_trait = "trait2", relatives_kind = "FS",
                       stratify_columns = list("born_at_year")),
    relatedness = 0.5
  )
  args$meta_analyze <- meta
  args
}

pipeline_private <- function(pipeline) pipeline$.__enclos_env__$private

# The five cohorts of `args` keyed like the term tables, with a stratum column.
plan_cohorts <- function(pipeline, args) {
  private <- pipeline_private(pipeline)
  cohorts <- list()

  for (one in private$rg_cifs(args)) {
    tte <- do.call(pipeline$get_tte, one)
    tte[, stratum := private$stratum_key(tte, one$stratify_columns)]
    if (!("weight" %in% names(tte))) tte[, weight := 1]
    cohorts[[private$cache_key("cif", one)]] <- tte
  }

  cohorts
}

# Influence vectors of a plan's outputs, one row per id of `ids`.
plan_influence <- function(pipeline, args, plan, ids) {
  by_stratum <- lapply(plan_cohorts(pipeline, args), function(tte) {
    rows <- chmatch(tte$person_id, ids)
    split(tte[, .(row = rows, stratum, trait_age, trait_status, weight)], by = "stratum", keep.by = FALSE)
  })
  SandwichAnalysis$new()$assemble_influence(plan$terms, by_stratum, length(ids))
}
