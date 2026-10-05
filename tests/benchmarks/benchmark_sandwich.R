# Wall time and pair-sum passes of run_default_rg with and without sandwich SEs.
#
# Run from the package root, one configuration per process, so that the process's peak RSS
# belongs to one run:
#
#   /usr/bin/time -f "%M" Rscript tests/benchmarks/benchmark_sandwich.R \
#     <probands> <strata> <meta: none|fixed> <mode: native|sandwich|pairlist> <output.tsv>
#
# `sandwich` sums over pairs with relationship_moments; `pairlist` swaps in the pair list
# (relationship_pairs plus a gather of both members), to show what the moments pass saves.
# The pedigree has four generations of probands / 2 people; the two youngest are the
# probands. Strata are censored at different ages, so their rg ages differ. Generated data
# are cached under tmp/.

suppressMessages({
  library(data.table, quietly = TRUE, warn.conflicts = FALSE)
  pkgload::load_all(".", quiet = TRUE)
})
options(warn = 1, pedigreegraph.progress = FALSE)

args     <- commandArgs(trailingOnly = TRUE)
probands <- as.integer(args[1])
strata   <- as.integer(args[2])
meta     <- args[3]
mode     <- args[4]
output   <- args[5]

generate <- function(probands, strata, seed = 1) {
  set.seed(seed)
  per_gen <- probands %/% 2
  ids     <- function(g) paste0("g", g, "_", seq_len(per_gen))
  ped     <- list(data.table(person_id = ids(0), mother_id = NA_character_, father_id = NA_character_))

  for (g in 1:3) {
    prev   <- ids(g - 1)
    mother <- prev[seq(1, per_gen, 2)]
    father <- prev[seq(2, per_gen, 2)]
    couple <- sample(length(mother), per_gen, TRUE)
    # Most children come from a couple (full sibs), the rest from random matings (half sibs).
    random <- runif(per_gen) < 0.2
    ped[[g + 1]] <- data.table(
      person_id = ids(g),
      mother_id = mother[couple],
      father_id = ifelse(random, sample(father, per_gen, TRUE), father[couple])
    )
  }
  ped <- rbindlist(ped)

  persons <- ped[startsWith(person_id, "g2_") | startsWith(person_id, "g3_")]
  n       <- nrow(persons)
  stratum <- sample.int(strata, n, TRUE)
  family  <- runif(length(unique(persons$mother_id)))[match(persons$mother_id, unique(persons$mother_id))]
  end     <- 40L + (stratum %% 7L)

  pool <- rbindlist(lapply(c("SCZ", "CAD"), function(trait) {
    p1     <- 0.05 + 0.25 * family
    u      <- runif(n)
    status <- ifelse(u < p1, 1L, ifelse(u < p1 + 0.1, 2L, 0L))
    rel_n  <- sample.int(4, n, TRUE)
    data.table(
      person_id         = persons$person_id,
      trait             = trait,
      trait_status      = status,
      trait_age         = as.numeric(ifelse(status == 0L, end, pmax(1L, floor(runif(n) * end)))),
      relatives_kind    = "siblings",
      relatives_n       = rel_n,
      relatives_n_trait = as.integer(rbinom(n, rel_n, 0.05 + 0.4 * family)),
      birth_year        = 1960L + stratum
    )
  }))

  list(pedigree = ped, pool = pool)
}

cache <- sprintf("tmp/benchmark_sandwich_%d_%d.rds", probands, strata)
if (!file.exists(cache)) {
  dir.create("tmp", showWarnings = FALSE)
  saveRDS(generate(probands, strata), cache)
}
data <- readRDS(cache)

namespace <- asNamespace("epimight")
moments   <- get("pair_variances", namespace)
pairlist  <- function(graph, psi, max_degree, categories = NULL) {
  pairs <- pedigreegraph::relationship_pairs(graph, max_degree = max_degree, ids = FALSE, progress = FALSE)
  cross <- colSums(psi[pairs$first, , drop = FALSE] * psi[pairs$second, , drop = FALSE])
  unname(colSums(psi ^ 2) + 2 * cross)
}
tally   <- new.env()
counted <- function(graph, psi, ...) {
  tally$passes  <- tally$passes + 1L
  tally$columns <- tally$columns + ncol(psi)
  (if (mode == "pairlist") pairlist else moments)(graph, psi, ...)
}
tally$passes  <- 0L
tally$columns <- 0L
unlockBinding("pair_variances", namespace)
assign("pair_variances", counted, envir = namespace)

pipeline <- if (mode == "native") {
  Pipeline$new(pool = data$pool)
} else {
  Pipeline$new(pool = data$pool, pedigree = data$pedigree)
}
rg_args <- list(
  heritability1    = list(trait = "SCZ", relatives_kind = "siblings", relatedness = 0.5),
  heritability2    = list(trait = "CAD", relatives_kind = "siblings", relatedness = 0.5),
  stratify_columns = list("birth_year")
)
if (meta != "none") rg_args$meta_analyze <- meta

started <- proc.time()[["elapsed"]]
rg      <- do.call(pipeline$run_default_rg, rg_args)
wall    <- proc.time()[["elapsed"]] - started

row <- data.table(
  probands      = probands,
  strata        = strata,
  meta          = meta,
  mode          = mode,
  threads       = pedigreegraph::thread_budget(),
  wall_s        = round(wall, 3),
  passes        = tally$passes,
  columns       = tally$columns,
  batch_size    = if (is.null(rg$metadata$sandwich)) NA else rg$metadata$sandwich$batch_size,
  pedigree_rows = nrow(data$pedigree)
)
fwrite(row, output, append = file.exists(output), sep = "\t")
print(row)
