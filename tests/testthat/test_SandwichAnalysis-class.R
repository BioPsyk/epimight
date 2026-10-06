library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

skip_if_not_installed("pedigreegraph")

#=================================================================================
# Preparation
#=================================================================================

options(pedigreegraph.progress = FALSE)

sandwich <- SandwichAnalysis$new()
private  <- sandwich$.__enclos_env__$private
pedigree <- toy_pedigree()
probands <- pedigree[!grepl("^[fm][0-9]", person_id), person_id]
built    <- private$build_graph(pedigree, probands, 3L, trim = FALSE)

# Influence columns over the graph rows: random on probands, zero elsewhere, with one
# column whose pair terms nearly cancel its diagonal.
influence_columns <- function(person_id, q, seed = 6) {
  set.seed(seed)
  on  <- person_id %chin% probands
  psi <- matrix(rnorm(length(person_id) * q), ncol = q) * on
  psi[, q] <- ifelse(on, 1e-3 * sign(rnorm(length(person_id))), 0)
  psi
}

expect_close <- function(actual, expected, rtol, atol) {
  gap <- abs(actual - expected)
  expect_true(all(gap <= atol + rtol * abs(expected)), info = sprintf("max gap %.3g", max(gap)))
}

#=================================================================================
# Tests
#=================================================================================

describe("calculate_pair_variances", {
  psi <- influence_columns(built$person_id, 5)

  it("equals the dense brute-force sandwich over the engine's pairs", {
    expect_close(sandwich$calculate_pair_variances(built$graph, psi, 3L), brute_pair_variances(built$graph, psi, 3L),
                 rtol = 1e-10, atol = 1e-12)
  })

  it("equals the sibship cluster sandwich on the full-sib kernel", {
    sibship <- pedigree[chmatch(built$person_id, person_id), fifelse(
      is.na(mother_id) | is.na(father_id) | mother_id == "" | father_id == "",
      person_id, paste(mother_id, father_id)
    )]
    cluster <- colSums(rowsum(psi, sibship) ^ 2)

    expect_close(sandwich$calculate_pair_variances(built$graph, psi, NULL, categories = c("MZ", "FS")), cluster,
                 rtol = 1e-10, atol = 1e-12)
  })

  it("keeps a negative or nearly cancelled variance as computed", {
    trio  <- private$build_graph(data.table(person_id = c("mum", "dad", "kid"), mother_id = c(NA, NA, "mum"),
                                       father_id = c(NA, NA, "dad")), c("mum", "dad", "kid"), 1L)
    # Kid 1 and parents -a: V = 1 + 2 a^2 - 4 a, negative at a = 0.5 and near 0 at a = 1 - 1 / sqrt(2).
    a     <- c(0.5, 1 - 1 / sqrt(2) + 1e-9)
    psi   <- vapply(a, function(x) ifelse(trio$person_id == "kid", 1, -x), numeric(3))
    exact <- 1 + 2 * a ^ 2 - 4 * a

    expect_close(sandwich$calculate_pair_variances(trio$graph, psi, 1L), exact, rtol = 1e-10, atol = 1e-12)
    expect_close(brute_pair_variances(trio$graph, psi, 1L), exact, rtol = 1e-10, atol = 1e-12)
    expect_lt(sandwich$calculate_pair_variances(trio$graph, psi, 1L)[1], 0)
  })

  it("gives each column the same variance whatever its batch mates", {
    wide  <- influence_columns(built$person_id, 32, seed = 7)
    whole <- sandwich$calculate_pair_variances(built$graph, wide, 3L)

    for (size in c(1, 7)) {
      batches <- split(seq_len(32), ceiling(seq_len(32) / size))
      batched <- unlist(
        lapply(batches, function(cols) sandwich$calculate_pair_variances(built$graph, wide[, cols, drop = FALSE], 3L)),
        use.names = FALSE
      )
      expect_identical(batched, whole)
    }
  })

  it("gives the same variances on 1 and 4 threads", {
    root <- normalizePath(file.path("..", ".."))
    load <- if (file.exists(file.path(root, "DESCRIPTION"))) {
      sprintf("pkgload::load_all('%s', quiet = TRUE)", root)
    } else {
      "library(epimight)"
    }
    input <- tempfile(fileext = ".rds")
    saveRDS(list(pedigree = pedigree, probands = probands), input)

    on_threads <- function(threads) {
      output <- tempfile(fileext = ".rds")
      script <- sprintf(
        paste(
          "suppressMessages(%s)",
          "x <- readRDS('%s')",
          "sandwich <- epimight:::SandwichAnalysis$new()",
          "built <- sandwich$.__enclos_env__$private$build_graph(x$pedigree, x$probands, 3L, trim = FALSE)",
          "set.seed(8)",
          "psi <- matrix(rnorm(built$graph$n * 6), ncol = 6)",
          "v <- sandwich$calculate_pair_variances(built$graph, psi, 3L)",
          "saveRDS(list(variance = v, threads = pedigreegraph::thread_budget()), '%s')",
          sep = "; "
        ),
        load, input, output
      )
      status <- system2(
        file.path(R.home("bin"), "Rscript"), c("-e", shQuote(script)),
        env = sprintf("PEDIGREE_GRAPH_THREADS=%d", threads), stdout = FALSE, stderr = FALSE
      )
      expect_equal(status, 0)
      result <- readRDS(output)
      expect_equal(result$threads, threads)
      result$variance
    }

    expect_identical(on_threads(4), on_threads(1))
  })
})

describe("build_graph", {
  it("keeps every proband pair when trimmed to max_degree - 1 ancestor generations", {
    set.seed(9)
    youngest <- pedigree[startsWith(person_id, "g3_"), person_id]
    spread   <- c(youngest, sample(pedigree[startsWith(person_id, "g2_"), person_id], 60),
                  sample(pedigree[startsWith(person_id, "g1_"), person_id], 20))

    for (case in list(list(probands = youngest, degree = 3L), list(probands = spread, degree = 3L),
                      list(probands = spread, degree = 2L))) {
      trimmed <- private$build_graph(pedigree, case$probands, case$degree)
      full    <- private$build_graph(pedigree, case$probands, case$degree, trim = FALSE)
      values  <- setNames(rnorm(length(case$probands)), case$probands)
      on_rows <- function(person_id) matrix(ifelse(person_id %chin% case$probands, values[person_id], 0))

      expect_lt(trimmed$graph$n, full$graph$n)
      expect_equal(
        sandwich$calculate_pair_variances(trimmed$graph, on_rows(trimmed$person_id), case$degree),
        sandwich$calculate_pair_variances(full$graph, on_rows(full$person_id), case$degree),
        tolerance = 1e-12
      )
    }
  })

  it("keeps external parents, so their children stay half sibs", {
    pairs <- pedigreegraph::relationship_pairs(built$graph, max_degree = 2, progress = FALSE)
    ids   <- built$person_id

    expect_true(any(pairs$code == "MHS" & ids[pairs$first] %in% c("ext1", "ext2") &
                      ids[pairs$second] %in% c("ext1", "ext2")))
  })

  it("classifies the twins as MZ and the double first cousins as one pair", {
    pairs <- pedigreegraph::relationship_pairs(built$graph, max_degree = 3, progress = FALSE)
    key   <- paste(built$person_id[pmin(pairs$first, pairs$second)], built$person_id[pmax(pairs$first, pairs$second)])

    expect_equal(as.character(pairs$code[key == "tw1 tw2"]), "MZ")
    expect_equal(sum(key == "dc1 dc2"), 1)
    expect_false(anyDuplicated(key) > 0)
  })
})

describe("batch_size", {
  it("fits 16 bytes per row and output into the budget, between 1 and 32", {
    expect_equal(private$batch_size(1e7, 2^31), 13)
    expect_equal(private$batch_size(100, 2^31), 32)
    expect_equal(private$batch_size(1e9, 2^31), 1)
  })
})

describe("Pipeline with a pedigree", {
  golden   <- test_path("..", "data", "sandwich-golden")
  pool     <- fread(file.path(golden, "pool.csv"), colClasses = list(character = "person_id"))
  pedigree <- fread(file.path(golden, "pedigree.csv"), colClasses = "character", na.strings = "")
  python   <- fread(file.path(golden, "se.csv"))
  pipeline <- Pipeline$new(pool = pool, pedigree = pedigree)
  h2_args  <- function(trait) {
    list(
      cif_pop     = list(index_trait = trait, stratify_columns = list("born_at_year")),
      cif_fh      = list(index_trait = trait, relatives_trait = trait, relatives_kind = "FS",
                         stratify_columns = list("born_at_year")),
      relatedness = 0.5
    )
  }
  headline <- function(results) results[!is.na(sandwich_se)][order(born_at_year)]
  expect_golden <- function(results, name, estimate) {
    expected <- python[quantity == name][order(born_at_year)]
    expect_equal(results$born_at_year, expected$born_at_year)
    expect_equal(results[[estimate]], expected$point, tolerance = 1e-10)
    expect_equal(results$sandwich_se, expected$se, tolerance = 1e-8)
  }

  it("matches the Python sandwich on per-stratum h2", {
    for (k in 1:2) {
      results <- headline(do.call(pipeline$run_h2, h2_args(paste0("trait", k)))$results)
      expect_golden(results, paste0("h2_", k), "h2")
    }
  })

  it("adds sandwich columns at each stratum's last age only", {
    results <- do.call(pipeline$run_h2, h2_args("trait1"))$results
    last    <- results[, .(age = max(age)), by = born_at_year]

    expect_equal(results[!is.na(sandwich_se), .(born_at_year, age)][order(born_at_year)], last[order(born_at_year)])
    expect_equal(results$sandwich_l95, results$h2 - 1.96 * results$sandwich_se)
    expect_equal(results$sandwich_u95, results$h2 + 1.96 * results$sandwich_se)
  })

  it("reports the batch size and pass count, also on a cache hit", {
    fresh  <- Pipeline$new(pool = pool, pedigree = pedigree)
    first  <- do.call(fresh$run_h2, h2_args("trait2"))
    cached <- do.call(fresh$run_h2, h2_args("trait2"))

    expect_equal(first$metadata$sandwich, list(batch_size = 32, passes = 1L))
    expect_equal(cached$metadata$sandwich, first$metadata$sandwich)
  })

  it("handles an empty string as a stratum label", {
    labelled <- copy(pool)[, region := ifelse(born_at_year == 2001L, "", "north")]
    by_label <- function(column) {
      args <- h2_args("trait1")
      args$cif_pop$stratify_columns <- list(column)
      args$cif_fh$stratify_columns  <- list(column)
      results <- do.call(Pipeline$new(pool = labelled, pedigree = pedigree)$run_h2, args)$results
      results[!is.na(sandwich_se)][order(h2)]$sandwich_se
    }

    expect_equal(by_label("region"), by_label("born_at_year"))
    expect_length(by_label("region"), 2)
  })

  it("adds the columns to top-level calls only, in one pass", {
    fresh <- Pipeline$new(pool = pool, pedigree = pedigree)
    rg    <- fresh$run_rg(
      h2_t1 = h2_args("trait1"), h2_t2 = h2_args("trait2"), relatedness = 0.5,
      cif_cross = list(index_trait = "trait1", relatives_trait = "trait2", relatives_kind = "FS",
                       stratify_columns = list("born_at_year"))
    )
    nested <- fresh$get_results("h2", h2_args("trait1"))
    asked  <- do.call(fresh$run_h2, h2_args("trait1"))

    pooled <- Pipeline$new(pool = pool, pedigree = pedigree)
    pooled$run_rg(
      h2_t1 = c(h2_args("trait1"), meta_analyze = "fixed"), h2_t2 = h2_args("trait2"), relatedness = 0.5,
      cif_cross = list(index_trait = "trait1", relatives_trait = "trait2", relatives_kind = "FS",
                       stratify_columns = list("born_at_year"))
    )

    expect_equal(rg$metadata$sandwich$passes, 1L)
    expect_false("sandwich_se" %in% names(nested))
    expect_false("sandwich_se" %in% names(pooled$get_results("h2", h2_args("trait1"))))
    expect_true("sandwich_se" %in% names(asked$results))
    expect_true("sandwich_se" %in% names(fresh$get_results("h2", h2_args("trait1"))))
  })

  it("leaves results unchanged without a pedigree", {
    plain <- Pipeline$new(pool = pool)
    with  <- do.call(pipeline$run_h2, h2_args("trait1"))$results
    alone <- do.call(plain$run_h2, h2_args("trait1"))

    expect_equal(alone$results, with[, !c("sandwich_se", "sandwich_l95", "sandwich_u95")])
    expect_null(alone$metadata$sandwich)
  })

  it("returns an empty h2 table, as without a pedigree, when every h2 is dropped", {
    # Relatives' history mostly on unaffected probands puts the family-history CIF below the
    # population's, so every h2 is negative and dropped.
    set.seed(13)
    flat <- copy(pool)[, relatives_n_trait := as.integer(
      ifelse(runif(.N) < ifelse(trait_status == 1L, 0.02, 0.9), relatives_n, 0L)
    )]
    alone <- do.call(Pipeline$new(pool = flat)$run_h2, h2_args("trait1"))$results
    with  <- do.call(Pipeline$new(pool = flat, pedigree = pedigree)$run_h2, h2_args("trait1"))$results

    expect_equal(nrow(alone), 0)
    expect_equal(nrow(with), 0)
    expect_true(all(c("sandwich_se", "sandwich_l95", "sandwich_u95") %in% names(with)))
  })

  it("refuses a max_degree the pair engine cannot reach", {
    expect_error(Pipeline$new(pool = pool, pedigree = pedigree, max_degree = 6L), "larger than maximum")
  })

  it("drops the pedigree once the graph is built", {
    analysis <- SandwichAnalysis$new(pedigree, pedigree$person_id, 3L)
    analysis$graph()

    expect_null(analysis$.__enclos_env__$private$pedigree)
    expect_null(analysis$.__enclos_env__$private$probands)
    expect_s3_class(analysis$graph()$graph, "pedigree_graph")
  })

  it("refuses a pedigree that misses probands", {
    expect_error(Pipeline$new(pool = pool, pedigree = pedigree[-(1:3)]), "3 proband `person_id` values are missing")
  })

  it("matches the Python sandwich on per-stratum rg", {
    rg <- pipeline$run_rg(
      h2_t1 = h2_args("trait1"), h2_t2 = h2_args("trait2"), relatedness = 0.5,
      cif_cross = list(index_trait = "trait1", relatives_trait = "trait2", relatives_kind = "FS",
                       stratify_columns = list("born_at_year"))
    )
    expect_golden(headline(rg$results), "rg", "rg")
  })
})
