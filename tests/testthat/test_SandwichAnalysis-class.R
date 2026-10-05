library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)

skip_if_not_installed("pedigreegraph")

#=================================================================================
# Preparation
#=================================================================================

options(pedigreegraph.progress = FALSE)

pedigree <- toy_pedigree()
probands <- pedigree[!grepl("^[fm][0-9]", person_id), person_id]
built    <- sandwich_graph(pedigree, probands, 3L, trim = FALSE)

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

describe("pair_variances", {
  psi <- influence_columns(built$person_id, 5)

  it("equals the dense brute-force sandwich over the engine's pairs", {
    expect_close(pair_variances(built$graph, psi, 3L), brute_pair_variances(built$graph, psi, 3L),
                 rtol = 1e-10, atol = 1e-12)
  })

  it("equals the sibship cluster sandwich on the full-sib kernel", {
    sibship <- pedigree[chmatch(built$person_id, person_id), fifelse(
      is.na(mother_id) | is.na(father_id) | mother_id == "" | father_id == "",
      person_id, paste(mother_id, father_id)
    )]
    cluster <- colSums(rowsum(psi, sibship) ^ 2)

    expect_close(pair_variances(built$graph, psi, NULL, categories = c("MZ", "FS")), cluster,
                 rtol = 1e-10, atol = 1e-12)
  })

  it("gives each column the same variance whatever its batch mates", {
    wide  <- influence_columns(built$person_id, 32, seed = 7)
    whole <- pair_variances(built$graph, wide, 3L)

    for (size in c(1, 7)) {
      batches <- split(seq_len(32), ceiling(seq_len(32) / size))
      batched <- unlist(
        lapply(batches, function(cols) pair_variances(built$graph, wide[, cols, drop = FALSE], 3L)),
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
          "built <- epimight:::sandwich_graph(x$pedigree, x$probands, 3L, trim = FALSE)",
          "set.seed(8)",
          "psi <- matrix(rnorm(built$graph$n * 6), ncol = 6)",
          "v <- epimight:::pair_variances(built$graph, psi, 3L)",
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

describe("sandwich_graph", {
  it("keeps every proband pair when trimmed to max_degree - 1 ancestor generations", {
    youngest <- pedigree[startsWith(person_id, "g3_"), person_id]
    trimmed  <- sandwich_graph(pedigree, youngest, 3L)
    full     <- sandwich_graph(pedigree, youngest, 3L, trim = FALSE)
    set.seed(9)
    values   <- setNames(rnorm(length(youngest)), youngest)
    on_rows  <- function(person_id) matrix(ifelse(person_id %chin% youngest, values[person_id], 0))

    expect_lt(trimmed$graph$n, full$graph$n)
    expect_equal(
      pair_variances(trimmed$graph, on_rows(trimmed$person_id), 3L),
      pair_variances(full$graph, on_rows(full$person_id), 3L),
      tolerance = 1e-12
    )
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

describe("sandwich_batch_size", {
  it("fits 16 bytes per row and output into the budget, between 1 and 32", {
    expect_equal(sandwich_batch_size(1e7, 2^31), 13)
    expect_equal(sandwich_batch_size(100, 2^31), 32)
    expect_equal(sandwich_batch_size(1e9, 2^31), 1)
  })
})
