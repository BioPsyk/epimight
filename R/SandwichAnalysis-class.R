#' Rows of a pedigree to keep: the given rows plus `depth` generations of their ancestors.
#'
#' Two probands related at degree `d` share an ancestor at most `d - 1` generations above
#' one of them, unless that ancestor is a proband, so `depth = d - 1` keeps every pair up to
#' degree `d`. A dropped ancestor's id stays on its children as an external parent.
#'
#' @param mother_row,father_row Each row's parent row, `NA` when the parent has no row.
#' @param keep Logical per row: the rows to start from.
#' @param depth Generations of ancestors to add.
#' @returns Logical per row.
#' @keywords internal
ancestor_trim <- function(mother_row, father_row, keep, depth) {
  rows <- which(keep)

  for (generation in seq_len(depth)) {
    parents     <- unique(c(mother_row[rows], father_row[rows]))
    rows        <- parents[!is.na(parents) & !keep[parents]]
    keep[rows]  <- TRUE
  }

  keep
}

#' Pedigree graph for the sandwich pair sum.
#'
#' Ids are strings and are coded to integers here. Parents with no row of their own stay
#' as external parents, so their children are still siblings.
#'
#' @param pedigree Data.table with `person_id`, `mother_id`, `father_id` and an optional
#'   `twin` (the co-twin's id); `NA` or `""` marks an unknown parent or no twin.
#' @param probands Ids of everyone whose influence can be non-zero.
#' @param max_degree The degree the pair sum reaches, which sets the ancestor trim.
#' @param trim `FALSE` keeps every row.
#' @returns A list with the `graph` and `person_id`, the id on each graph row.
#' @keywords internal
sandwich_graph <- function(pedigree, probands, max_degree, trim = TRUE) {
  missing_to_na <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
  ids    <- pedigree$person_id
  mother <- missing_to_na(pedigree$mother_id)
  father <- missing_to_na(pedigree$father_id)
  twin   <- if ("twin" %in% names(pedigree)) missing_to_na(pedigree$twin) else rep(NA_character_, length(ids))

  keep <- if (trim) {
    ancestor_trim(chmatch(mother, ids), chmatch(father, ids), ids %chin% probands, max_degree - 1L)
  } else {
    rep(TRUE, length(ids))
  }

  universe <- unique(c(ids, mother, father))
  universe <- universe[!is.na(universe)]
  code     <- function(x) chmatch(x[keep], universe)
  twin     <- ifelse(twin %chin% ids[keep], twin, NA_character_)
  graph    <- pedigreegraph::pedigree_graph(data.frame(
    id = code(ids), mother = code(mother), father = code(father), twin = code(twin)
  ))

  list(graph = graph, person_id = universe[graph$id])
}

#' Outputs per pair-sum pass that fit a memory budget.
#'
#' A pass holds its columns as doubles in R and as a quantized int64 copy in the binding,
#' about 16 bytes per pedigree row and column.
#'
#' @keywords internal
sandwich_batch_size <- function(n_rows, budget) {
  max(1, min(32, floor(budget / (16 * n_rows))))
}

#' Sandwich variance of each influence column over pedigree pairs.
#'
#' `V_q = sum_i psi_iq^2 + 2 sum_{(i, j)} psi_iq psi_jq` over each pair of the given
#' categories once, from one `relationship_moments` pass that never builds the pairs.
#'
#' @param graph A `pedigree_graph`.
#' @param psi Graph rows by at most 32 outputs.
#' @param max_degree Highest kinship degree in the pair set.
#' @param budget Accumulator memory for the pass, in bytes.
#' @param categories Pair categories in place of `max_degree`.
#' @returns The variance of each column.
#' @keywords internal
pair_variances <- function(graph, psi, max_degree, budget = 2^31, categories = NULL) {
  columns  <- paste0("v", seq_len(ncol(psi)))
  values   <- setNames(lapply(seq_len(ncol(psi)), function(q) psi[, q]), columns)
  products <- lapply(columns, function(v) paste0(c("first.", "second."), v))

  moments <- pedigreegraph::relationship_moments(
    graph,
    max_degree          = if (is.null(categories)) max_degree,
    categories          = categories,
    values              = values,
    products            = products,
    symmetric           = "canonical",
    memory_budget_bytes = budget,
    progress            = FALSE
  )
  cross <- as.data.frame(pedigreegraph::moments_sum(moments, "category"), stats = "cross")

  unname(colSums(psi ^ 2) + 2 * unlist(cross[1, paste0("cross.first.", columns, ":second.", columns)]))
}
