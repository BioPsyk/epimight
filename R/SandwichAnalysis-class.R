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
#' @param categories Pair categories in place of `max_degree`.
#' @returns The variance of each column.
#' @keywords internal
pair_variances <- function(graph, psi, max_degree, categories = NULL) {
  columns  <- paste0("v", seq_len(ncol(psi)))
  values   <- stats::setNames(lapply(seq_len(ncol(psi)), function(q) psi[, q]), columns)
  products <- lapply(columns, function(v) paste0(c("first.", "second."), v))

  moments <- pedigreegraph::relationship_moments(
    graph,
    max_degree          = if (is.null(categories)) max_degree,
    categories          = categories,
    values              = values,
    products            = products,
    symmetric           = "canonical",
    progress            = FALSE
  )
  cross <- as.data.frame(pedigreegraph::moments_sum(moments, "category"), stats = "cross")

  unname(colSums(psi ^ 2) + 2 * unlist(cross[1, paste0("cross.first.", columns, ":second.", columns)]))
}

#' @title Pedigree-pair sandwich variances of h2 and rg estimates.
#' @description
#' Turns a term table (see `h2_terms`) into one complete influence vector per output and
#' sums each over the pedigree pairs up to `max_degree`, with a uniform kernel. The graph is
#' built on first use from the pedigree trimmed to the probands' ancestors.
#' @docType class
#' @import R6
#' @import data.table
#' @keywords internal
SandwichAnalysis <- R6::R6Class( #nolint
  "SandwichAnalysis",
  inherit = Analysis,
  private = list(
    pedigree   = NULL,
    probands   = NULL,
    max_degree = NULL,
    built      = NULL
  ),
  public = list(
    #' @description
    #' Checks that every proband has a pedigree row.
    #'
    #' @param pedigree Data.table with `person_id`, `mother_id`, `father_id` and an optional
    #'   `twin`, all strings.
    #' @param probands Every `person_id` an analysis can read.
    #' @param max_degree Highest kinship degree in the pair set.
    initialize = function(pedigree, probands, max_degree) {
      super$initialize()

      if (anyDuplicated(pedigree$person_id)) stop("The pedigree has duplicated `person_id` values")

      absent <- probands[!(probands %chin% pedigree$person_id)]

      if (length(absent) > 0) {
        stop(length(absent), " proband `person_id` values are missing from the pedigree, e.g. \"", absent[1], "\"")
      }

      private$pedigree   <- pedigree
      private$probands   <- probands
      private$max_degree <- max_degree
    },
    #' @description
    #' The trimmed pedigree graph and the `person_id` on each of its rows.
    graph = function() {
      if (is.null(private$built)) {
        private$built <- sandwich_graph(private$pedigree, private$probands, private$max_degree)
      }

      private$built
    },
    #' @description
    #' Sandwich variance of every output of a term table.
    #'
    #' Outputs with a non-finite coefficient or CIF get `NA`. The outputs are assembled and
    #' summed in batches sized by `getOption("epimight.sandwich_batch_bytes", 2^31)`.
    #'
    #' @param terms Term table with columns `output`, `cohort`, `stratum`, `age`, `k`, `coef`.
    #' @param cohorts Named list over the term table's cohort keys of the cohorts' TTE
    #'   data.tables with a `stratum` column, as `Pipeline$get_tte` returns them.
    #' @returns A list with `variance` (a data.table of `output` and `variance`), `batch_size`
    #'   and `passes`.
    run = function(terms, cohorts) {
      built  <- self$graph()
      n_rows <- built$graph$n
      budget <- getOption("epimight.sandwich_batch_bytes", 2^31)
      size   <- sandwich_batch_size(n_rows, budget)

      by_stratum <- lapply(cohorts, function(tte) {
        split(
          data.table(
            row          = chmatch(tte$person_id, built$person_id),
            stratum      = tte$stratum,
            trait_age    = tte$trait_age,
            trait_status = tte$trait_status,
            weight       = if ("weight" %in% names(tte)) tte$weight else 1
          ),
          by = "stratum", keep.by = FALSE
        )
      })

      variance <- terms[, .(variance = NA_real_, finite = all(is.finite(coef) & is.finite(k))), by = output]
      outputs  <- variance[finite == TRUE, output]
      batches  <- split(outputs, ceiling(seq_along(outputs) / size))

      for (batch in batches) {
        psi    <- assemble_influence(terms[output %chin% batch], by_stratum, n_rows)
        values <- pair_variances(built$graph, psi, private$max_degree)
        variance[.(colnames(psi)), on = "output", variance := values]
      }

      list(variance = variance[, .(output, variance)], batch_size = size, passes = length(batches))
    }
  )
)
