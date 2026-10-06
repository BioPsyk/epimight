# Column names the term tables use inside data.table expressions.
utils::globalVariables(c(
  ".", "age", "coef", "cohort", "cross", "fh", "from", "h2_t1_output", "h2_t2_output", "i.coef", "k",
  "k_cross", "k_fh", "k_pop", "k_pop1", "k_pop2", "output", "pop", "pop1", "pop2", "stratum", "to"
))

#' @title Pedigree-pair sandwich standard errors of h2 and rg estimates.
#' @description
#' Every h2 and rg estimate is a smooth function of weighted Aalen-Johansen CIF values, so
#' each has a first-order expansion `estimate - truth ~ sum_i phi_i` over the probands of
#' the cohorts it reads. The `calculate_*` methods give the `phi` of one CIF and the
#' chain-rule coefficients that carry it into h2, rg and their meta-analyses. A term table
#' (see `h2_terms`) collects those coefficients, `assemble_influence` turns it into one
#' complete influence vector per output, and `run` sums each over the pedigree pairs up to
#' `max_degree`, with a uniform kernel. The graph is built on first use from the pedigree
#' trimmed to the probands' ancestors.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @keywords internal
SandwichAnalysis <- R6::R6Class( #nolint
  "SandwichAnalysis",
  inherit = Analysis,
  private = list(
    pedigree   = NULL,
    probands   = NULL,
    max_degree = NULL,
    built      = NULL,
    # Weighted sums of `weight` per 1-based `index`, as a vector of length `n`.
    sum_by_index = function(index, weight, n) {
      out  <- numeric(n)
      sums <- rowsum(weight, index)
      out[as.integer(rownames(sums))] <- sums[, 1]
      out
    },
    # Rows of a pedigree to keep: the rows in `keep` plus `depth` generations of their
    # ancestors. Two probands related at degree `d` share an ancestor at most `d - 1`
    # generations above one of them, unless that ancestor is a proband, so `depth = d - 1`
    # keeps every pair up to degree `d`. A dropped ancestor's id stays on its children as an
    # external parent. `mother_row` and `father_row` are each row's parent row, `NA` when the
    # parent has no row.
    trim_to_ancestors = function(mother_row, father_row, keep, depth) {
      rows <- which(keep)

      for (generation in seq_len(depth)) {
        parents    <- unique(c(mother_row[rows], father_row[rows]))
        rows       <- parents[!is.na(parents) & !keep[parents]]
        keep[rows] <- TRUE
      }

      keep
    },
    # Pedigree graph for the pair sum, as a list with the `graph` and `person_id`, the id on
    # each graph row. Ids are strings and are coded to integers here. Parents with no row of
    # their own stay as external parents, so their children are still siblings. `probands`
    # are the ids whose influence can be non-zero; `max_degree` sets the ancestor trim and
    # `trim = FALSE` keeps every row.
    build_graph = function(pedigree, probands, max_degree, trim = TRUE) {
      missing_to_na <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
      ids    <- pedigree$person_id
      mother <- missing_to_na(pedigree$mother_id)
      father <- missing_to_na(pedigree$father_id)
      twin   <- if ("twin" %in% names(pedigree)) {
        missing_to_na(pedigree$twin)
      } else {
        rep(NA_character_, length(ids))
      }

      keep <- if (trim) {
        private$trim_to_ancestors(
          chmatch(mother, ids), chmatch(father, ids), ids %chin% probands, max_degree - 1L
        )
      } else {
        rep(TRUE, length(ids))
      }

      universe <- unique(c(ids, mother, father))
      universe <- universe[!is.na(universe)]
      code     <- function(x) chmatch(x[keep], universe)
      twin     <- ifelse(twin %chin% ids[keep], twin, NA_character_)
      graph    <- pedigreegraph::pedigree_graph(data.frame(
        id     = code(ids),
        mother = code(mother),
        father = code(father),
        twin   = code(twin)
      ))

      list(graph = graph, person_id = universe[graph$id])
    },
    # Outputs per pair-sum pass that fit a memory budget. A pass holds its columns as doubles
    # in R and as a quantized int64 copy in the binding, about 16 bytes per pedigree row and
    # column.
    batch_size = function(n_rows, budget) {
      max(1, min(32, floor(budget / (16 * n_rows))))
    }
  ),
  public = list(
    #' @description
    #' Creates the analysis. Without a pedigree only the calculators can be used.
    #'
    #' @param pedigree Data.table with `person_id`, `mother_id`, `father_id` and an optional
    #'   `twin` (the co-twin's id), all strings; `NA` or `""` marks an unknown parent or no
    #'   twin. Every proband needs a row.
    #' @param probands Every `person_id` an analysis can read.
    #' @param max_degree Highest kinship degree in the pair set.
    initialize = function(pedigree = NULL, probands = NULL, max_degree = NULL) {
      super$initialize()

      if (is.null(pedigree)) return(invisible(self))

      if (anyDuplicated(pedigree$person_id)) stop("The pedigree has duplicated `person_id` values")

      absent <- probands[!(probands %chin% pedigree$person_id)]

      if (length(absent) > 0) {
        stop(
          length(absent), " proband `person_id` values are missing from the pedigree, e.g. \"",
          absent[1], "\""
        )
      }

      private$pedigree   <- pedigree
      private$probands   <- probands
      private$max_degree <- max_degree
    },
    #' @description
    #' Per-row influence of the cause-1 cumulative incidence at the given ages.
    #'
    #' The value reported at age `a` is the left limit
    #' `F(a-) = sum_{t < a} S(t-) dN1(t) / Y(t)`, with all-cause survival `S`, which is what
    #' `CumulativeIncidenceAnalysis` reports for both the weighted and the `cmprsk` path. A
    #' competing event (`trait_status == 2`) enters through `S` only. `phi[i, j]` carries row
    #' `i`'s share `w_i / sum(w)` of the cohort weight, so scaling row `i`'s weight by
    #' `1 + eps` moves the CIF at `ages[j]` by `eps * phi[i, j]` to first order, and the CIF
    #' error is `sum_i phi[i, j]` without a `1 / n`.
    #'
    #' @param trait_age Integer event or censoring age per row.
    #' @param trait_status 0 censored, 1 the event of interest, 2 a competing event.
    #' @param weight Row weight; 1 for an unweighted cohort.
    #' @param ages Ages to evaluate the CIF at.
    #' @returns A list with `phi`, a `length(trait_age)` by `length(ages)` matrix, and `cif`,
    #'   the plug-in CIF at each of `ages`.
    calculate_cif_influence = function(trait_age, trait_status, weight, ages) {
      trait_age <- as.integer(trait_age)
      n         <- length(trait_age)
      phi       <- matrix(0, n, length(ages))
      cif       <- numeric(length(ages))

      if (n == 0) return(list(phi = phi, cif = cif))

      age_count  <- max(trait_age) + 1L
      age_index  <- trait_age + 1L
      weight_sum <- sum(weight)
      is_event_1 <- trait_status == 1
      is_event_n <- trait_status != 0

      weight_all     <- private$sum_by_index(age_index, weight, age_count)
      weight_event_1 <- private$sum_by_index(age_index[is_event_1], weight[is_event_1], age_count)
      weight_event_n <- private$sum_by_index(age_index[is_event_n], weight[is_event_n], age_count)

      at_risk       <- rev(cumsum(rev(weight_all)))
      at_risk_share <- at_risk / weight_sum
      live          <- at_risk > 0
      hazard_n      <- ifelse(live, weight_event_n / at_risk, 0)
      hazard_1      <- ifelse(live, weight_event_1 / at_risk, 0)

      surv_before <- c(1, cumprod(1 - hazard_n)[-age_count])
      increment   <- surv_before * hazard_1
      # The exact discrete-time factor of the hazard's derivative, 1 / (Y / W * (1 - lambda)).
      hazard_factor      <- ifelse(live & hazard_n < 1, 1 / (at_risk_share * (1 - hazard_n)), 0)
      increment_per_risk <- ifelse(live, increment / at_risk_share, 0)
      event_jump         <- ifelse(live, surv_before / at_risk_share, 0)

      cif_acc                <- cumsum(increment)
      hazard_acc             <- cumsum(hazard_n * hazard_factor)
      hazard_acc_before      <- c(0, hazard_acc[-age_count])
      increment_per_risk_acc <- cumsum(increment_per_risk)
      increment_hazard_acc   <- cumsum(increment * hazard_acc_before)
      weight_share           <- weight / weight_sum

      for (j in seq_along(ages)) {
        if (ages[j] < 1) next

        # 1-based index of the last age before ages[j], capped at the last observed age
        last     <- min(ages[j], age_count)
        cif_last <- cif_acc[last]
        capped   <- pmin(age_index, last)
        capped_1 <- pmin(age_index + 1L, last)

        # The row's own event, its time at risk, its all-cause event's effect on the later
        # survival, and that effect's compensator.
        own_event     <- (is_event_1 & age_index <= last) * event_jump[age_index]
        at_risk_term  <- increment_per_risk_acc[capped]
        survival_term <- (is_event_n & age_index < last) * hazard_factor[age_index] *
          (cif_last - cif_acc[capped])
        compensator   <- increment_hazard_acc[capped_1] + hazard_acc[capped] * (cif_last - cif_acc[capped_1])

        phi[, j] <- weight_share * (own_event - at_risk_term - survival_term + compensator)
        cif[j]   <- cif_last
      }

      list(phi = phi, cif = cif)
    },
    #' @description
    #' Central-difference Jacobian of a row-vectorized function.
    #'
    #' `f` takes a named list of equal-length numeric vectors and returns one value per row.
    #' Rows are independent, so every row's partial in one input comes from a single pair of
    #' calls. For an input below `1000 * step` the step is a thousandth of the input, so it
    #' never crosses zero (a CIF or h2 below 1e-6 would otherwise give `NaN`) and stays small
    #' against it.
    #'
    #' @param f Function of a named list of numeric vectors.
    #' @param inputs Named list of numeric vectors, one value per row.
    #' @param step Absolute step of the central difference.
    #' @returns A rows by inputs matrix of partial derivatives.
    calculate_jacobian = function(f, inputs, step = 1e-6) {
      rows <- length(inputs[[1]])

      if (rows == 0) {
        return(matrix(numeric(0), nrow = 0, ncol = length(inputs), dimnames = list(NULL, names(inputs))))
      }

      grad <- vapply(names(inputs), function(name) {
        h            <- pmin(step, abs(inputs[[name]]) * 1e-3)
        up           <- inputs
        down         <- inputs
        up[[name]]   <- up[[name]] + h
        down[[name]] <- down[[name]] - h
        (f(up) - f(down)) / (2 * h)
      }, numeric(rows))

      matrix(grad, nrow = rows, dimnames = list(NULL, names(inputs)))
    },
    #' @description
    #' Gradient of `HeritabilityAnalysis$calculate_h2`'s point estimate in its two CIFs. The
    #' case counts only enter the native SE, so they are held at 1.
    #'
    #' @param k_pop Population CIF per row.
    #' @param k_fh Family-history CIF per row.
    #' @param rc Relationship coefficient.
    #' @param calculator The heritability analysis to differentiate.
    #' @returns A matrix with columns `pop` and `fh`.
    calculate_h2_gradient = function(k_pop, k_fh, rc, calculator = HeritabilityAnalysis$new()) {
      ones <- rep(1, length(k_pop))

      self$calculate_jacobian(
        function(x) suppressWarnings(calculator$calculate_h2(ones, x$pop, x$fh, ones, ones, rc)$h2),
        list(pop = k_pop, fh = k_fh)
      )
    },
    #' @description
    #' Gradient of `GeneticCorrelationAnalysis$calculate_rg`'s rg in its three CIFs and two
    #' h2s. The h2 inputs are free: a caller that computed them from the same CIFs chains
    #' their own influence in through the `h2_t1` and `h2_t2` columns.
    #'
    #' @param k_pop1 Trait 1 population CIF per row.
    #' @param k_cross Cross-trait CIF per row.
    #' @param k_pop2 Trait 2 population CIF per row.
    #' @param h2_t1 Trait 1 heritability per row.
    #' @param h2_t2 Trait 2 heritability per row.
    #' @param rc Relationship coefficient.
    #' @param calculator The genetic correlation analysis to differentiate.
    #' @returns A matrix with columns `pop1`, `cross`, `pop2`, `h2_t1` and `h2_t2`.
    calculate_rg_gradient = function(k_pop1, k_cross, k_pop2, h2_t1, h2_t2, rc,
                                     calculator = GeneticCorrelationAnalysis$new()) {
      ones <- rep(1, length(k_pop1))

      self$calculate_jacobian(
        function(x) {
          suppressWarnings(
            calculator$calculate_rg(ones, x$pop1, x$cross, x$pop2, ones, ones, ones, x$h2_t1, x$h2_t2, rc)$rg
          )
        },
        list(pop1 = k_pop1, cross = k_cross, pop2 = k_pop2, h2_t1 = h2_t1, h2_t2 = h2_t2)
      )
    },
    #' @description
    #' Normalized meta-analysis weights, matching `Analysis$run_meta`.
    #'
    #' Rows with a non-finite estimate or SE are dropped before anything else. Fixed raw
    #' weights are `1 / se^2`; random raw weights are `1 / (se^2 + var(estimate))` with the
    #' variance over every kept row, not per group. Each kept row's share is its raw weight
    #' over its group's total. The pooled point is `sum(share * estimate)` per group.
    #'
    #' @param estimate Estimate per row.
    #' @param se Native SE per row.
    #' @param group Grouping key per row.
    #' @param method `"fixed"` or `"random"`.
    #' @returns The share per row, `NA` for dropped rows or an undefined pooling.
    calculate_meta_shares = function(estimate, se, group, method) {
      keep <- is.finite(estimate) & is.finite(se)
      raw  <- rep(NA_real_, length(estimate))

      raw[keep] <- switch(
        method,
        fixed  = 1 / se[keep] ^ 2,
        random = 1 / (se[keep] ^ 2 + stats::var(estimate[keep]))
      )

      total <- stats::ave(ifelse(keep, raw, 0), group, FUN = sum)

      ifelse(keep, raw / total, NA_real_)
    },
    #' @description
    #' Influence terms of per-stratum h2 values.
    #'
    #' A term table maps CIF influences to estimate influences: output `o`'s influence is the
    #' sum over its rows of `coef * phi(cohort, stratum, age)`, where `k` is the CIF the
    #' estimate read at that point.
    #'
    #' @param units Data.table with one row per h2 value: `output`, `stratum`, `age`, the
    #'   cohort keys `pop` and `fh`, and their CIFs `k_pop` and `k_fh` at that age.
    #' @param rc Relationship coefficient.
    #' @returns A term table with columns `output`, `cohort`, `stratum`, `age`, `k`, `coef`.
    h2_terms = function(units, rc) {
      grad <- self$calculate_h2_gradient(units$k_pop, units$k_fh, rc)

      bind_rows(
        units |> transmute(output, cohort = pop, stratum, age, k = k_pop, coef = grad[, "pop"]),
        units |> transmute(output, cohort = fh, stratum, age, k = k_fh, coef = grad[, "fh"])
      )
    },
    #' @description
    #' Influence terms of linear combinations of other outputs.
    #'
    #' @param terms A term table.
    #' @param weights Data.table with columns `from` (an output of `terms`), `to` (the new
    #'   output) and `coef`.
    #' @returns The term table of the `to` outputs, one row per output and CIF point.
    compose_terms = function(terms, weights) {
      terms |>
        inner_join(
          weights, by = join_by(output == from), relationship = "many-to-many", suffix = c("", ".weight")
        ) |>
        group_by(output = to, cohort, stratum, age) |>
        summarise(k = k[1], coef = sum(coef * coef.weight), .groups = "drop") |>
        as.data.table()
    },
    #' @description
    #' Influence terms of per-stratum rg values.
    #'
    #' The rg calculator reads three CIFs directly and two h2 values. Each h2 value is
    #' either that stratum's own h2 at the rg age or a pooled h2, and its influence comes in
    #' through the h2 term table and the rg gradient in that h2.
    #'
    #' @param units Data.table with one row per rg value: `output`, `stratum`, `age`, cohort
    #'   keys `pop1`, `cross`, `pop2`, their CIFs `k_pop1`, `k_cross`, `k_pop2`, the h2
    #'   values `h2_t1`, `h2_t2` the calculator used, and `h2_t1_output`, `h2_t2_output`,
    #'   the outputs of `h2_terms` behind them.
    #' @param h2 Term table of every output named in `h2_t1_output` and `h2_t2_output`.
    #' @param rc Relationship coefficient.
    #' @returns A term table.
    rg_terms = function(units, h2, rc) {
      grad <- self$calculate_rg_gradient(
        units$k_pop1, units$k_cross, units$k_pop2, units$h2_t1, units$h2_t2, rc
      )

      direct <- bind_rows(
        units |> transmute(output, cohort = pop1, stratum, age, k = k_pop1, coef = grad[, "pop1"]),
        units |> transmute(output, cohort = cross, stratum, age, k = k_cross, coef = grad[, "cross"]),
        units |> transmute(output, cohort = pop2, stratum, age, k = k_pop2, coef = grad[, "pop2"])
      )
      through_h2 <- self$compose_terms(h2, bind_rows(
        units |> transmute(from = h2_t1_output, to = output, coef = grad[, "h2_t1"]),
        units |> transmute(from = h2_t2_output, to = output, coef = grad[, "h2_t2"])
      ))

      bind_rows(direct, through_h2) |>
        group_by(output, cohort, stratum, age) |>
        summarise(k = k[1], coef = sum(coef), .groups = "drop") |>
        as.data.table()
    },
    #' @description
    #' Complete influence vectors of the outputs of a term table, over pedigree rows.
    #'
    #' @param terms A term table.
    #' @param cohorts Named list over cohort keys; each a named list over strata of
    #'   data.tables with columns `row` (the person's pedigree row), `trait_age`,
    #'   `trait_status`, `weight`.
    #' @param n_rows Pedigree row count.
    #' @param tolerance Largest allowed gap between a term's `k` and the plug-in CIF.
    #' @returns An `n_rows` by outputs matrix, columns named by output.
    assemble_influence = function(terms, cohorts, n_rows, tolerance = 1e-10) {
      outputs <- unique(terms$output)
      psi     <- matrix(0, n_rows, length(outputs), dimnames = list(NULL, outputs))

      for (part in split(terms, by = c("cohort", "stratum"), sorted = TRUE)) {
        cohort  <- part$cohort[1]
        stratum <- part$stratum[1]
        # By position: `[[""]]` never matches a name, and "" is a valid stratum label.
        strata  <- cohorts[[cohort]]
        tte     <- strata[match(stratum, names(strata))][[1]]

        if (is.null(tte)) stop("No cohort rows for \"", cohort, "\" in stratum \"", stratum, "\"")

        part <- part |>
          group_by(output, age) |>
          summarise(k = k[1], coef = sum(coef), .groups = "drop")
        ages <- sort(unique(part$age))
        infl <- self$calculate_cif_influence(tte$trait_age, tte$trait_status, tte$weight, ages)
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
    },
    #' @description
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
    calculate_pair_variances = function(graph, psi, max_degree, categories = NULL) {
      columns  <- paste0("v", seq_len(ncol(psi)))
      values   <- stats::setNames(lapply(seq_len(ncol(psi)), function(q) psi[, q]), columns)
      products <- lapply(columns, function(v) paste0(c("first.", "second."), v))

      moments <- pedigreegraph::relationship_moments(
        graph,
        max_degree = if (is.null(categories)) max_degree,
        categories = categories,
        values     = values,
        products   = products,
        symmetric  = "canonical",
        progress   = FALSE
      )
      cross <- as.data.frame(pedigreegraph::moments_sum(moments, "category"), stats = "cross")

      unname(colSums(psi ^ 2) + 2 * unlist(cross[1, paste0("cross.first.", columns, ":second.", columns)]))
    },
    #' @description
    #' The trimmed pedigree graph and the `person_id` on each of its rows. The pedigree and
    #' proband ids are only needed to build it, so they are dropped once it exists.
    graph = function() {
      if (is.null(private$built)) {
        if (is.null(private$pedigree)) stop("This SandwichAnalysis was created without a pedigree")

        private$built    <- private$build_graph(private$pedigree, private$probands, private$max_degree)
        private$pedigree <- NULL
        private$probands <- NULL
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
    #' @returns A list with `variance` (a data.table of `output` and `variance`),
    #'   `batch_size` and `passes`.
    run = function(terms, cohorts) {
      built  <- self$graph()
      n_rows <- built$graph$n
      budget <- getOption("epimight.sandwich_batch_bytes", 2^31)
      size   <- private$batch_size(n_rows, budget)

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

      variance <- terms |>
        group_by(output) |>
        summarise(finite = all(is.finite(coef) & is.finite(k)), .groups = "drop") |>
        mutate(variance = NA_real_)
      outputs  <- variance$output[variance$finite]
      batches  <- split(outputs, ceiling(seq_along(outputs) / size))

      for (batch in batches) {
        psi    <- self$assemble_influence(terms |> filter(output %chin% batch), by_stratum, n_rows)
        values <- self$calculate_pair_variances(built$graph, psi, private$max_degree)

        variance$variance[match(colnames(psi), variance$output)] <- values
      }

      list(
        variance   = variance |> select(output, variance) |> as.data.table(),
        batch_size = size,
        passes     = length(batches)
      )
    }
  )
)
