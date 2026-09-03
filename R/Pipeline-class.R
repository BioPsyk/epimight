#' @title Class that takes care of running all parts of the complete pipeline.
#' @docType class
#' @import R6
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stringr
#' @import rjson
#' @import waldo
#' @export
Pipeline <- R6::R6Class( #nolint
  "Pipeline",
  private = list(
    pool     = NULL,
    analyses = NULL,
    results  = list(
      cif = list(),
      h2  = list(),
      rg  = list()
    )
  ),
  public = list(
    validation_rules = list(),
    #' @description
    #' Creates an pipeline instance that stores the given time-to-event data.
    initialize = function(...) {
      validator <- ArgumentsValidator$new(
        pool = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            person_id = list(
              type     = "string",
              required = TRUE
            ),
            trait = list(
              type     = "string",
              required = TRUE
            ),
            trait_status = list(
              type     = "integer",
              enum     = list(0, 1, 2),
              required = TRUE
            ),
            trait_age = list(
              type     = "numeric",
              minimum  = 0,
              required = TRUE
            ),
            relatives_kind = list(
              required = TRUE,
              type     = "string"
            ),
            relatives_n = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            ),
            relatives_n_trait = list(
              type     = "integer",
              minimum  = 0,
              required = TRUE
            )
          )
        )
      )

      args         <- validator$run(...)
      private$pool <- args$pool

      self$validation_rules$cif <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          index_trait = list(
            required = TRUE,
            type     = "string"
          ),
          relatives_trait = list(
            required = FALSE,
            type     = "string"
          ),
          relatives_kind = list(
            required = FALSE,
            type     = "string"
          ),
          stratify_columns = list(
            type    = "list",
            items   = list(type = "string"),
            default = list()
          ),
          use_weighted = list(
            type    = "logical",
            default = TRUE
          )
        )
      )

      self$validation_rules$h2 <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          cif_pop = self$validation_rules$cif,
          cif_fh  = self$validation_rules$cif,
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          )
        )
      )

      self$validation_rules$rg <- list(
        required   = TRUE,
        type       = "named_list",
        properties = list(
          cif_cross  = self$validation_rules$cif,
          h2_t1      = self$validation_rules$h2,
          h2_t2      = self$validation_rules$h2,
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          )
        )
      )
      self$validation_rules$rg$properties$cif_cross$relatives_trait$required <- TRUE
      self$validation_rules$rg$properties$cif_cross$relatives_kind$required  <- TRUE

      private$analyses <- list(
        core = Analysis$new(),
        h2   = HeritabilityAnalysis$new(),
        cif  = CumulativeIncidenceAnalysis$new(),
        rg   = GeneticCorrelationAnalysis$new()
      )
    },
    clear_results = function() {
      private$results <- list(cif = list(), h2 = list(), rg = list())
    },
    add_results = function(type, results, args) {
      if (!is.character(type)) stop("Given `type` was not a character")
      if (!is.data.table(results)) stop("Given `results` was not a data.table")
      if (!is.list(args)) stop("Given `args` was not a named list")
      if (!(type %in% names(self$validation_rules))) stop("Given `type` \"", type, "\" was unknown")

      rules     <- self$validation_rules[[type]]
      validator <- do.call(ArgumentsValidator$new, rules$properties)
      args      <- do.call(validator$run, args)
      args      <- args[order(names(args))]
      key       <- rjson::toJSON(args)

      private$results[[type]][[key]] <- results
    },
    get_results = function(type, args) {
      if (!is.character(type)) stop("Given `type` was not a character")
      if (!is.list(args)) stop("Given `args` was not a named list")
      if (!(type %in% names(self$validation_rules))) stop("Given `type` \"", type, "\" was unknown")

      rules     <- self$validation_rules[[type]]
      validator <- do.call(ArgumentsValidator$new, rules$properties)
      args      <- do.call(validator$run, args)
      args      <- args[order(names(args))]
      key       <- rjson::toJSON(args)

      private$results[[type]][[key]]
    },
    #' @description
    #' Retrieves time-to-event data to use in a run based on the given traits, relationship kind and
    #' straitfy columns. Makes sure that the retrieved data fulfills the requirements of carrying out
    #' a single pipeline run.
    get_tte = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$cif$properties)
      args      <- validator$run(...)
      columns   <- c(c("person_id", "trait_status", "trait_age"), unlist(args$stratify_columns))

      for (col in columns) {
        if (!(col %in% colnames(private$pool))) {
          stop("Column \"", col, "\" was not found in the TTE pool: ", paste(colnames(private$pool), collapse = ", "))
        }
      }

      tte <- private$pool[
        trait == args$index_trait
      ][
        , .SD[1], by = "person_id"
      ][
        , ..columns
      ]

      if (nrow(tte) == 0) stop(paste0("No proband TTE data found for trait ", index_trait))

      if (!is.null(args$relatives_trait) && !is.null(args$relatives_kind)) {
        relatives_tte <- private$pool[
          trait == args$relatives_trait & relatives_kind == args$relatives_kind
        ][
          , .SD[1], by = "person_id"
        ][
          , c("person_id", "relatives_kind", "relatives_n", "relatives_n_trait")
        ]

        if (nrow(relatives_tte) == 0) {
          stop(paste0(
            "No family history TTE data found for trait \"", args$relatives_trait,
            "\" and relationship kind \"", args$relatives_kind, "\""
          ))
        }

        tte <- tte[
          relatives_tte,
          on = .(person_id = person_id)
        ][
          relatives_n_trait > 0
        ]

        if (isTRUE(args$use_weighted)) {
          tte <- tte[
            , weight := ifelse(relatives_n_trait > 0.0, relatives_n_trait / relatives_n, 0.0)
          ]
        }

        if (nrow(tte) == 0) {
          stop(paste0(
            "No probands with at least 1 relative (of kind \"",
            args$relatives_kind, "\") with trait \"", args$relatives_trait, "\""
          ))
        }
      }

      return(tte)
    },
    #' @description
    #' Helper that aggregates the given estimates dataset down to a single row per stratification
    #' combination, where the kept row is the one with the largest `time` value within it's group.
    #'
    #' If we have a dataset of cumulative incidences stratified by birth year and sex, each
    #' stratification combination will have multiple rows, like this:
    #'
    #'   |------+------------+--------+------------+------|
    #'   | age  | birth_year | sex    |  estimates | case |
    #'   |------+------------+--------+------------+------|
    #'   |   43 |       1981 | f      | 0.10639881 |  141 |
    #'   |   42 |       1981 | f      | 0.09763101 |  131 |
    #'   |   41 |       1981 | f      | 0.09335325 |  125 |
    #'   |   43 |       1981 | m      | 0.09816850 |  134 |
    #'   |   40 |       1981 | m      | 0.09417040 |  122 |
    #'   |   39 |       1981 | m      | 0.09747292 |  141 |
    #'   |------+------------+--------+------------+------|
    #'
    #' Running `max_age_by_stratification(cif_example, list("birth_year", "sex"))`
    #' on this dataset would produce:
    #'
    #' |------+------------+--------+------------+------|
    #' | age  | birth_year | sex    |  estimates | case |
    #' |------+------------+--------+------------+------|
    #' |   43 |       1981 | f      | 0.10639881 |  141 |
    #' |   43 |       1981 | m      | 0.09816850 |  134 |
    #' |------+------------+--------+------------+------|
    max_age_by_stratification = function(estimates, stratify_columns) {
      estimates |>
        group_by(!!!rlang::syms(stratify_columns)) |>
        arrange(desc(age)) |>
        filter(row_number() == 1) |>
        as.data.table()
    },
    add_cif_prefix = function(cif, prefix, stratify_columns) {
      cif |>
        select(!!!stratify_columns, age, cif, cases) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(cif, cases))
    },
    #' @description
    #' Helper that removes prefixes from column names that the function `run_h2` adds
    #' to its results.
    add_h2_prefix = function(h2, prefix, stratify_columns) {
      h2 |>
        select(!!!stratify_columns, age, h2) |>
        rename_with(~ paste0(prefix, "_", .), .cols = c(h2))
    },
    #' @description
    #' Helper that runs cif on the given time-to-event data and handles prefixing columns according to
    #' given trait and cohort naming.
    run_cif = function(...) {
      validator  <- do.call(ArgumentsValidator$new, self$validation_rules$cif$properties)
      args       <- validator$run(...)
      cached_cif <- self$get_results("cif", args)

      if (!is.null(cached_cif)) {
        return(list(
          args    = args,
          results = cached_cif
        ))
      }

      tte <- do.call(self$get_tte, args)
      cif <- private$analyses$cif$run(
        tte              = tte,
        stratify_columns = args$stratify_columns
      ) |>
        mutate(
          index_trait     = args$index_trait,
          relatives_trait = ifelse("relatives_trait" %in% args, args$relatives_trait, NA),
          relatives_kind  = ifelse("relatives_kind" %in% args,  args$relatives_kind,  NA)
        ) |>
        select(
          index_trait,
          relatives_trait,
          relatives_kind,
          all_of(unlist(args$stratify_columns)),
          age,
          everything()
        )

      if (is.null(cif)) {
        stop(paste0(
          "No TTE events found when producing cif_", index_trait, "_", rel_trait, "_", rel_kind
        ))
      }

      self$add_results("cif", cif, args)

      list(
        args    = args,
        results = cif
      )
    },
    #' @description
    #' Helper that runs h2 on the given time-to-event data and handles prefixing columns according to
    #' given trait and cohort naming.
    run_h2 = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$h2$properties)
      validator$add_post_validation(function(args, rules) {
        if ("relatives_trait" %in% args$cif_pop) {
          stop("Using `relatives_trait` in `cif_pop` is not allowed")
        }

        if ("relatives_kind" %in% args$cif_pop) {
          stop("Using `relatives_kind` in `cif_pop` is not allowed")
        }

        if (args$cif_pop$index_trait != args$cif_fh$index_trait) {
          stop("Using different `index_traits` in `cif_pop` and `cif_fh` is not allowed")
        }

        if (args$cif_fh$index_trait != args$cif_fh$relatives_trait) {
          stop("Using different `index_traits` and `relatives_trait` in `cif_fh` is not allowed")
        }

        if (!identical(args$cif_pop$stratify_columns, args$cif_fh$stratify_columns)) {
          stop("Using different `stratify_columns` in `cif_pop` and `cif_fh` is not allowed")
        }

        return(args)
      })

      args      <- validator$run(...)
      cached_h2 <- self$get_results("h2", args)

      if (!is.null(cached_h2)) {
        return(list(
          args    = args,
          results = cached_h2
        ))
      }

      cif_pop <- do.call(self$run_cif, args$cif_pop)
      cif_fh  <- do.call(self$run_cif, args$cif_fh)

      stratify_columns <- args$cif_pop$stratify_columns
      stratify_symbols <- rlang::syms(stratify_columns)

      cif <- cif_pop$results |>
        inner_join(cif_fh$results, by = join_by(age, !!!stratify_columns)) |>
        rename(
          pop_cif   = cif.x,
          pop_cases = cases.x,
          fh_cif    = cif.y,
          fh_cases  = cases.y
        ) |>
        select(age, !!!stratify_symbols, pop_cif, pop_cases, fh_cif, fh_cases)

      h2 <- private$analyses$h2$run(
        cif         = cif,
        relatedness = args$relatedness
      ) |>
        mutate(index_trait = args$cif_pop$index_trait) |>
        select(index_trait, age, !!!stratify_symbols, h2, se, l95, u95)

      if (is.null(h2)) stop(paste0("No valid results found when producing h2 for trait ", args$cif_pop$index_trait))

      self$add_results("h2", h2, args)

      list(
        args    = args,
        results = h2
      )
    },
    run_rg = function(...) {
      validator <- do.call(ArgumentsValidator$new, self$validation_rules$rg$properties)

      validator$add_post_validation(function(args, rules) {
        if (!identical(args$h2_t1$cif_pop$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `h2_t1` and `h2_t2` is not allowed")
        }

        if (!identical(args$cif_cross$stratify_columns, args$h2_t2$cif_pop$stratify_columns)) {
          stop("Using different `stratify_columns` in `cif_cross`, `h2_t1` and `h2_t2` is not allowed")
        }

        return(args)
      })

      args      <- validator$run(...)
      cached_rg <- self$get_results("rg", args)

      if (!is.null(cached_rg)) {
        return(list(
          args    = args,
          results = cached_rg
        ))
      }

      cif_t1_pop <- do.call(self$run_cif, args$h2_t1$cif_pop)
      cif_t2_pop <- do.call(self$run_cif, args$h2_t2$cif_pop)
      h2_t1      <- do.call(self$run_h2, args$h2_t1)
      h2_t2      <- do.call(self$run_h2, args$h2_t1)
      cif_cross  <- do.call(self$run_cif, args$cif_cross)

      stratify_columns <- args$cif_cross$stratify_columns
      join_columns     <- c(list("age"), stratify_columns)
      join_symbols     <- rlang::syms(join_columns)

      combined <- self$add_cif_prefix(cif_t1_pop$results, "t1_pop", stratify_columns) |>
        inner_join(
          self$add_cif_prefix(cif_cross$results, "cross", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_cif_prefix(cif_t2_pop$results, "t2_pop", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_t1$results, "t1", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        inner_join(
          self$add_h2_prefix(h2_t2$results, "t2", stratify_columns),
          by = join_by(!!!join_columns)
        ) |>
        self$max_age_by_stratification(stratify_columns)

      if (nrow(combined) == 0) stop("After joining all cif and h2 results no data was left")

      rg <- private$analyses$rg$run(
        estimates   = combined,
        relatedness = args$relatedness
      ) |>
        select(!!!stratify_columns, rg, se, l95, u95)

      if (nrow(rg) == 0) stop("No genetic correlation results produced")

      self$add_results("rg", rg, args)

      list(
        args    = args,
        results = rg
      )
    },
    #' @description
    #' Runs a single analysis using the given two traits, relationship kind, straitfy colums
    #' and amount of draws.
    run = function(...) {
      heritability_rules <- list(
        required = TRUE,
        type = "named_list",
        properties = list(
          trait = list(
            required = TRUE,
            type     = "string"
          ),
          relatives_kind = list(
            required = FALSE,
            type     = "string"
          ),
          relatedness = list(
            required = TRUE,
            type     = "numeric",
            minimum  = 0
          )
        )
      )

      validator <- ArgumentsValidator$new(
        heritability1 = heritability_rules,
        heritability2 = heritability_rules,
        stratify_columns = list(
          type    = "list",
          items   = list(type = "string"),
          default = list()
        ),
        use_weighted = list(
          type    = "logical",
          default = TRUE
        )
      )

      args <- validator$run(...)

      rg_args <- list(
        h2_t1 = list(
          cif_pop = list(
            index_trait = args$heritability1$trait
          ),
          cif_fh = list(
            index_trait     = args$heritability1$trait,
            relatives_trait = args$heritability1$trait,
            relatives_kind  = args$heritability1$relatives_kind
          ),
          relatedness = args$heritability1$relatedness
        ),
        h2_t2 = list(
          cif_pop = list(
            index_trait = args$heritability2$trait
          ),
          cif_fh = list(
            index_trait     = args$heritability2$trait,
            relatives_trait = args$heritability2$trait,
            relatives_kind  = args$heritability2$relatives_kind
          ),
          relatedness = args$heritability2$relatedness
        ),
        cif_cross = list(
          index_trait     = args$heritability1$trait,
          relatives_trait = args$heritability2$trait,
          relatives_kind  = args$heritability2$relatives_kind
        ),
        relatedness = args$heritability2$relatedness
      )

      do.call(self$run_rg, rg_args)
    },
    run_meta = function(results) {
      validator <- ArgumentsValidator$new(
        metadata = list(
          required   = TRUE,
          type       = "named_list",
          properties = list(
            version = list(
              required   = TRUE,
              type       = "string"
            ),
            arguments = list(
              required   = TRUE,
              type       = "named_list",
              properties = self$validation_rules$run
            )
          )
        ),
        cif = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            index_trait = list(
              type     = "string",
              required = TRUE
            ),
            relatives_trait = list(
              type     = "string",
              required = TRUE
            ),
            relatives_kind = list(
              type     = "string",
              required = TRUE
            ),
            cif = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        ),
        h2 = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            index_trait = list(
              type     = "string",
              required = TRUE
            ),
            h2 = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        ),
        rg = list(
          required = TRUE,
          type     = "data.table",
          columns  = list(
            rg = list(
              type     = "numeric",
              required = TRUE
            ),
            se = list(
              type     = "numeric",
              required = TRUE
            ),
            l95 = list(
              type     = "numeric",
              required = TRUE
            ),
            u95 = list(
              type     = "numeric",
              required = TRUE
            )
          )
        )
      )
      args <- do.call(validator$run, results)

      #---------------------------------------------------------------------------------
      # Cumulative incidence

      cif_meta <- private$analyses$core$run_meta(
        estimates        = args$cif,
        estimate_column  = "cif",
        se_column        = "se",
        stratify_columns = list("index_trait", "relatives_trait", "relatives_kind", "age")
      ) |>
        select(index_trait, relatives_trait, relatives_kind, everything())

      #---------------------------------------------------------------------------------
      # Heritability

      h2_meta <- private$analyses$core$run_meta(
        estimates        = args$h2,
        estimate_column  = "h2",
        se_column        = "se",
        stratify_columns = list("index_trait")
      ) |>
        select(index_trait, everything())

      #---------------------------------------------------------------------------------
      # Genetic correlation

      rg_meta <- private$analyses$core$run_meta(
        estimates       = args$rg,
        estimate_column = "rg",
        se_column       = "se"
      )

      return(list(
        cif = cif_meta,
        h2  = h2_meta,
        rg  = rg_meta
      ))
    }
  )
)
