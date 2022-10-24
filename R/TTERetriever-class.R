
#' @title Generates raw SQL queries by provided column filters.
#' @description
#' R6 class that generates raw SQL queries for different types of advanced data retrieval
#' tasks. The generated SQL queries are generated in a way that should be readable and easy
#' to debug and analyze.
#' @docType class
#' @import R6
#' @import jinjar
#' @import readr
#' @export
TTERetriever <- R6::R6Class(
  "TTERetriever",
  private = list(
    sql_dir = NULL,
    jinjar_config = NULL,
    render_template = function(template_name, data) {
      template_path <- file.path(private$sql_dir, template_name)
      template      <- readr::read_file(template_path)
      results       <- jinjar::render(template, !!!data, .config = private$jinjar_config)

      return(results)
    },
    create_validators = function() {
      single_disorder_rules = list(
        diagnosis_filters = list(
          type = "named_list",
          required = TRUE,
          description = "
          The filters to apply on each diagnosis collected for a single individual.
          From the results of that filtering, the diagnosis that occured first is selected
          as the 'time-to-event' for that individual.
          ",
          properties = list(
            icd_codes_regexp = list(
              required = TRUE,
              description = "
              POSIX regular expression that is run on a diagnosis ICD code to determine if the
              diagnosis should be included or not. When the regexp matches, the diagnosis is
              included and when it doesn't match, the diagnosis is excluded.

              For details, see the PostgreSQL documentation:
              https://www.postgresql.org/docs/13/functions-matching.html#FUNCTIONS-POSIX-REGEXP
              ",
              type = "string"
            ),
            diagnosis_kind = list(
              type = "list",
              description = "Which kinds of diagnoses to investigate.",
              items = list(
                type = "string",
                enum = IbpRiskEstimations:::diagnosis_kinds
              )
            ),
            record_origin = list(
              type = "string",
              description = "
              Which register the medical records originates from.

              - pcrr = Psychiatric Central Research Register
              - npr  = National Patient Register
              ",
              enum = list("pcrr", "npr")
            )
          )
        ),
        sample_filters = list(
          type = "named_list",
          description = "
          The filters to apply on each individual in the population.
          The results of that filtering becomes the sample under study.
          ",
          properties = list(
            born_at_min = list(type = "date"),
            born_at_max = list(type = "date"),
            gender = list(
              type = "string",
              enum = IbpRiskEstimations:::genders
            ),
            status = list(
              type = "list",
              items = list(
                type = "string",
                enum = IbpRiskEstimations:::civil_statuses
              )
            ),
            diagnosis_earliest_onset = list(
              type = "integer",
              minimum = 1
            ),
            diagnosis_latest_onset = list(
              type = "integer",
              minimum = 1
            )
          )
        ),
        study_end_at = list(
          required = TRUE,
          description = "
          The study end date to use when determining the failure status/time.
          If an individual isn't diagnosed or deceased before this date, they
          have 'survived' the period, and is marked as 'censored (0)'.
          ",
          type = "date"
        )
      )

      single_disorder_with_relatives_rules <- copy(single_disorder_rules)
      single_disorder_with_relatives_rules$relationship_filters = list(
        type = "named_list",
        required = TRUE,
        properties = list(
          component = list(
            type = "string",
            description = "
            The genealogy is divided into two 'components' (lingo for 'group' in graph theory):

            - pedigree1 = Everyone with Danish ancestry
            - rest = Everyone without Danish ancestry
            ",
            enum = list("pedigree1", "rest")
          ),
          kind = list(
            type = "string",
            required = TRUE,
            enum = as.list(names(IbpRiskEstimations:::relationship_kinds))
          )
        )
      )

      two_disorders_exclusion_rules <- copy(single_disorder_rules)
      two_disorders_exclusion_rules$exclusion_diagnosis_filters = copy(single_disorder_rules$diagnosis_filters)

      two_disorders_exclusion_with_relatives_rules <- copy(two_disorders_exclusion_rules)
      two_disorders_exclusion_with_relatives_rules$relationship_filters = copy(single_disorder_with_relatives_rules$relationship_filters)

      self$single_disorder_validator <- rlang::exec(
        ArgumentsValidator$new,
        !!!single_disorder_rules
      )

      self$single_disorder_with_relatives_validator <- rlang::exec(
        ArgumentsValidator$new,
        !!!single_disorder_with_relatives_rules
      )

      self$two_disorders_exclusion_validator <- rlang::exec(
        ArgumentsValidator$new,
        !!!two_disorders_exclusion_rules
      )

      self$two_disorders_exclusion_with_relatives_validator <- rlang::exec(
        ArgumentsValidator$new,
        !!!two_disorders_exclusion_with_relatives_rules
      )

      vertical_relationship_checker <- function(args, rules) {
        verticals <- vertical_relationship_kinds
        if (args$relationship_filters$kind %in% verticals) {
          args$using_vertical_relationship <- TRUE
        }

        return(args)
      }

      self$single_disorder_with_relatives_validator$add_post_validation(vertical_relationship_checker)
      self$two_disorders_exclusion_with_relatives_validator$add_post_validation(vertical_relationship_checker)
    }
  ),
  public = list(
    single_disorder_validator = NULL,
    single_disorder_with_relatives_validator = NULL,
    two_disorders_exclusion_validator = NULL,
    two_disorders_exclusion_with_relatives_validator = NULL,
    initialize = function() {
      private$sql_dir <- system.file("extdata", "sql", package = "IbpRiskEstimations")

      if (!dir.exists(private$sql_dir)) {
        stop("sql_dir (", private$sql_dir, ") does not exist!")
      }

      private$jinjar_config <- jinjar::jinjar_config(
        loader = jinjar::path_loader(private$sql_dir),
        trim_blocks = TRUE,
        lstrip = TRUE
      )

      private$create_validators()
    },
    #' @description
    #' Generates an SQL query that retrieves TTE for a single disorder.
    #'
    #' @returns Generated SQL query as a string.
    single_disorder = function(...) {
      data <- self$single_disorder_validator$run(...)

      return(
        private$render_template("single-disorder.sql", data)
      )
    },
    #' @description
    #' Generates an SQL query that retrieves TTE for a single disorder along with a count of
    #' how many relatives each sample have and how many of those relatives are affected by the disorder.
    #'
    #' @returns Generated SQL query as a string.
    single_disorder_with_relatives = function(...) {
      data <- self$single_disorder_with_relatives_validator$run(...)

      return(
        private$render_template("single-disorder-with-relatives.sql", data)
      )
    },
    #' @description
    #' Generates an SQL query that retrieves TTE for two disorders and removes samples which was diagnosed
    #' with the exclusion disorder before the target disorder.
    #'
    #' @returns Generated SQL query as a string.
    two_disorders_exclusion = function(...) {
      data <- self$two_disorders_exclusion_validator$run(...)

      return(
        private$render_template("two-disorders-exclusion.sql", data)
      )
    },
    #' @description
    #' Generates an SQL query that retrieves TTE for two disorders and removes samples which was diagnosed
    #' with the exclusion disorder before the target disorder, along with a count of how many relatives each
    #' sample have and how many of those relatives are affected by the disorder.
    #'
    #' @returns Generated SQL query as a string.
    two_disorders_exclusion_with_relatives = function(...) {
      data <- self$two_disorders_exclusion_with_relatives_validator$run(...)

      return(
        private$render_template("two-disorders-exclusion-with-relatives.sql", data)
      )
    },

    #' @description
    #' Executes the given query using the PostgreSQL client psql outside of R
    #' and saves the results as a CSV-file of the given output path, along with
    #' a SQL file with the run querry (named "{output path}.sql").
    #'
    #' @param query SQL query to execute.
    #' @param output_path Path of file to output the results into.
    #' @param hostname Hostname to use when connecting to the database.
    #' @param username Username to authenticate with when connecting to the database.
    #' @param password Password to authenticate with when connecting to the database.
    execute_query = function(query, output_path, hostname, username=NULL, password=NULL) {
      env <- c()

      if (!is.null(username)) {
        env <- append(env, sprintf("PGUSER=%s", username))
      }

      if (!is.null(password)) {
        env <- append(env, sprintf("PGPASSWORD=%s", password))
      }

      query_lines <- strsplit(query, split = "\n")[[1]]

      query_file <- file(paste0(output_path, ".sql"))
      writeLines(query_lines, query_file)
      close(query_file)

      output <- system2(
        "psql",
        args=c(
          "-h", hostname,
          "-P", "footer=off",
          "-A",
          "-F','",
          "-o", output_path,
          "ibp_registry"
        ),
        env=env,
        input=query,
        stdout=TRUE,
        stderr=TRUE
      )

      if (length(output) == 0) {
        return()
      }

      for (i in 1:length(query_lines)) {
        query_lines[[i]] = paste0(
          i,
          ": ",
          query_lines[[i]]
        )
      }

      stop(
        "---\n",
        paste(query_lines, collapse = "\n"),
        "\n---\n\nExecuting the query above resulted in the following error from the database: \n\n---\n",
        paste(output, collapse = "\n"),
        "\n---\n"
      )
    }
  )
)
