#' @title Generates raw SQL queries by provided column filters.
#' @description
#' R6 class that generates raw SQL queries for different types of advanced data retrieval
#' tasks. The generated SQL queries are generated in a way that should be readable and easy
#' to debug and analyze.
#' @docType class
#' @import R6
#' @import jinjar
#' @import readr
#' @import yaml
#' @export
TTERetriever <- R6::R6Class( #nolint
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
    make_env = function() {
      env <- c()

      if (!is.null(self$username)) {
        env <- append(env, sprintf("PGUSER=%s", self$username))
      }

      if (!is.null(self$password)) {
        env <- append(env, sprintf("PGPASSWORD=%s", self$password))
      }

      return(env)
    }
  ),
  public = list(
    output_directory = NULL,
    hostname = NULL,
    username = NULL,
    password = NULL,
    validator = NULL,
    rules_post_validate = function(args, rules) {
      if (!is.null(args[["output_columns"]])) {
        cols <- list(
          "gender",
          "born_at",
          "father_id",
          "mother_id",
          "status",
          "status_changed",
          "birthplace_id"
        )

        for (diag in names(args$samples$diagnosis_filters)) {
          cols <- append(cols, paste0(diag, "_diagnosed_at"))
          cols <- append(cols, paste0(diag, "_diagnosis_kind"))
          cols <- append(cols, paste0(diag, "_diagnosis_icd_edition"))
          cols <- append(cols, paste0(diag, "_diagnosis_icd_id"))
          cols <- append(cols, paste0(diag, "_record_patient_kind"))
        }

        rules$output_columns$items$enum <- cols

        self$validator$check_type("output_columns", rules$output_columns, args$output_columns)
      }

      verticals <- epimight:::vertical_relationship_kinds

      if (is.null(args[["relatives"]])) return(args)
      if (is.null(args$relatives[["relationship_filters"]])) return(args)
      if (!(args$relatives$relationship_filters$kind %in% verticals)) return(args)

      args$relatives$using_vertical_relationship <- TRUE

      return(args)
    },
    initialize = function(output_directory, hostname, username = NULL, password = NULL) {
      if (!dir.exists(output_directory)) {
        stop("Given output_directory does not exist, or is not accessible")
      }

      self$output_directory <- output_directory
      self$hostname         <- hostname
      self$username         <- username
      self$password         <- password

      private$sql_dir <- system.file("extdata", "sql", package = "epimight")

      if (!dir.exists(private$sql_dir)) {
        stop("Internal package error, sql_dir (", private$sql_dir, ") does not exist!")
      }

      private$jinjar_config <- jinjar::jinjar_config(
        loader = jinjar::path_loader(private$sql_dir),
        trim_blocks = TRUE,
        lstrip = TRUE
      )

      rules          <- epimight:::tte_retriever_rules
      self$validator <- rlang::exec(ArgumentsValidator$new, !!!rules)

      self$validator$add_post_validation(self$rules_post_validate)
    },
    #' @description
    #' Generates an SQL query that retrieves TTE for a single disorder.
    #'
    #' @returns Generated SQL query as a string.
    generate_query = function(...) {
      data <- self$validator$run(...)

      return(
        private$render_template("base.sql", data)
      )
    },
    #' @description
    #' Executes the given query using the PostgreSQL client psql outside of R
    #' and saves the results as a CSV-file of the given output path, along with
    #' a SQL file with the run querry (named "{output path}.sql").
    #'
    #' @param output_prefix The
    #' @param query SQL query to execute.
    #' @param output_path Path of file to output the results into.
    #' @param hostname Hostname to use when connecting to the database.
    #' @param username Username to authenticate with when connecting to the database.
    #' @param password Password to authenticate with when connecting to the database.
    execute_query = function(output_path, query) {
      data_path <- paste0(output_path, ".csv")

      output <- system2(
        "psql",
        args = c(
          "-h", self$hostname,
          "-P", "footer=off",
          "-A",
          "-F','",
          "-o", data_path,
          "ibp_registry"
        ),
        env = private$make_env(),
        input = query,
        stdout = TRUE,
        stderr = TRUE
      )

      if (length(output) == 0) {
        return(data_path)
      }

      query_lines <- strsplit(query, split = "\n")[[1]]

      for (i in seq_along(query_lines)) {
        query_lines[[i]] <- paste0(
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
    },
    write_args = function(args, args_path) {
      yaml::write_yaml(
        args,
        args_path,
        handlers = list(
          Date = function(d) format(d, "%Y-%m-%d")
        )
      )
    },
    read_args = function(args_path) {
      yaml::read_yaml(
        args_path,
        handlers = list(
          # This is needed so that homogeneous lists are converted into lists and not vectors.
          seq = function(s) return(s)
        )
      )
    },
    #' @description
    #' Generates an SQL query and executes it using the PostgreSQL client psql
    #' outside of R and saves the results as a CSV-file of the given output path, along with
    #' a SQL file with the run querry (named "{output path}.sql").
    #'
    #' @param output_prefix The output prefix to use for all files that are output.
    #' @param args The TTE arguments to use when generating the query.
    run = function(output_prefix, args) {
      query <- rlang::exec(self$generate_query, !!!args)

      if (!is.character(output_prefix)) {
        stop("Given output_prefix (first argument) was not a string: ", class(output_prefix))
      } else if (!grepl("^[A-Za-z0-9_-]+$", output_prefix)) {
        stop(
          "Given output_prefix (first argument) contained illegal characters",
          "(Only A-Z, a-z, 0-9, _ and - are allowed): '",
          class(output_prefix), "'"
        )
      }
      
      output_path <- file.path(self$output_directory, output_prefix)
      query_path  <- paste0(output_path, ".sql")      
      args_path   <- paste0(output_path, ".yaml")
      query_lines <- strsplit(query, split = "\n")[[1]]
      query_file  <- file(query_path)
      
      writeLines(query_lines, query_file)
      close(query_file)

      self$write_args(args, args_path)

      data_path <- self$execute_query(output_path, query)      

      return(list(
        data  = data_path,
        query = query_path,
        args  = args_path
      ))
    },
    #' @description
    #' Reads the arguments YAML file from the given path then generates an SQL query and
    #' executes it using the PostgreSQL client psql outside of R and saves the results
    #' as a CSV-file of the given output path, along with a SQL file with the run query
    #' (named "{output path}.sql").
    #'
    #' @param output_prefix The output prefix to use for all files that are output.
    #' @param args_path Path to the arguments YAML file.
    run_from_file = function(output_prefix, args_path) {
      args <- self$read_args(args_path)

      return(
        self$run(output_prefix, args)
      )
    }
  )
)
