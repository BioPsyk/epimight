#' @title Validates named arguments using a ruleset.
#' @description
#' R6 class that takes a ruleset that is similar to JSON schema and validates a set of arguments
#' from that ruleset.
#' @docType class
#' @import R6
ArgumentsValidator <- R6::R6Class( #nolint
  "ArgumentsValidator",
  private = list(
    rules = NULL,
    post_validation = NULL
  ),
  public = list(
    #' @description
    #' Initializes the validator with the given rules.
    initialize = function(...) {
      private$rules <- list(...)

      if (!self$is_named_list(private$rules)) {
        stop("Given rules were not a named list")
      }
    },
    #' @description
    #' Determines if the given numeric is an integer or not.
    #'
    #' @param value Numeric value to check.
    #' @return True/false
    is_integer = function(value) {
      if (is.null(value)) {
        return(FALSE)
      }

      tol     <- .Machine$double.eps^0.5
      results <- abs(value - round(value)) < tol

      return(all(results))
    },
    #' @description
    #' Determines if the given list only contains named elements.
    #'
    #' @param value List to check.
    #' @return True/false
    is_named_list = function(value) {
      if (!is.list(value)) {
        return(FALSE)
      }

      properties <- length(value)

      if (properties == 0) {
        return(TRUE)
      }

      if (is.null(names(value))) {
        return(FALSE)
      }

      return(
        # Makes sure all elements are named
        properties == sum(names(value) != "", na.rm = TRUE)
      )
    },
    #' @description
    #' Checks that the given list against the given ruleset.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains rules for all members in the given value to validated.
    #' @param value List to validate
    check_list = function(key, rule, value) {
      if (!is.list(value)) {
        stop("Argument '", key, "' was not a list.")
      }

      if (is.null(rule$items)) {
        stop("Rule for argument '", key, "' did not have 'items' specified")
      }

      amount <- length(value)

      if (!is.null(rule$minimum_length) && amount < rule$minimum_length) {
        stop("Argument '", key, "' had too few elements: ", amount, " < ", rule$minimum_length)
      }

      if (!is.null(rule$maximum_length) && amount > rule$maximum_length) {
        stop("Argument '", key, "' had too many elements: ", amount, " > ", rule$maximum_length)
      }

      index <- 0
      for (elem in value) {
        value[[index + 1]] <- self$check_type(
          sprintf("%s[%d]", key, index),
          rule$items,
          elem
        )
        index <- index + 1
      }

      return(value)
    },
    #' @description
    #' Checks that the given named list against the given ruleset.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains rules for all members in the given value to validated.
    #' @param value List to validate
    check_named_list = function(key, rule, value) {
      if (!self$is_named_list(value)) {
        stop("Argument '", key, "' was not a named list")
      }

      if (is.null(rule$properties)) {
        stop("Rule for argument '", key, "' did not have 'properties' specified")
      }

      for (prop_key in names(rule$properties)) {
        prop_rule <- rule$properties[[prop_key]]
        full_key <- sprintf("%s[[%s]]", key, prop_key)

        if (isTRUE(prop_rule$required) && !(prop_key %in% names(value))) {
          stop("Named property '", full_key, "' did not exist")
        } else if (!isTRUE(prop_rule$required) && !(prop_key %in% names(value))) {
          next
        }

        prop_value <- value[[prop_key]]

        value[[prop_key]] <- self$check_type(full_key, prop_rule, prop_value)
      }

      return(value)
    },
    #' @description
    #' Checks that the given generic named list against the given ruleset.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains rules for all members in the given value to validated.
    #' @param value List to validate
    check_generic_named_list = function(key, rule, value) {
      if (!self$is_named_list(value)) {
        stop("Argument '", key, "' was not a named list")
      }

      if (is.null(rule$items)) {
        stop("Rule for argument '", key, "' did not have 'items' specified")
      }

      amount <- length(value)

      if (!is.null(rule$minimum_length) && amount < rule$minimum_length) {
        stop("Argument '", key, "' had too few elements: ", amount, " < ", rule$minimum_length)
      }

      if (!is.null(rule$maximum_length) && amount > rule$maximum_length) {
        stop("Argument '", key, "' had too many elements: ", amount, " > ", rule$maximum_length)
      }

      prop_rule <- rule$items

      for (prop_key in names(value)) {
        full_key  <- sprintf("%s[[%s]]", key, prop_key)
        prop_value <- value[[prop_key]]

        value[[prop_key]] <- self$check_type(full_key, prop_rule, prop_value)
      }

      return(value)
    },
    #' @description
    #' Checks that the given data.table against the given ruleset.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains rules for all columns in the given value to validated.
    #' @param value Data.table to validate
    check_data.table = function(key, rule, value) {
      message("check_data_table (", key, "):")
      print(value)

      if (!is.data.table(value)) {
        stop("Argument '", key, "' was not a data.table")
      }

      if (is.null(rule$columns)) {
        stop("Rule for argument '", key, "' did not have 'columns' specified")
      }

      for (col_key in names(rule$columns)) {
        col_rule <- rule$columns[[col_key]]
        full_key <- sprintf("%s[[%s]]", key, col_key)

        if (isTRUE(col_rule$required) && !(col_key %in% colnames(value))) {
          stop("Data.table column '", full_key, "' did not exist")
        } else if (!isTRUE(col_rule$required) && !(col_key %in% colnames(value))) {
          next
        }

        col_value <- value[[col_key]]

        message(" check_data_table (", col_key, "): ")
        print(class(col_value))
        print(col_value)

        value[[col_key]] <- self$check_type(full_key, col_rule, col_value)
      }

      return(value)
    },
    #' @description
    #' Checks that the given numeric value falls within the range of the given ruleset.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains the minimum and maximum values.
    #' @param value Numeric value to check.
    check_range = function(key, rule, value) {
      if (!is.null(rule$minimum) && value < rule$minimum) {
        stop("Argument '", key, "' was smaller than minimum value: ", rule$minimum)
      }

      if (!is.null(rule$maximum) && value > rule$maximum) {
        stop("Argument '", key, "' was larger than maximum value: ", rule$maximum)
      }

      return(value)
    },
    #' @description
    #' Checks that the given value exists in the enum of the given rule.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains the enum list to check against.
    #' @param value String/numeric to validate
    check_enum = function(key, rule, value) {
      if (is.null(rule$enum)) return(value)
      if (is.null(value)) return(value)
      if (value %in% rule$enum) return(value)

      stop(
        "Argument '", key, "' value '",
        value, "', was not one of allowed values: '",
        paste(enum, collapse = ", "),
        "'"
      )
    },
    #' @description
    #' Checks that the given value is a Date.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset that contains the enum list to check against.
    #' @param value Date to validate
    check_date = function(key, rule, value) {
      if (inherits(value, "Date")) {
        return(value)
      }

      if (!is.character(value)) {
        stop("Argument '", key, "' was not a Date (see `as.Date` for details) or date formatted string")
      }

      return(as.Date(value))
    },
    #' @description
    #' Checks that the given value is the type that the given ruleset specifies.
    #'
    #' @param key Key of element in parent named list.
    #' @param rule Ruleset for the given value.
    #' @param value Value to validate.
    check_type = function(key, rule, value) {
      type <- rule$type

      if (is.null(type)) {
        stop("Rule for argument '", key, "' did not have 'type' specified")
      } else if (type == "list") {
        return(
          self$check_list(key, rule, value)
        )
      } else if (type == "named_list") {
        return(
          self$check_named_list(key, rule, value)
        )
      } else if (type == "generic_named_list") {
        return(
          self$check_generic_named_list(key, rule, value)
        )
      } else if (type == "data.table") {
        return(
          self$check_data.table(key, rule, value)
        )
      } else if (type == "date") {
        return(
          self$check_date(key, rule, value)
        )
      }

      if (type == "string" && !is.character(value)) {
        stop("Argument '", key, "' was not a string")
      } else if (type == "integer" && !self$is_integer(value)) {
        stop("Argument '", key, "' was not an integer")
      } else if (type == "numeric" && !is.numeric(value)) {
        stop("Argument '", key, "' was not a numeric")
      }

      value <- self$check_range(key, rule, value)
      value <- self$check_enum(key, rule, value)

      return(value)
    },
    #' @description
    #' Applies the rule for the argument of the given key.
    #'
    #' @param args All arguments given to the run function.
    #' @param key Key of argument to validate.
    #' @returns Arguments after validation of key was done.
    handle_rule = function(args, key) {
      rule <- private$rules[[key]]

      if (is.null(rule)) {
        stop("Rule for key '", key, "' is NULL")
      } else if (!self$is_named_list(rule)) {
        stop("Rule for key '", key, "' is not a named list: (", class(rule), ") -> ", rule)
      }

      value <- args[[key]]

      if (is.null(value)) {
        if (isTRUE(rule$required)) {
          stop("Required argument '", key, "' was NULL")
        }

        if (!is.null(rule$default)) {
          value <- rule$default
        }
      }

      if (is.function(rule$custom_handler)) {
        value <- rule$custom_handler(args, value)
      }

      if (!is.null(value)) {
        value <- self$check_type(key, rule, value)
      }

      args[[key]] <- value

      return(args)
    },
    #' @description
    #' Adds a post validation function that is applied to the arguments after validation.
    #'
    #' @param f Function to use in post-validation.
    add_post_validation = function(f) {
      private$post_validation <- f
    },
    #' @description
    #' Runs the validation on the given arguments.
    #'
    #' @returns Validated arguments.
    run = function(...) {
      args <- list(...)

      for (key in names(private$rules)) {
        args <- self$handle_rule(args, key)
      }

      if (is.function(private$post_validation)) {
        new_args <- private$post_validation(args, private$rules)

        if (!is.null(new_args)) {
          args <- new_args
        }
      }

      return(args)
    }
  )
)
