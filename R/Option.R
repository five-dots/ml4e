#' @include helper_assertion.R
NULL

#' @title Option Class
#'
#' @description
#' Key-value dictionary type base class contains a machine learning project
#' option. A majority of features are derived from [Dict::Dict] class.
#'
#' Some concreate classes that stores a project option are also defined in this
#' package, so you do not need to play with this class directly.
#'
#' @export
Option <- R6::R6Class(
  "Option",
  inherit = Dict::Dict,
  public = rlang::list2(
    #' @description
    #' Construct a new `Option` class object.
    #' @param ... Key and value pairs.
    #' @param .class A character scalar of object's class. Should be one of the
    #' outputs from [class()].
    #' @param .key_prefix A character scalar of key prefix. Used for an auto
    #' complement key's prefix, if the key is omitted.
    #' @param .key_width A integer scalar of auto increment key number width.
    #' If `.key_width = 2L` (default) and `.key_prefix = "op"` (default), the
    #' first auto complement key will be `op01`.
    #' @param .value_width A integer scalar of character length. Used as max
    #' width of output value by `self$print()` method.
    #' @return A new `Option` class object.
    initialize = function(..., .class = "any", .key_prefix = "opt",
                          .key_width = 2L, .value_width = Inf) {
      private$.key_prefix <- .assert_str(.key_prefix)
      private$.key_width <- .assert_count(.key_width)
      private$.value_width <- .assert_count(.value_width) # only writable

      ## Items must be added after .key_prefix and .key_width set
      super$initialize(..., .class = .class, .overwrite = FALSE)
      private$.check_if_empty()
    },

    #' @description
    #' Print option items.
    #' @param items_name A character scalar of printed item name.
    #' @param format_fun A funtion to format item values. Must be a function
    #' that receives a option's value and returns a character scalar.
    print_items = function(items_name = "Objects", format_fun = NULL) {
      .assert_str(items_name)
      .assert_class(format_fun, "function")

      key_width <- max(stringr::str_length(self$keys)) + 3L # 3 means "" and :
      cat(items_name, ":", "\n", sep = "")
      purrr::pwalk(self$items, function(key, value) {
        value <- format_fun(value)
        .print(key, value, key_width, TRUE, FALSE, self$value_width)
      })
    },

    #' @description
    #' Adds key-value objects to the dictionary.
    #' @param ... Key and value pairs.
    add = function(...) {
      dots <- list(...) %>%
        private$.delete_duplicates() %>%
        private$.complement_keys()
      if (length(dots) > 0L) rlang::exec(super$add, !!!dots)
      invisible(self)
    },

    #' @description
    #' Removes objects from the dictionary.
    #' @param key A character scalar.
    remove = function(key = NULL) {
      if (nrow(super$items) == 1L && self$has(key))
        stop("Cannot remove the last item, as Option object must have at least",
             " one item.", call. = FALSE)
      super$remove(key)
      invisible(self)
    },

    #' @description
    #' Overwrite super class's `self$clear()` to throw an error which ensures
    #' the Option object always has at least one item.
    clear = function() {
      stop("clear() is disabled by Option class.", call. = FALSE)
    }
  ),
  active = rlang::list2(
    #' @field value_width max print width.
    value_width = function(value_width = NULL) {
      if (is.null(value_width)) {
        private$.value_width
      } else {
        private$.value_width <- .assert_count(value_width)
      }
    },
  ),
  private = rlang::list2(
    .generate_key = function() {
      repeat {
        key_num <- stringr::str_pad(private$.key_count,
                                    width = private$.key_width, pad = "0")
        key <- paste0(private$.key_prefix, key_num)
        private$.key_count <- private$.key_count + 1L
        if (!self$has(key)) break
      }
      key
    },

    .complement_keys = function(dots) {
      for (i in seq_along(dots)) {
        name <- names(dots)[i]
        ## Go to next if valid name
        if (rlang::is_string(name) && name != "") next
        names(dots)[i] <- private$.generate_key()
      }
      dots
    },

    .is_equal = function(x, y) {
      identical(x, y)
    },

    .delete_duplicates = function(dots) {
      if (length(dots) == 0L) return(dots)

      ## Check duplicates in dots
      ## `new_dots` keeps unique values only
      new_dots <- list()
      for (i in seq_along(dots)) {
        is_duplicate <- FALSE
        for (j in seq_along(new_dots)) {
          if (private$.is_equal(dots[[i]], new_dots[[j]])) {
            is_duplicate <- TRUE
            next
          }
        }
        if (!is_duplicate) new_dots <- c(new_dots, dots[i])
      }

      ## Check duplicates with current values
      res <- list()
      values <- tibble::deframe(self$items)
      for (i in seq_along(new_dots)) {
        is_duplicate <- FALSE
        for (j in seq_along(values)) {
          if (private$.is_equal(new_dots[[i]], values[[j]])) {
            is_duplicate <- TRUE
            next
          }
        }
        if (!is_duplicate) res <- c(res, new_dots[i])
      }
      res
    },

    .check_if_empty = function() {
      if (nrow(super$items) == 0L)
        stop("Options must have at least one row.", call. = FALSE)
    },

    .safely = function(expr) {
      ## Keep the items to be restored in case of any error
      items <- self$items
      res <- tryCatch(expr, error = function(e) e)

      if (inherits(res, "simpleError")) {
        ## Rollback items
        private$.items <- items
        stop(res$message, call. = FALSE)
      }
      invisible(self)
    },

    .key_prefix = character(),
    .key_width = integer(),
    .key_count = 1L,
    .value_width = integer(),
  )
)
