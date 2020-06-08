#' @title Seeds Class
#'
#' @description
#' Key-value dictionary contains named `integer` class objects.
#'
#' @export
Seeds <- R6::R6Class(
  classname = "Seeds",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `Seeds` class object.
    #' @param ... Key and value pairs.
    #' @return A new `Seeds` class object
    initialize = function(...) {
      if (length(list(...)) > 0L) {
        private$.check_names(...)
        super$initialize(..., .class = "integer", .key_prefix = "sed_")
      } else {
        seed <- private$.generate_random_seed()
        super$initialize(seed, .class = "integer", .key_prefix = "sed_")
      }
    },

    #' @description
    #' Print object.
    print = function() {
      cat(crayon::bgRed(" Seeds "), "\n")
      .print_sep()
      super$print_items(items_name = "Seeds", format_fun = .str_list)
      .print_sep()
    },

    #' @description
    #' Adds key-value objects to the dictionary.
    #' @param ... Key and value pairs.
    add = function(...) {
      private$.check_names(...)
      super$add(...)
    },

    #' @description
    #' Adds a random seed to the dictionary.
    add_random = function() {
      seed <- private$.generate_random_seed()
      super$add(seed)
    },
  ),
  private = rlang::list2(
    .check_names = function(...) {
      dots <- list(...)
      purrr::walk(dots, function(value) {
        missing <- setdiff(private$.names, names(value))
        if (length(missing) > 0L)
          stop(.str_quote(missing), " is missing in the seed names.",
               call. = FALSE)
      })
      invisible(NULL)
    },

    .generate_random_seed = function() {
      repeat {
        max_x <- min(10000, .Machine$integer.max)
        seed <- sample(0L:max_x, size = length(private$.names))
        names(seed) <- private$.names
        if (!private$.is_duplicated(seed)) break
      }
      seed
    },

    .is_duplicated = function(seed) {
      is_duplicated <- FALSE
      for (cur_seed in self$values) {
        for (name in private$.names) {
          if (seed[name] == cur_seed[name])
            is_duplicated <- TRUE
        }
      }
      is_duplicated
    },

    .names = c("search", "cv", "model"),
  )
)

#' Seeds Class Constructor
#'
#' @rdname Seeds
#'
#' @param ... Key and value pairs.
#'
#' @return A new \code{Seeds} class object.
#'
#' @export
new_seeds <- function(...) {
  Seeds$new(...)
}
