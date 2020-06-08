#' @title FitParamSpecs Class
#'
#' @description
#' Key-value dictionary contains [FitParamSpec] class objects.
#' This class is passed to [Model] class and defines parameters of
#' model's fit functon.
#'
#' @export
FitParamSpecs <- R6::R6Class(
  classname = "FitParamSpecs",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `FitParamSpecs` class object.
    #' @param ... Key and value pairs.
    #' @return A new `FitParamSpecs` class object
    initialize = function(...) {
      ## No need to set `.key_prefix` as it will definitely be an explicit key
      ## or FitParamSpec name.
      super$initialize(..., .class = "FitParamSpec")
    },

    #' @description
    #' Adds key-value objects to the dictionary.
    #' @param ... Key and value pairs.
    add = function(...) {
      dots <- list(...)
      names(dots) <- private$.name_as_key(dots)
      rlang::exec(super$add, !!!dots)
    },

    #' @description
    #' Print object.
    print = function() {
      cat(crayon::bgRed(" Fit param specs "), "\n")
      .print_sep()
      super$print_items(items_name = "Fit param spec",
                        format_fun = .str_fit_param_spec)
      .print_sep()
    },

    #' @description
    #' Get paramter grid.
    #' @return A `data.frame` of parameter grid.
    get_grid = function() {
      x <- purrr::map(self$values, "dials_param")
      levels <- purrr::map_int(self$values, "nlevels")
      dials::grid_regular(x, levels = levels)
    },

    #' @description
    #' Get random paramter grid.
    #' @param size A integer scalar of parameter size.
    #' @return A `data.frame` of parameter grid.
    get_random = function(size = NULL) {
      .assert_count(size)
      x <- purrr::map(self$values, ~ .$get_dials_param(quant_values = FALSE))
      dials::grid_random(x, size = size)
    },

    #' @description
    #' Get information for BayesianSearch.
    #' @return A `list` of parameter infomation.
    get_bayes_info = function() {
      grid <- self$get_grid()
      uniques <- purrr::map(as.list(grid), unique)
      ## fixed params
      fixed <- purrr::keep(uniques, ~ length(.) == 1L)
      if (length(fixed) == 0L) fixed <- NULL
      ## tuned params
      tuned <- purrr::keep(uniques, ~ length(.) > 1L)
      purrr::iwalk(tuned, function(value, param) {
        if (!is.numeric(value))
          stop(param, " is not a numeric parameter. ",
               "All tuning parameters for BayesSearch must be numeric.",
               call. = FALSE)
      })
      if (length(tuned) == 0L) {
        tuned <- NULL
        bounds <- NULL
      } else {
        bounds <- purrr::map(tuned, ~ c(min(.), max(.)))
      }
      list(fixed = fixed, tuned = tuned, bounds = bounds)
    }
  ),
  active = rlang::list2(
    #' @field defaults A list of default values
    defaults = function() {
      purrr::map(tibble::deframe(self$items), "trans_default")
    },
  ),
  private = rlang::list2(
    .name_as_key = function(dots) {
      purrr::imap_chr(dots, function(fps, key) {
        if (rlang::is_string(key) && key != "") return(key)
        if (inherits(fps, "FitParamSpec")) return(fps$name)
        NA_character_
      })
    },

    .is_equal = function(x, y) {
      x$name == y$name
    },
  )
)

#' FitParamSpecs Class Constructor
#'
#' @rdname FitParamSpecs
#' @param ... Key and value pairs.
#' @return A new `FitParamSpecs` class object.
#'
#' @export
new_fit_param_specs <- function(...) {
  FitParamSpecs$new(...)
}
