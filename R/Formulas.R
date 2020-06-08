#' @title Formulas Class
#'
#' @description
#' Key-value dictionary contains `formula` class objects.
#' This class is used maily to define a model's response variable and features
#' and passed to [Options] class. Note that all formulas must have
#' the same labels (response variable).
#'
#' @export
Formulas <- R6::R6Class(
  classname = "Formulas",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `Formulas` class object.
    #' @param ... Key and value pairs.
    #' @return A new `Formulas` class object
    initialize = function(...) {
      super$initialize(..., .class = "formula", .key_prefix = "fml_")
    },

    #' @description
    #' Print object.
    print = function() {
      cat(crayon::bgRed(" Formulas "), "\n")
      .print_sep()
      super$print_items(items_name = "Formulas", format_fun = .str_call)
      .print_sep()
    },

    #' @description
    #' Adds key and value pairs.
    #' @param ... Key and value pairs.
    #' Checked if the labels (response variabel) is the same with the current
    #' items.
    #' @return `Formulas` object by `invisible(self)`.
    add = function(...) {
      private$.safely({
        dots <- private$.set_global_env(...)
        rlang::exec(super$add, !!!dots)
        private$.check_response()
      })
    }
  ),
  private = rlang::list2(
    .set_global_env = function(...) {
      dots <- list(...)
      purrr::map(dots, function(value) {
        if (rlang::is_formula(value)) environment(value) <- globalenv()
        value
      })
    },

    .check_response = function() {
      num_response <- length(unique(purrr::map(self$values, ~ all.vars(.)[1L])))
      if (num_response != 1L)
          stop("All formula response must be the same.", call. = FALSE)
    }
  )
)

#' Formulas Class Constructor
#'
#' @rdname Formulas
#'
#' @param ... Key and value pairs.
#'
#' @return A new [Formulas] class object.
#'
#' @export
new_formulas <- function(...) {
  Formulas$new(...)
}
