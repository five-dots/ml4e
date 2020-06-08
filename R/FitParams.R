#' @title FitParams Class
#'
#' @description
#' Key-value dictionary contains `list` class objects. This class is passed to
#' [Model] class and defines parameters of model's fit functon.
#'
#' @export
FitParams <- R6::R6Class(
  classname = "FitParams",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `FitParams` class object.
    #' @param ... Key and value pairs.
    #' @return A new `FitParams` class object
    initialize = function(...) {
      if (length(list(...)) != 0L) {
        super$initialize(..., .class = "list", .key_prefix = "par_")
      } else {
        super$initialize(default = list(), .class = "list", .key_prefix = "par_")
      }
    },

    #' @description
    #' Print object.
    print = function() {
      cat(crayon::bgRed(" Fit params "), "\n")
      .print_sep()
      super$print_items(items_name = "Fit param", format_fun = .str_list)
      .print_sep()
    },
  )
)

#' FitParams Class Constructor
#'
#' @rdname FitParams
#'
#' @param ... Key and value pairs.
#'
#' @return A new `FitParams` class object.
#'
#' @export
new_fit_params <- function(...) {
  FitParams$new(...)
}
