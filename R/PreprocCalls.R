#' @title PreprocCalls Class
#'
#' @description
#' Key-value dictionary contains `call` class objects. The calls must
#' be calls that accept `data` (`data.frame`) and generate
#' `data.frame`.
#'
#' @export
PreprocCalls <- R6::R6Class(
  classname = "PreprocCalls",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `PreprocCalls` class object.
    #' @param ... Key and value pairs.
    #' @return A new 'PreprocCalls' class object
    initialize = function(...) {
      if (length(list(...)) != 0) {
        super$initialize(..., .class = "call", .key_prefix = "ppc_")
      } else {
        ## Set a default preproc call. `preproc_identity()` does nothing, but
        ## need to keep Option object stores at least one item.
        super$initialize(quote(preproc_identity(data)),
                         .class = "call", .key_prefix = "ppc_")
      }
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 11
      cat(crayon::bgRed(" Preproc calls "), "\n")
      .print_sep()
      super$print_items(items_name = "Preproc calls", format_fun = .str_call)
      .print_sep()
    },
  )
)

#' PreprocCalls Class Constructor
#'
#' @rdname PreprocCalls
#'
#' @param ... Key and value pairs.
#'
#' @return A new `PreprocCalls` class object.
#'
#' @export
new_preproc_calls <- function(...) {
  PreprocCalls$new(...)
}

#' Preproc call that does nothing
#'
#' @param data A `data.frame` object.
#' @return Input `data.frame`
#'
#' @export
preproc_identity <- function(data) {
  data
}
