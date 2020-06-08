#' @title Models Class
#'
#' @description
#' Key-value dictionary contains \code{\link{Model}} class objects.
#' This class is passed to \code{\link{Options}} class.
#'
#' @export
Models <- R6::R6Class(
  classname = "Models",
  inherit = Option,
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new \code{Models} class object.
    #' @param ... Key and value pairs.
    #' @return A new \code{Models} class object
    initialize = function(...) {
      super$initialize(..., .class = "Model", .key_prefix = "mod")
    },

    #' @description
    #' Print object.
    print = function() {
      cat(crayon::bgRed(" Models "), "\n")
      .print_sep()
      super$print_items(items_name = "Model", format_fun = .str_model)
      .print_sep()
    },

    #' @description
    #' Adds key-value objects to the dictionary.
    #' @param ... Key and value pairs.
    add = function(...) {
      dots <- list(...)
      names(dots) <- private$.engine_as_key(dots)
      rlang::exec(super$add, !!!dots)
    },
  ),
  private = rlang::list2(
    .engine_as_key = function(dots) {
      purrr::imap_chr(dots, function(model, key) {
        if (rlang::is_string(key) && key != "") return(key)
        if (inherits(model, "Model")) return(model$engine)
        NA_character_
      })
    },

    ## .is_equal = function(x, y) {
    ##   if (!inherits(x, "Model") || !inherits(y, "Model")) return(FALSE)
    ##   x$engine == y$engine
    ## },
  )
)

#' Models Class Constructor
#'
#' @rdname Models
#'
#' @param ... Key and value pairs.
#'
#' @return A new \code{Models} class object.
#'
#' @export
new_models <- function(...) {
  Models$new(...)
}
