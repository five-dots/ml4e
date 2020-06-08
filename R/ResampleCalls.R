#' @title ResampleCalls Class
#'
#' @description
#' Key-value dictionary contains `call` class objects. The calls must be calls
#' that generate `rset` class object from [rsample::rsample] package.
#' This class is usually passed to [Datasets] class then used to generate
#' resamples from datasets.
#'
#' @export
ResampleCalls <- R6::R6Class(
  classname = "ResampleCalls",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `ResampleCalls` class object.
    #' @param ... Key and value pairs.
    #' @return A new `ResampleCalls` class object
    initialize = function(...) {
      super$initialize(..., .class = "call", .key_prefix = "rsp_")
    },
    #' @description
    #' Print object.
    print = function() {
      key_width <- 11L
      cat(crayon::bgRed(" Resample calls "), "\n")
      .print_sep()
      super$print_items(items_name = "Resample calls", format_fun = .str_call)
      .print_sep()
    },
    #' @description
    #' Get resample data.
    #' @param data A `data.frame` to be converted to a resample.
    #' @param resample_call_key A character or integer scalar of the
    #' resample_calls keys.
    #' @param seed A integer scalar seed.
    #' @return A resample of \code{rset} class object.
    get_resample = function(data = NULL, resample_call_key = NULL,
                            seed = NULL) {
      .assert_class(data, "data.frame")
      .assert_seed(seed)

      resample_call <- self[resample_call_key]
      ## generate reample
      mask <- rlang::new_data_mask(rlang::env(data = data))
      set.seed(seed)
      resample <- rlang::eval_tidy(resample_call, mask)
      private$.assert_resample(resample)
    },
  ),
  private = rlang::list2(
    .assert_resample = function(resample) {
      resample <- .assert_class(resample, "rset")
      ## resampling from rsample::nested_cv() is not allowed for cv data.
      ## This name check ensures that the resmaple object is generated from
      ## suported functions.
      valid_names <- c("splits", "id", "id2")
      if (!all(colnames(resample) %in% valid_names))
        stop("resample object must have valid names.", call. = FALSE)
      resample
    },
  )
)

#' ResampleCalls Class Constructor
#'
#' @rdname ResampleCalls
#' @param ... Key and value pairs.
#' @return A new `ResampleCalls` class object.
#'
#' @export
new_resample_calls <- function(...) {
  ResampleCalls$new(...)
}
