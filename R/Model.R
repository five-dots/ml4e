#' @title Model Class
#'
#' @description
#' A class defines the model specifications.
#'
#' @export
Model <- R6::R6Class(
  classname = "Model",
  public = rlang::list2(
    #' @description
    #' Construct a new `Model` class object.
    #' @param engine A character scalar of model engines.
    #' @param preproc_calls A [PreprocCalls] class object.
    #' @param fit_params A [FitParams] class object. If `NULL`, [FitParams]
    #' with an emtpy list parameter is created.
    #' @param fit_param_specs A [FitParamSpecs] class object. If `NULL`,
    #' default specs is used. (If defined by the pacakge)
    #' @return A new `Model` class object
    initialize = function(engine = NULL, preproc_calls = NULL,
                          fit_params = NULL, fit_param_specs = NULL) {
      private$.engine <- .assert_engine(engine)
      private$.preproc_calls <- .assert_class(preproc_calls, "PreprocCalls",
                                              allow_null = TRUE)
      private$.fit_params <- .assert_class(fit_params, "FitParams",
                                           allow_null = TRUE)
      private$.fit_param_specs <- .assert_class(fit_param_specs,
                                                "FitParamSpecs",
                                                allow_null = TRUE)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      data_call <- .str_call(self$data_call)
      fit_call <- .str_call(self$fit_call)
      pred_call <- .str_call(self$pred_call)

      cat(crayon::bgRed(" Model "), "\n")
      .print_sep()
      .print("Engine", private$.engine, key_width, FALSE, TRUE)
      .print("Data call", data_call, key_width, FALSE, FALSE)
      .print("Fit call", fit_call, key_width, FALSE, FALSE)
      .print("Predict call", pred_call, key_width, FALSE, FALSE)

      ## PreprocCalls
      .print_sep()
      self$preproc_calls$print_items(items_name = "Preproc calls",
                                     format_fun = .str_call)

      ## FitParas
      .print_sep()
      self$fit_params$print_items(items_name = "Fit params",
                                  format_fun = .str_list)

      ## FitParamSpecs
      if (!is.null(self$fit_param_specs)) {
        .print_sep()
        self$fit_param_specs$print_items(items_name = "Fit param specs",
                                         format_fun = .str_fit_param_spec)
      }
      .print_sep()
    },

    #' @description
    #' Get a model's fit call by a fit_params key.
    #' @param fit_param_key A character or integer scalar of the fit_params
    #' keys.
    #' @return A call of the model fit.
    get_fit_call_by_key = function(fit_param_key = NULL) {
      fit_param <- self$fit_params[fit_param_key]
      self$get_fit_call(fit_param)
    },

    #' @description
    #' Get a model's fit call by a fit_param.
    #' @param fit_param A list of fit parameters.
    #' @return A call of the model fit.
    get_fit_call = function(fit_param = NULL) {
      .assert_fit_param(fit_param)
      fixed <- self$spec$fit_fixed

      ## User fit args (override by fixed args)
      user <- fit_param[!names(fit_param) %in% names(fixed)]
      if (length(user) == 0L) user <- NULL

      ## Default fit args (override by user args)
      default <- self$spec$fit_default
      default <- default[!names(default) %in% names(user)]
      if (length(default) == 0L) default <- NULL

      ## Build a fit call from all args
      args <- c(fixed, user, default)
      fun <- self$spec$fit_fun
      ns <- self$spec$fit_ns
      rlang::call2(fun, !!!args, .ns = ns)
    },
  ),
  active = rlang::list2(
    #' @field engine A model engine.
    engine = function() private$.engine,
    #' @field fit_params [FitParams] class object.
    fit_params = function() {
      ## Default fit_params
      if (is.null(private$.fit_params)) {
        private$.fit_params <- new_fit_params()
      }
      private$.fit_params
    },
    #' @field fit_param_specs A [FitParamSpecs] class object.
    fit_param_specs = function(fit_param_specs = NULL) {
      if (is.null(fit_param_specs)) {
        private$.fit_param_specs
      } else {
        private$.fit_param_specs <- .assert_class(fit_param_specs,
                                                  "FitParamSpecs",
                                                  allow_null = FALSE)
      }
    },
    #' @field preproc_calls A [PreprocCalls] class object.
    preproc_calls = function() {
      ## Default preproc_calls
      if (is.null(private$.preproc_calls)) {
        private$.preproc_calls <- new_preproc_calls()
      }
      private$.preproc_calls
    },
    #' @field has_fit_param_specs Wheather the model has fit_param_specs.
    has_fit_param_specs = function() !is.null(self$fit_param_specs),

    #' @field spec A `list` of model specifications.
    spec = function() {
      model_specs %>%
        dplyr::filter(engine == self$engine) %>%
        purrr::transpose() %>%
        purrr::pluck(1L)
    },
    #' @field data_call A `call` used to convert a `data.frame` to the format
    #' suited for the model input.
    data_call = function() {
      fun <- self$spec$data_fun
      args <- list(object = quote(object), formula = quote(formula))
      rlang::call2(fun, !!!args, .ns = "ml4e")
    },
    #' @field fit_call A \code{\link{call}} in fit routine.
    fit_call = function() self$get_fit_call(),
    #' @field pred_call A \code{\link{call}} in predict routine.
    pred_call = function() {
      fixed <- self$spec$pred_fixed
      args <- c(fixed) # placeholder
      fun <- self$spec$pred_fun
      ns <- self$spec$pred_ns
      rlang::call2(fun, !!!args, .ns = ns)
    },
  ),
  private = rlang::list2(
    .engine = character(),
    .fit_params = NULL,
    .fit_param_specs = NULL,
    .preproc_calls = NULL,
  )
)

#' Model Class Constructor
#'
#' @rdname Model
#'
#' @param engine A character scalar of model engines.
#' @param preproc_calls A \code{\link{PreprocCalls}} class object.
#' @param fit_params A \code{\link{FitParams}} class object. If \code{NULL},
#' \code{\link{FitParams}} with an emtpy list parameter is created.
#' @param fit_param_specs A \code{\link{FitParamSpecs}} class object. If
#' \code{NULL}, default specs is used. (If defined by the pacakge)
#'
#' @return A new \code{Model} class object.
#'
#' @export
new_model <- function(engine = NULL, preproc_calls = NULL, fit_params = NULL,
                      fit_param_specs = NULL) {
  Model$new(engine, preproc_calls, fit_params, fit_param_specs)
}
