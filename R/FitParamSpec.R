#' @title FitParamSpec Class
#'
#' @description
#' A class defines the model fit parameter specification.
#'
#' @export
FitParamSpec <- R6::R6Class(
  classname = "FitParamSpec",
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new `FitParamSpec` class object.
    #' @param name A character scalar of parameter name.
    #' @param values A double, integer, character or logical vector of parameter
    #' values.
    #' @param default A default value. Must be a value within values.
    #' @param trans A `trans` class object from [scales] package.
    #' @return A new `FitParamSpec` class object
    initialize = function(name = NULL, values = NULL, default = NULL,
                          trans = NULL) {
      private$.name <- .assert_str(name)
      private$.values <- private$.assert_values(values)
      private$.default <- private$.assert_default(default)
      private$.trans <- .assert_class(trans, "trans", allow_null = TRUE)
      private$.dials_param <- self$get_dials_param()
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 8L
      cat(crayon::bgRed(" Fit param spec "), "\n")
      .print_sep()
      .print("Name", self$name, key_width, FALSE, TRUE)
      ## trans
      if (!is.null(self$trans))
        .print("Trans", self$trans$name, key_width, FALSE, TRUE)

      ## trans values
      if (self$type == "character") {
        values <- paste0("[", .str_quote(self$trans_values), "]")
      } else {
        values <- paste0("[", paste(self$trans_values, collapse = ", "), "]")
      }
      .print("Values", values, key_width, FALSE, FALSE)

      ## default
      if (self$has_default) {
        quote_value <- self$type == "character"
        .print("Default", self$trans_default, key_width, FALSE, quote_value)
      }
      .print_sep()
    },

    #' @description
    #' Print object.
    #' @param quant_values A logical scalar wheather to set fixed values.
    #' @return A new [dials] class object.
    get_dials_param = function(quant_values = TRUE) {
      .assert_flag(quant_values)

      type <- typeof(self$values)
      default <- if (self$has_default) self$default else dials::unknown()
      label <- rlang::chr(!!self$name := paste("#", self$name))

      if (typeof(self$values) %in% c("double", "integer")) {
        dials::new_quant_param(
          type = type,
          values = if (quant_values) self$values else NULL,
          default = default,
          range = c(min(self$values), max(self$values)),
          inclusive = c(TRUE, TRUE),
          trans = self$trans,
          label = label,
          finalize = NULL
        )
      } else if (typeof(self$values) %in% c("character", "logical")) {
        dials::new_qual_param(
          type = type,
          values = self$values,
          default = default,
          label = label,
          finalize = NULL)
      } else {
        stop(typeof(self$values), " is not supported type.", call. = FALSE)
      }
    },
  ),
  active = rlang::list2(
    #' @field name A parameter name.
    name = function() private$.name,
    #' @field values parameter values.
    values = function(values = NULL) {
      if (is.null(values)) {
        private$.values
      } else {
        private$.values <- private$.assert_values(values)
        private$.dials_param <- self$get_dials_param()
        invisible(self)
      }
    },
    #' @field default A default parameter value.
    default = function() private$.default,
    #' @field trans A trans class object from sclaes package.
    trans = function() private$.trans,
    #' @field nlevels A lenght of parameters.
    nlevels = function() length(self$values),
    #' @field type A parameter type.
    type = function() typeof(self$values),
    #' @field range A parameter range.
    range = function() {
      if (!self$type %in% c("double", "integer")) return(NULL)
      list(lower = min(self$values), upper = max(self$values))
    },
    #' @field has_default A logical scalar wheather to have default value
    has_default = function() {
      !is.null(private$.default)
    },
    #' @field trans_values Translated values
    trans_values = function() {
      if (is.null(self$trans)) return(self$values)
      dials::value_seq(self$dials_param, self$nlevels)
    },
    #' @field trans_default A translated default value
    trans_default = function() {
      if (is.null(self$default) || is.null(self$trans))
        return(self$default)
      dials::value_seq(self$dials_param, 1L)
    },
    #' @field dials_param A param object from dials package
    dials_param = function() private$.dials_param,
  ),
  private = rlang::list2(
    .assert_values = function(values) {
      if (!(is.double(values) || is.integer(values) ||
              is.character(values) || is.logical(values))
          || anyNA(values))
        stop("Values must be double, integer, character or logical ",
             "and must not contains NAs.", call. = FALSE)
      values
    },

    .assert_default = function(default) {
      if (!(length(default) == 1L && !is.na(default) &&
              typeof(self$values) == typeof(default)))
        stop("Default must be length 1 and the same type of the values.",
             call. = FALSE)
      default
    },

    .name = NULL,
    .values = NULL,
    .default = NULL,
    .trans = NULL,
    .dials_param = NULL,
  )
)

#' FitParamSpec Class Constructor
#'
#' @rdname FitParamSpec
#'
#' @param name A character scalar of parameter name.
#' @param values A double, integer, character or logical vector of parameter
#' values.
#' @param default A default value. Must be a value within values.
#' @param trans A trans class object from scales package.
#' @return A new `FitParamSpec` class object
#'
#' @return A new `FitParamSpec` class object.
#'
#' @export
new_fit_param_spec <- function(name = NULL, values = NULL, default = NULL,
                               trans = NULL) {
  FitParamSpec$new(name, values, default, trans)
}
