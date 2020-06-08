#' @title Options Class
#'
#' @description
#' A class stores all machine learning project [Option]. This class checks if
#' the option have valid values each other. (e.g. All data in Datasets have the
#' label variable described in the formulas, or all models support the task
#' type, etc.)
#'
#' Methods that do actual tasks are not defined in the classs. The concept is,
#' gather the all options in one place and pass it to [Task] and [Project]
#' classes.
#'
#' @export
Options <- R6::R6Class(
  classname = "Options",
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new `Options` class object.
    #'
    #' @param formulas A [Formulas] class object.
    #' @param datasets A [Datasets] class object.
    #' @param seeds A [Seeds] class object.
    #' @param models A [Models] class object.
    #' @param measure A [Measure] class object. If `NULL`, a Measure class
    #' object with default metrics are used.
    #' @param show_progress A logical scalar wheather to show a progress bar.
    #' @param keep_data A logical scalar wheather to keep original data.
    #' If `TRUE`, the original dataset are kept in predicted values and
    #' cross-validation folds.
    #' @param parallel A logical scalar wheather to calculate parallely.
    #'
    #' @return A new `Options` class object
    initialize = function(formulas = NULL, datasets = NULL, seeds = NULL,
                          models = NULL, measure = NULL, show_progress = TRUE,
                          keep_data = FALSE, parallel = FALSE) {
      private$.formulas <- .assert_class(formulas, "Formulas")
      private$.datasets <- .assert_class(datasets, "Datasets")
      private$.seeds <- .assert_class(seeds, "Seeds")
      private$.models <- .assert_class(models, "Models")
      private$.measure <- .assert_class(measure, "Measure", allow_null = TRUE)

      private$.show_progress <- .assert_flag(show_progress)
      private$.keep_data <- .assert_flag(keep_data)
      private$.parallel <- .assert_flag(parallel)

      ## check if options are valid
      private$.check_models_task_type()
      private$.check_datasets_labels()
      private$.check_datasets_resample_calls()
    },

    #' @description
    #' Print object.
    print = function() {
      self$print_options()
      invisible(self)
    },

    #' @description
    #' Print Options object.
    #' @param title A character scalar of printed title.
    #' @param key_width A integer scalar of key column width.
    print_options = function(title = " Options ", key_width = 10L) {
      task <- .str_task(self$task_type, self$label_levels)
      metrics <- .str_list(self$measure$metrics)
      flags <- paste0("show_progress = ", self$show_progress, ", ",
                      "keep_data = ", self$keep_data, ", ",
                      "parallel = ", self$parallel)
      cat(crayon::bgRed(title), "\n")
      .print_sep()
      .print("Task type", task, key_width, FALSE, FALSE)
      .print("Metrics", metrics, key_width, FALSE, FALSE)
      .print("Flags", flags, key_width, FALSE, FALSE)

      ## Formulas
      .print_sep()
      self$formulas$print_items(items_name = "Formulas", format_fun = .str_call)

      ## Datasets
      .print_sep()
      self$datasets$print_items(items_name = "Datasets", format_fun = .str_df)

      ## ResampleCalls
      .print_sep()
      self$datasets$resample_calls$print_items(items_name = "Resample calls",
                                               format_fun = .str_call)

      ## Seeds
      .print_sep()
      self$seeds$print_items(items_name = "Seeds", format_fun = .str_list)

      ## Models
      .print_sep()
      self$models$print_items(items_name = "Models", format_fun = .str_model)
      .print_sep()
    },
  ),
  active = rlang::list2(
    #' @field formulas A \code{Formulas} class object.
    formulas = function() private$.formulas,
    #' @field datasets A \code{Datasets} class object.
    datasets = function() private$.datasets,
    #' @field seeds A \code{Seeds} class object.
    seeds = function() private$.seeds,
    #' @field models A \code{Models} class object.
    models = function() private$.models,
    #' @field measure A \code{Measure} class object.
    measure = function() {
      if (is.null(private$.measure)) {
        ## default measure by task type
        private$.measure <- new_measure(self$task_type)
      }
      private$.measure
    },

    #' @field show_progress Wheather to show progress messages.
    show_progress = function(show_progress = NULL) {
      if (is.null(show_progress)) {
        private$.show_progress
      } else {
        private$.show_progress <- .assert_flag(show_progress)
      }
    },
    #' @field keep_data Wheather to keep data.
    keep_data = function(keep_data = NULL) {
      if (is.null(keep_data)) {
        private$.keep_data
      } else {
        private$.keep_data <- .asset_flag(keep_data)
      }
    },
    #' @field parallel Wheather to calculate parallely.
    parallel = function(parallel = NULL) {
      if (is.null(parallel)) {
        private$.parallel
      } else {
        private$.parallel <- .assert_flag(parallel)
      }
    },

    #' @field labels A vector of labels (response variable).
    labels = function() {
      data <- self$datasets[1L]
      formula <- self$formulas[1L]
      labels <- stats::model.response(stats::model.frame(formula, data))
      names(labels) <- NULL
      labels
    },
    #' @field label_var A label (response) variable name.
    label_var = function() all.vars(self$formulas[1L])[1L],
    #' @field label_levels Levels of label (response) variable.
    label_levels = function() levels(self$labels),
    #' @field task_type A task type.
    task_type = function() {
      labels <- self$labels
      if (is.matrix(labels)) stop("matrix labels is not currently supported.")

      if (is.factor(labels)) {
        if (nlevels(labels) == 2L) return("binary")
        if (nlevels(labels) > 2L) return("multiclass")
        stop("factor nlevels must be greater than or equal to 2.")
      } else if (is.numeric(labels)) {
        if (rlang::is_integerish(labels) && all(labels >= 0L)) return("poisson")
        return("regression")
      } else {
        stop("could not find an appropreate task from the label type.")
      }
    }
  ),
  private = rlang::list2(
    .check_models_task_type = function() {
      ## Check if all models support the task_type
      purrr::walk(self$models$values, function(model) {
        if (!self$task_type %in% supported_tasks(model$engine))
          stop(self$task_type, " is not a valid task_type for ",
               .str_quote(model$engine), " engine.", call. = FALSE)
      })
    },

    .check_datasets_labels = function() {
      ## Check if all datasets have label_var
      purrr::walk(self$datasets$keys, function(key) {
        if (!rlang::has_name(self$datasets[key], self$label_var))
          stop("datasets ", .str_quote(key), " does not have ",
               .str_quote(self$label_var), " labels.", call. = FALSE)
      })
    },

    .check_datasets_resample_calls = function() {
      if (!self$datasets$has_resample_calls)
        stop("datasets must have a resample_calls.", call. = FALSE)
    },

    .formulas = NULL,
    .datasets = NULL,
    .seeds = NULL,
    .models = NULL,
    .measure = NULL,
    .show_progress = TRUE,
    .keep_data = FALSE,
    .parallel = FALSE,
  )
)

#' Options Class Constructor
#'
#' @rdname Options
#'
#' @param formulas A [Formulas] class object.
#' @param datasets A [Datasets] class object.
#' @param seeds A [Seeds] class object.
#' @param models A [Models] class object.
#' @param measure A [Measure] class object. If `NULL`, a Measure class
#' object with default metrics are used.
#' @param show_progress A logical scalar wheather to show a progress bar.
#' @param keep_data A logical scalar wheather to keep original data.
#' If `TRUE`, the original dataset are kept in predicted values and
#' cross-validation folds.
#' @param parallel A logical scalar wheather to calculate parallely.
#'
#' @return A new `Options` class object
#'
#' @export
new_options <- function(formulas = NULL, datasets = NULL, seeds = NULL,
                        models = NULL, measure = NULL, show_progress = TRUE,
                        keep_data = FALSE, parallel = FALSE) {
  Options$new(formulas, datasets, seeds, models, measure, show_progress,
              keep_data, parallel)
}
