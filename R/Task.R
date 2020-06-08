#' @title Task Class
#'
#' @description
#' A base class describe a machine learning task specification. A task is
#' defined by a [Options] and `keys` which is named character vector specifiyng
#' option's key. The main functionality is evaluate quoted call arguments and
#' assign the acutal values to be called in the later task.
#'
#' @export
Task <- R6::R6Class(
  classname = "Task",
  public = rlang::list2(
    #' @description
    #' Construct a new `Task` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys. Names must be "formula",
    #' "dataset", "resample", "seed", "model", "preproc", "fit_param" and
    #' values must be key of the each options.
    #'
    #' @return A new `Task` class object
    initialize = function(options = NULL, keys = NULL) {
      private$.options <- .assert_class(options, "Options")
      private$.keys <- private$.assert_keys(keys)
    },

    #' @description
    #' Print object.
    print = function() {
      self$print_task()
      .print_sep()
      invisible(self)
    },

    #' @description
    #' Print task object.
    #'
    #' @param title A character scalar of printed title.
    #' @param key_width A integer scalar of key column width.
    print_task = function(title = " Task ", key_width = 14L) {
      task <- .str_task(self$task_type, self$label_levels)
      keys <- .str_list(self$keys)
      metrics <- .str_list(self$measure$metrics)
      flags <- paste0("show_progress = ", self$show_progress, ", ",
                      "keep_data = ", self$keep_data, ", ",
                      "parallel = ", self$parallel)

      preproc_call <- .str_call(self$preproc_call)
      resample_call <- .str_call(self$resample_call)
      data_call <- .str_call(self$data_call)
      fit_call <- .str_call(self$fit_call)
      pred_call <- .str_call(self$pred_call)

      cat(crayon::bgRed(title), "\n")
      .print_sep()
      .print("Task type", task, key_width, FALSE, FALSE)
      .print("Keys", keys, key_width, FALSE, FALSE)
      .print("Metrics", metrics, key_width, FALSE, FALSE)
      .print("Flags", flags, key_width, FALSE, FALSE)

      .print_sep()
      .print("Preproc call", preproc_call, key_width, FALSE, FALSE)
      .print("Resample call", resample_call, key_width, FALSE, FALSE)
      .print("Data call", data_call, key_width, FALSE, FALSE)
      .print("Fit call", fit_call, key_width, FALSE, FALSE)
      .print("Predict call", pred_call, key_width, FALSE, FALSE)
    },

    #' @description
    #' Evaluate call arguments in the task environment.
    #'
    #' @param call `call` object.
    #'
    #' @return A `call` object.
    eval_call_args = function(call = NULL) {
      if (is.null(call) || !is.call(call))
        stop("call must be a call object.", call. = FALSE)

      args <- rlang::call_args(call)

      ## Arguments that should not be evaluated here
      no_eval_args <- c("data", "eval", "x", "y", "eval_x", "eval_y",
                        "fitted", "object")

      eval_data <- rlang::new_data_mask(self)

      eval_args <- purrr::map(args, function(arg) {
        ## Skip if the arg is in no_eval_args
        if (rlang::is_symbol(arg) && rlang::as_string(arg) %in% no_eval_args)
          return(arg)
        rlang::eval_tidy(arg, eval_data)
      })

      ## rebuild a call with evaluated args
      fun <- rlang::call_name(call)
      ns <- rlang::call_ns(call)
      rlang::call2(fun, !!!eval_args, .ns = ns)
    },
  ),
  active = rlang::list2(
    #' @field options A [Options] object.
    options = function() private$.options,
    #' @field keys A named character vector of keys.
    keys = function() private$.keys,

    #' @field formulas A [Formulas] object.
    formulas = function() self$options$formulas,
    #' @field datasets A [Datasets] object.
    datasets = function() self$options$datasets,
    #' @field seeds A [Seeds] object.
    seeds = function() self$options$seeds,
    #' @field models A [Models] object.
    models = function() self$options$models,
    #' @field measure A [Measure] object.
    measure = function() self$options$measure,

    #' @field labels A vector of labels (response variable).
    labels = function() self$options$labels,
    #' @field label_var A label (response) variable name.
    label_var = function() self$options$label_var,
    #' @field label_levels Levels of label (response) variable.
    label_levels = function() self$options$label_levels,
    #' @field task_type A task type.
    task_type = function() self$options$task_type,

    #' @field show_progress Wheather to show progress messages.
    show_progress = function() self$options$show_progress,
    #' @field keep_data Wheather to keep data.
    keep_data = function() self$options$keep_data,
    #' @field parallel Wheather to calculate parallely.
    parallel = function() self$options$parallel,

    #' @field formula A tasks' formula.
    formula = function() self$formulas[self$keys["formula"]],
    #' @field dataset A task's dataset.
    dataset = function() self$datasets[self$keys["dataset"], self$preproc_call],
    #' @field train A task's train data.
    train = function() {
      self$datasets$get_train(self$keys["dataset"], self$preproc_call)
    },
    #' @field test A task's test data.
    test = function() {
      self$datasets$get_test(self$keys["dataset"], self$preproc_call)
    },
    #' @field resample_call A task's resample call.
    resample_call = function() {
      self$datasets$resample_calls[self$keys["resample"]]
    },
    #' @field seed A task's seed.
    seed = function() self$seeds[self$keys["seed"]],
    #' @field model A task's model.
    model = function() self$models[self$keys["model"]],

    #' @field search_seed A task's search seed for resampling.
    search_seed = function() {
      res <- self$seed["search"]
      names(res) <- NULL
      res
    },
    #' @field cv_seed A task's cv seed for resampling.
    cv_seed = function() {
      res <- self$seed["cv"]
      names(res) <- NULL
      res
    },
    #' @field model_seed A task's model seed.
    model_seed = function() {
      res <- self$seed["model"]
      names(res) <- NULL
      res
    },
    #' @field id_col A task's datasets' id_col.
    id_col = function() {
      if (self$datasets$has_id_col) self$datasets$id_col else ".id"
    },
    #' @field test_ids A task's datasets' test_ids.
    test_ids = function() self$datasets$test_ids,

    #' @field preproc_call A task's preproc call.
    preproc_call = function() {
      call <- self$model$preproc_calls[self$keys["preproc"]]
      self$eval_call_args(call)
    },
    #' @field data_call A task's data call.
    data_call = function() {
      call <- self$model$data_call
      self$eval_call_args(call)
    },
    #' @field fit_call A task's fit call.
    fit_call = function() {
      call <- self$model$fit_call
      self$eval_call_args(call)
    },
    #' @field pred_call A task's prediction call.
    pred_call = function() {
      ## add predict type
      call <- self$model$pred_call
      pred_type <- self$model$spec$pred_type[self$task_type]
      names(pred_type) <- NULL

      if (is.na(pred_type)) {
        type_arg <- list()
      } else if (self$model$engine %in% c("catboost_es")) {
        type_arg <- list(prediction_type = pred_type)
      } else {
        type_arg <- list(type = pred_type)
      }
      call <- rlang::call_modify(call, !!!type_arg)
      self$eval_call_args(call)
    },

    #' @field has_test_labels Wheather the dataset has test labels.
    has_test_labels = function() {
      if (!self$datasets$has_test_ids) return(FALSE)
      ## Check if test data contains lables or all NAs
      !all(is.na(dplyr::pull(self$test, self$label_var)))
    },
    #' @field result A task result.
    result = function() private$.result,
    #' @field done Wheather the task is finished.
    done = function() !is.null(self$result),
  ),
  private = rlang::list2(
    .assert_keys = function(keys) {
      if (!is.character(keys))
        stop("keys must be a character vector.", call. = FALSE)

      ## Formulas
      if (is.na(keys["formula"]))
        stop("Keys must have a key named \"formula\".", call. = FALSE)
      if (!self$formulas$has(keys["formula"]))
        stop("Formulas does not have ", .str_quote(keys["formula"]), " key.",
             call. = FALSE)

      ## Datasets
      if (is.na(keys["dataset"]))
        stop("Keys must have a key named \"dataset\".", call. = FALSE)
      if (!self$datasets$has(keys["dataset"]))
        stop("Datasets does not have ", .str_quote(keys["dataset"]), " key.",
             call. = FALSE)

      ## ResampleCalls
      if (is.na(keys["resample"]))
        stop("Keys must have a key named \"resample\".", call. = FALSE)
      if (!self$datasets$resample_calls$has(keys["resample"]))
        stop("ResampleCalls does not have ", .str_quote(keys["resample"]),
             " key.", call. = FALSE)

      ## Seeds
      if (is.na(keys["seed"]))
        stop("Keys must have a key named \"seed\".", call. = FALSE)
      if (!self$seeds$has(keys["seed"]))
        stop("Seeds does not have ", .str_quote(keys["seed"]), " key.",
             call. = FALSE)

      ## models
      if (is.na(keys["model"]))
        stop("Keys must have a key named \"model\".", call. = FALSE)
      if (!self$models$has(keys["model"]))
        stop("Models does not have ", .str_quote(keys["model"]), " key.",
             call. = FALSE)

      ## PreprocCalls
      if (is.na(keys["preproc"]))
        stop("Keys must have a key named \"preporc\".", call. = FALSE)
      if (!self$models[keys["model"]]$preproc_calls$has(keys["preproc"]))
        stop("PreprocCalls does not have ", .str_quote(keys["preproc"]),
             " key.", call. = FALSE)
      keys
    },

    .check_if_done = function() {
      if (!self$done)
        stop("Calculation is not finished. first execute self$do().",
             call. = FALSE)
    },

    .options = NULL,
    .keys = character(),
    .result = NULL,
  )
)
