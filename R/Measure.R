#' @title Measure Class
#'
#' @description
#' A class defines metrics specification.
#'
#' @export
Measure <- R6::R6Class(
  classname = "Measure",
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new `Measure` class object.
    #' @param task_type A character scalar of task type. See
    #' [supported_tasks] for tasks supported by a model/engine.
    #' @param metrics A character vector of model metrics. Metrics must be
    #' metrics of [supported_metrics]. If the vector is named, the names will be
    #' used insted of metrics' name itself. If `NULL`, default metrics will be
    #' set based on a `task_type`.
    #' @return A new `Measure` class object
    initialize = function(task_type = NULL, metrics = NULL) {
      private$.task_type <- .assert_task_type(task_type)
      private$.metrics <- private$.assert_metrics(metrics)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 10L
      metrics <- .str_list(self$metrics)
      cat(crayon::bgRed(" Measure "), "\n")
      .print_sep()
      .print("Task type", self$task_type, key_width, FALSE, TRUE)
      .print("Metrics", metrics, key_width, FALSE, FALSE)
      .print_sep()
    },

    #' @description
    #' Measure metrics of predictions.
    #' @param pred A `data.frame` of predictions.
    #' @param label_var A character scalar of label variable name.
    #' @return A `list` of metrics.
    do = function(pred = NULL, label_var = NULL) {
      private$.assert_pred(pred, label_var)
      .assert_str(label_var)
      metric_set <- private$.build_metric_set()
      if (any(stringr::str_starts(names(pred), ".prob_"))) {
        res <- metric_set(pred, truth = !!rlang::sym(label_var),
                          estimate = .pred, dplyr::starts_with(".prob_"))
      } else {
        res <- metric_set(pred, truth = !!rlang::sym(label_var),
                          estimate = .pred)
      }
      ## convert list
      res %>%
        dplyr::mutate(.metric = self$metric_names) %>%
        tidyr::pivot_wider(id_cols = -.estimator,
                           names_from = .metric,
                           values_from = .estimate) %>%
        purrr::transpose() %>% purrr::pluck(1L)
    },

    #' @description
    #' Assert a metric name.
    #' @param metric_name A character scalar of metric name.
    #' @param conversion A logical scalar wheather to convert metric name.
    #' @return A character scalar of metric name.
    assert_metric_name = function(metric_name = NULL, conversion = TRUE) {
      .assert_str(metric_name, allow_null = TRUE)
      .assert_flag(conversion)
      metric_names <- self$metric_names
      metrics <- self$metrics
      if (conversion) {
        if (is.null(metric_name))
          metric_name <- metric_names[1L]
        if (!metric_name %in% metric_names && metric_name %in% metrics)
          metric_name <- metric_names[which(metrics == metric_name)]
      }
      if (!metric_name %in% metric_names)
        stop("metric_name must be one of ", .str_quote(metric_names), ".",
             call. = FALSE)
      metric_name
    },

    #' @description
    #' Wheather the metric is minimize function.
    #' @param metric_name A character scalar of metric name. If NULL, the first
    #' metric_name in \code{options$measure$metric_names} is used.
    #' @return A logical scalar.
    is_minimize_metric = function(metric_name = NULL) {
      metric_name <- self$assert_metric_name(metric_name, conversion = TRUE)
      supported_metrics() %>%
        dplyr::filter(metric == !!self$metrics[metric_name]) %>%
        dplyr::pull(minimize)
    },
  ),
  active = rlang::list2(
    #' @field task_type A task type.
    task_type = function(task_type = NULL) {
      if (is.null(task_type)) {
        private$.task_type
      } else {
        private$.task_type <- task_type
      }
    },
    #' @field metrics A character vector of metrics.
    metrics = function(metrics = NULL) {
      if (is.null(metrics)) {
        ## getter
        if (!is.null(private$.metrics)) {
          private$.metrics
        } else {
          ## default names
          switch(
            self$task_type,
            "multiclass" = c(acc = "accuracy", logloss = "mn_log_loss"),
            "binary"     = c(acc = "accuracy", logloss = "mn_log_loss"),
            "regression" = c(rmse = "rmse", mae = "mae"),
            "poisson"    = c(rmse = "rmse"),
            stop("default metric names are not defined for ", self$task_type,
                 ".", call. = FALSE)
          )
        }
      } else {
        ## setter
        private$.metrics <- private$.assert_metrics(metrics)
      }
    },
    #' @field metric_names A character vector of metric names.
    metric_names = function() {
      metrics <- self$metrics
      names <- names(metrics)
      if (is.null(names)) names <- rep(NA_character_, length(metrics))
      names <- dplyr::na_if(names, "")
      dplyr::coalesce(names, metrics)
    },
  ),
  private = rlang::list2(
    .assert_metrics = function(metrics) {
      ## assertion
      if (is.null(metrics)) return(NULL)
      if (!is.character(metrics))
        stop("metrics must be a character vector.", call. = FALSE)
      ## valid metrics for task_type
      df <- supported_metrics()
      valid_metrics <- switch(
        self$task_type,
        "multiclass" = dplyr::filter(df, type == "class" | type == "prob"),
        "binary"     = dplyr::filter(df, type == "class" | type == "prob"),
        "regression" = dplyr::filter(df, type == "numeric"),
        "poisson"    = dplyr::filter(df, type == "numeric"),
        stop("could not find valid metrics for the ", self$task_type, " task.",
            call. = FALSE)
      )
      invalids <- purrr::map(metrics, function(metric) {
        if (!metric %in% valid_metrics$metric) metric
      }) %>% purrr::compact() %>% unlist()
      if (length(invalids) > 0L)
        stop(paste0(invalids, collapse = ", "),
             " is not supported metrics for ",
             self$task_type, " task.", call. = FALSE)
      metrics
    },

    .assert_pred = function(pred, label_var) {
      if (!is.data.frame(pred))
        stop("pred must be a data.frame.", call. = FALSE)
      if (nrow(pred) == 0L)
        stop("pred must have at least one row.", call. = FALSE)
      if (!all(c(label_var, ".pred") %in% colnames(pred)))
        stop("pred must have ", .str_quote(label_var),
             " and \".pred\" columns.", call. = FALSE)
      pred
    },

    .build_metric_set = function() {
      metric_funs <- purrr::map(self$metrics, function(metric) {
        fun_str <- paste0("yardstick::", metric)
        fun_expr <- rlang::parse_expr(fun_str)
        eval(fun_expr)
      })
      call <- rlang::call2("metric_set", !!!metric_funs, .ns = "yardstick")
      rlang::eval_tidy(call, env = rlang::current_env())
    },

    .metrics = character(),
    .task_type = character(),
  )
)

#' Measure Class Constructor
#'
#' @rdname Measure
#'
#' @param task_type A character scalar of task type. See
#' [supported_tasks] for tasks supported by a model/engine.
#' @param metrics A character vector of model metrics. Metrics must be
#' metrics of [supported_metrics]. If the vector is named, the names will be
#' used insted of metrics' name itself. If `NULL`, default metrics will be
#' set based on a `task_type`.
#'
#' @return A new `Measure` class object
#'
#' @export
new_measure <- function(task_type = NULL, metrics = NULL) {
  Measure$new(task_type, metrics)
}
