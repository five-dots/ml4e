#' @title CV Class
#'
#' @description
#' A class defines a cross-validation task specification.
#'
#' @export
CV <- R6::R6Class(
  classname = "CV",
  inherit = Task,
  public = rlang::list2(
    #' @description
    #' Construct a new `CV` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys.
    #' @param fit_param A list of fit parameters. If `NULL`, `keys` must have
    #' "fit_param" key.
    #'
    #' @return A new `CV` class object
    initialize = function(options = NULL, keys = NULL, fit_param = NULL) {
      private$.fit_param <- .assert_fit_param(fit_param)
      super$initialize(options, keys)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      self$print_task(title = " CV ", key_width = key_width)
      .print_sep()
      if (self$done) {
        cv_score <- .str_list(self$cv_score)
        .print("CV score", cv_score, key_width, FALSE, FALSE)
        if (self$has_test_labels) {
          test_score <- self$test_score %>% .str_list()
          .print("Test score", test_score, key_width, FALSE, FALSE)
        }
        .print_sep()
        ## fit
        cat("Fit object:", "\n")
        fit <- self$result$fits %>%
          purrr::discard(~ inherits(., "condition")) %>%
          purrr::pluck(1L)
        print(fit)
        .print_sep()
        ## predictions
        cat("Predictions:", "\n")
        print(self$pred, n = 5L)
        .print_sep()
      }
      invisible(self)
    },

    #' @description
    #' Get a resample.
    #'
    #' @param seed A integer scalar seed.
    #'
    #' @return A resample of \code{rset} class object.
    get_resample = function(seed = NULL) {
      seed <- .assert_seed(seed)
      self$datasets$get_resample(self$keys["dataset"], self$keys["resample"],
                                 seed, self$preproc_call)
    },

    #' @description
    #' Convert data by `self$data_call`.
    #'
    #' @param data A `data.frame` or `rsplit` object to be converted.
    #'
    #' @return A converted data appropreate for the model input.
    convert_data = function(data) {
      data <- private$.assert_data(data)
      data_args <- list(object = data, formula = self$formula)
      rlang::eval_tidy(self$data_call, data_args)
    },

    #' @description
    #' Fit model by `self$fit_call`.
    #'
    #' @param data A `data.frame` or `rsplit` object.
    #'
    #' @return A model fitted object.
    fit = function(data = NULL) {
      ## suppress all cat message
      sink("/dev/null")
      on.exit(sink())

      eval_data <- self$convert_data(data)
      set.seed(self$model$seed)
      tryCatch({
        fitted <- rlang::eval_tidy(self$fit_call, eval_data)
        butcher::butcher(fitted)
      }, error = function(e) e)
    },

    #' @description
    #' Predict by a model.
    #'
    #' @param fitted A model fitted object.
    #' @param new_data A `data.frame` to be predicted.
    #' @param ids A integer vector of row indexes.
    #'
    #' @return A `data.frame` of predicted values.
    predict = function(fitted = NULL, new_data = NULL, ids = NULL) {
      if (inherits(fitted, "condition")) return(NULL)

      ## Prepare data for pred_call
      eval_data <- self$convert_data(new_data)
      eval_data$fitted <- fitted

      pred <- tryCatch({
        rlang::eval_tidy(self$pred_call, eval_data)
      }, error = function(e) e)
      if (inherits(pred, "condition")) return(pred)

      pred <- switch(
        self$task_type,
        "multiclass" = private$.to_prob_nd(pred),
        "binary"     = private$.to_prob_1d(pred),
        "regression" = private$.to_num_1d(pred),
        "poisson"    = private$.to_num_1d(pred),
        stop("Prediction convert function not found.", call. = FALSE)
      )

      ## test_ids
      if (is.null(ids)) {
        ids_df <- tibble::tibble(!!self$id_col := rep(NA_integer_,
                                                      nrow(new_data)))
      } else {
        ids_df <- tibble::tibble(!!self$id_col := ids)
      }

      if (!self$keep_data)
        new_data <- dplyr::select(new_data, self$label_var)
      dplyr::bind_cols(ids_df, new_data, pred)
    },

    #' @description
    #' Get a fitted object by all training data.
    get_fitted = function() {
      data <- self$train
      self$fit(data)
    },

    #' @description
    #' Run a cross-validation. The result will be stored in `self$result`.
    #' @param cv_seed A logical scalar whether to use `self$cv_seed`. If
    #' `FALSE`, then `self$search_seed` is used.
    do = function(cv_seed = TRUE) {
      if (!is.null(self$result)) return(invisible(self))

      ## Switch seed
      seed <- if (cv_seed) self$cv_seed else self$search_seed

      ## CV by resample
      resample <- self$get_resample(seed)
      if (self$options$parallel) {
        ## Run parallely
        prev_plan <- future::plan(future::multiprocess)
        on.exit(future::plan(prev_plan))

        resample <- suppressMessages(
          dplyr::mutate(resample, fits = furrr::future_map(splits, self$fit))
        )
      } else {
        ## Run sequentially
        resample <- dplyr::mutate(resample, fits = purrr::map(splits, self$fit))
      }

      ## Add predictions
      res <- resample %>%
        dplyr::mutate(preds = purrr::map2(splits, fits, function(rsplit, fit) {
          new_data <- rsample::assessment(rsplit)
          ids <- rsample::complement(rsplit)
          self$predict(fit, new_data, ids)
        }))

      ## Predictions for test data
      fits <- purrr::discard(res$fits, ~ inherits(., "condition"))
      if (length(fits) > 0L) {
        ## Ensemble prediction by all cv fits
        new_data <- self$test
        ids <- self$datasets$test_ids
        pred <- private$.predict_ensemble(fits, new_data, ids)
        private$.test_pred <- pred
      }

      ## Finalize
      if (!self$keep_data) res <- dplyr::select(res, -splits)

      ## Delete "forest" to reduce memory usage
      if (self$model$engine == "ranger")
        for (i in seq_along(res)) res$fits[[i]]$forest <- NULL
      private$.result <- res
      invisible(self)
    },
  ),
  active = rlang::list2(
    #' @field fit_param A list of fit parameters.
    fit_param = function() {
      if (is.null(private$.fit_param)) {
        self$model$fit_params[self$keys["fit_param"]]
      } else {
        private$.fit_param
      }
    },
    #' @field fit_call A fit `call`.
    fit_call = function() {
      call <- self$model$get_fit_call(self$fit_param)
      self$eval_call_args(call)
    },

    #' @field cv_pred A `data.frame` of cross-validation predictions.
    cv_pred = function() {
      private$.check_if_done()
      self$result$preds %>%
        purrr::discard(inherits, "condition") %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(!!rlang::sym(self$id_col))
    },
    #' @field test_pred A `data.frame` of test predictions.
    test_pred = function() {
      private$.check_if_done()
      private$.test_pred
    },
    #' @field pred A `data.frame` of all predictions.
    pred = function() {
      test_pred <- tryCatch(self$test_pred, error = function(e) NULL)
      dplyr::bind_rows(self$cv_pred, self$test_pred)
    },

    #' @field cv_score A list of cross-validation score.
    cv_score = function() {
      self$measure$do(self$cv_pred, self$label_var)
    },
    #' @field test_score A list of test score.
    test_score = function() {
      if (!self$has_test_labels)
        stop("could not calculate a test score as test data has no labels.",
             call. = FALSE)
      self$measure$do(self$test_pred, self$label_var)
    },

    #' @field error_count A list of error counts against folds
    error_count = function() {
      if (is.null(self$result)) {
        folds <- 0L
        error <- 0L
      } else {
        folds <- nrow(self$result)
        error <- sum(purrr::map2_lgl(self$result$fits, self$result$preds, ~ {
          inherits(.x, "condition") || inherits(.y, "condition")
        }))
      }
      list(error = error, folds = folds)
    },
    #' @field error_count_str A character of error counts.
    error_count_str = function() {
      count <- self$error_count
      paste0(count$error, "/", count$folds)
    },
    #' @field done A logical if finished
    done = function() {
      if (is.null(self$result)) return(FALSE)
      count <- self$error_count
      ## must have at least one prediction
      count$error != count$folds
    },
  ),
  private = rlang::list2(
    .assert_keys = function(keys) {
      super$.assert_keys(keys)
      ## fit_params
      if (is.null(private$.fit_param)) {
        if (is.na(keys["fit_param"]))
          stop("keys must have a key named \"fit_param\".", call. = FALSE)
        if (!self$models[keys["model"]]$fit_params$has(keys["fit_param"]))
          stop("fit_params does not have ", .str_quote(keys["fit_param"]),
               " key.", call. = FALSE)
      }
      keys
    },

    .assert_data = function(data) {
      if (is.null(data))
        stop("Data must not be NULL.", call. = FALSE)
      if (!(is.data.frame(data) || inherits(data, "rsplit")))
        stop("Data must not be a data.frame or rsplit.", call. = FALSE)
      data
    },

    .check_if_done = function() {
      count <- self$error_count
      if (count$folds > 0L && count$folds == count$error)
        stop("All calculations must be failed.", call. = FALSE)
      super$.check_if_done()
      invisible(NULL)
    },

    .to_num_1d = function(pred) {
      ## task: regression, poisson
      ## data: vector or 1col-matrix
      if (self$model$engine == "catboost" && self$task_type == "poisson")
        pred <- exp(pred)
      if (inherits(pred, "ranger.prediction"))
        pred <- pred$predictions
      if (is.matrix(pred))
        pred <- pred[, 1L]
      tibble::tibble(.pred = pred)
    },

    .to_prob_1d = function(pred) {
      ## task: binary
      ## data: vector, 1col-matrix or 2col-matrix (ranger, kernlab, extraTrees)
      label_levels <- self$label_levels
      if (inherits(pred, "ranger.prediction")) pred <- pred$predictions
      if (is.matrix(pred)) pred <- pred[, ncol(pred)]
      class <- factor(label_levels[as.integer(pred >= 0.5) + 1L],
                      levels = label_levels)
      col_name <- paste(".prob", label_levels[2L], sep = "_")
      tibble::tibble(.pred = class, !!col_name := pred)
    },

    .to_prob_nd = function(pred) {
      ## task: multiclass
      ## data: ncol-matrix
      label_levels <- self$label_levels
      if (inherits(pred, "ranger.prediction")) pred <- pred$predictions
      if (length(dim(pred)) == 3L) pred <- pred[, , 1L]
      class <- factor(label_levels[max.col(pred)], levels = label_levels)
      colnames(pred) <- paste0(".prob_", label_levels)
      dplyr::bind_cols(
        tibble::tibble(.pred = class),
        tibble::as_tibble(pred)
      )
    },

    .predict_ensemble = function(fits, new_data, ids) {
      ## Output predicts from all cv's fitted objects
      preds <- purrr::map(fits, self$predict, new_data, ids) %>%
        purrr::discard(inherits, "condition")
      if (length(preds) == 0L) return(NULL)

      if (!any(stringr::str_starts(names(preds[[1L]]), ".prob_"))) {
        ## Regression case
        num_preds <- purrr::map(preds, ".pred")
        avg_pred <- purrr::reduce(num_preds, `+`) / length(num_preds)
        new_pred <- tibble::tibble(.pred = avg_pred)

      } else {
        ## Classification case with .prob_* columns
        ## Extract probability columns only
        probs <- purrr::map(preds, function(pred) {
          dplyr::select_at(pred, dplyr::vars(dplyr::starts_with(".prob_")))
        })

        ## Take average
        avg_prob <- purrr::reduce(probs, `+`) / length(probs)

        ## Generate new predictions
        if (ncol(avg_prob) == 1L) {
          new_pred <- private$.to_prob_1d(avg_prob[, 1L])
        } else {
          new_pred <- private$.to_prob_nd(avg_prob)
        }
      }

      ## combine data and new predictions
      id_col <- tibble::tibble(!!self$id_col := ids)
      if (!self$keep_data)
        new_data <- dplyr::select(new_data, self$label_var)
      dplyr::bind_cols(id_col, new_data, new_pred)
    },

    .fit_param = NULL,
    .test_pred = NULL,
    .pred = NULL,
  )
)

#' CV Class Constructor
#'
#' @rdname CV
#'
#' @param options \code{\link{Options}} class object.
#' @param keys A named character vector of keys.
#' @param fit_param A list of fit parameters.
#'
#' @return A new \code{CV} class object
#'
#' @export
new_cv <- function(options = NULL, keys = NULL, fit_param = NULL) {
  CV$new(options, keys, fit_param)
}
