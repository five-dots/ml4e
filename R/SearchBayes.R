#' @title SearchBayes Class
#'
#' @description
#' A class defines a bayesian search task specification.
#'
#' @export
SearchBayes <- R6::R6Class(
  classname = "SearchBayes",
  inherit = Task,
  public = rlang::list2(
    #' @description
    #' Construct a new `SearchBayes` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys.
    #' @param metric_name A character scalar of metric name. If NULL, the first
    #' metric_name in `self$options$measure$metric_names` is used.
    #'
    #' @return A new `SearchBayes` class object
    initialize = function(options = NULL, keys = NULL, metric_name = NULL) {
      super$initialize(options, keys)
      private$.metric_name <- options$measure$assert_metric_name(metric_name)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      self$print_task(title = " Bayes Search ", key_width = key_width)
      .print_sep()

      ## Result
      if (!is.null(self$result)) {
        cat("Bayes search result (metric = ",
            .str_quote(self$metric_name), "): ", "\n", sep = "")
        print(self$result)
        .print_sep()
      }
    },

    #' @description
    #' Run a bayesian optimization. The result will be stored in
    #' `self$result`.
    #'
    #' @param init_grid_dt A data.frame of `init_grid_dt`.
    #' @param init_points A integer scalar of `init_points`.
    #' @param n_iter A integer scalar of `n_iter`.
    #' @param acq A character scalar of `acq` (aquisition function).
    #' Can be "ucb", "ei" or "poi".
    #' @param kappa A numeric scalar of `kappa`.
    #' @param eps A numeric scalar of `eps`.
    #' @param kernel A list of `kernal` parameters.
    #'
    #' @return A `data.frame` of best parameters.
    do = function(init_grid_dt = NULL, init_points = 4L, n_iter = 5L,
                  acq = "ucb", kappa = 2.576, eps = 0.0,
                  kernel = list(type = "exponential", power = 2L)) {

      if (self$done) return(invisible(self))

      if (!rlang::is_installed("rBayesianOptimization"))
        stop("rBayesianOptimization package is not installed.", call. = FALSE)

      ## Suppress cat message
      if (!self$options$show_progress) {
        sink("/dev/null")
        on.exit(sink())
      }

      ## Message/Timer
      if (self$show_progress) {
        tictoc::tic()
        cat(.str_keys(self$keys), "\n")
      }

      ## Param info
      param_info <- self$model$fit_param_specs$get_bayes_info()
      inits <- param_info$fixed
      bounds <- param_info$bounds
      minimize <- self$measure$is_minimize_metric(self$metric_name)
      init_grid_dt <- private$.filter_init_grid_dt(init_grid_dt, inits, bounds,
                                                   minimize)
      ## Maximization fun (cv score)
      obj_fun <- function(...) {
        fit_param <- c(list(...), inits)
        cv <- new_cv(self$options, self$keys, fit_param)
        cv$do(cv_seed = FALSE) # use search_seed
        score <- if (cv$done) cv$cv_score[[self$metric_name]] else NA_real_
        ## pred <- cv$cv_pred$.pred
        if (minimize) score <- -score
        list(Score = score, Pred = 0.0)
      }

      ## Build bayes args
      args <- list(FUN = obj_fun, bounds = bounds, init_grid_dt = init_grid_dt,
                   init_points = init_points, n_iter = n_iter, acq = acq,
                   kappa = kappa, eps = eps, kernel = kernel)

      ## Run bayes optim
      res <- NULL
      error_count <- 0L
      max_error_count <- 1L
      while (TRUE) {
        bayes_res <- tryCatch({
          fun <- getExportedValue("rBayesianOptimization",
                                  "BayesianOptimization")
          do.call(fun, args)
        }, error = function(e) {
          message(e)
          e
        })
        ## If no error, break loop
        if (!inherits(bayes_res, "condition")) break
        ## error case
        error_count <- error_count + 1L
        if (error_count == max_error_count) {
          private$.result <- bayes_res
          private$.prev_grid <- self$model$fit_param_specs$get_grid()
          return(invisible(self))
        }
      }

      ## Best_params
      inits_df <- tibble::as_tibble(inits)
      res <- tibble::as_tibble(as.list(bayes_res$Best_Par)) %>%
        dplyr::bind_cols(inits_df) %>%
        dplyr::mutate(Value = bayes_res$Best_Value) %>%
        dplyr::mutate(Value = ifelse(minimize, -Value, Value)) %>%
        dplyr::rename(!!self$metric_name := Value)

      if (self$show_progress) tictoc::toc()
      private$.result <- res
      private$.prev_grid <- self$model$fit_param_specs$get_grid()
      invisible(self)
    },
  ),
  active = rlang::list2(
    #' @field metric_name A metric name.
    metric_name = function() private$.metric_name,
    #' @field is_new_grid A logical wheather grid is new.
    is_new_grid = function() {
      ## NULL means first run
      if (is.null(private$.prev_grid)) return(TRUE)
      !dplyr::setequal(private$.prev_grid,
                       self$model$fit_param_specs$get_grid())
    },
    #' @field done A logical if finished
    done = function() {
      if (is.null(self$result)) return(FALSE)
      if (self$is_new_grid) return(FALSE)
      !inherits(self$result, "condition")
    },
    #' @field error A logical if error
    error = function() {
      if (is.null(self$result)) return(FALSE)
      inherits(self$result, "condition")
    },
    #' @field result A task result.
    result = function() {
      if (self$is_new_grid) return(NULL)
      private$.result
    },
  ),
  private = rlang::list2(
    .filter_init_grid_dt = function(init_grid_dt, inits, bounds, minimize) {
      if (is.null(init_grid_dt) ||
          !inherits(init_grid_dt, "data.frame") ||
          nrow(init_grid_dt) == 0L)
        return(NULL)
      ## filter expressions by inits
      inits_exprs <- purrr::imap(inits, function(value, param) {
        param <- rlang::sym(param)
        if (is.double(value)) {
          rlang::call2("near", param, value, .ns = "dplyr")
        } else {
          rlang::call2("==", param, value, .ns = "base")
        }
      })
      ## filter expressions by bounds
      bounds_exprs <- purrr::imap(bounds, function(value, param) {
        param <- rlang::sym(param)
        rlang::call2("between", param, value[1L], value[2L], .ns = "dplyr")
      })
      exprs <- c(inits_exprs, bounds_exprs)
      names(exprs) <- NULL
      res <- init_grid_dt %>%
        dplyr::filter(!!!exprs) %>%
        tidyr::drop_na(self$metric_name)
      if (nrow(res) == 0L) return(NULL)
      res %>%
        dplyr::select(c(names(bounds), self$metric_name)) %>%
        ## init_grid_dt metric must be named as "Value"
        dplyr::rename(Value = !!self$metric_name) %>%
        dplyr::mutate(Value = ifelse(minimize, -Value, Value))
    },

    .metric_name = character(),
    .prev_grid = NULL,
  )
)

#' SearchBayes Class Constructor
#'
#' @rdname SearchBayes
#'
#' @param options A [Options] class object.
#' @param keys A named character vector of keys.
#' @param metric_name A character scalar of metric_name.
#'
#' @return A new `SearchBayes` class object
#'
#' @export
new_search_bayes <- function(options = NULL, keys = NULL, metric_name = NULL) {
  SearchBayes$new(options, keys, metric_name)
}
