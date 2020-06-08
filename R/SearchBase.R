#' @title SearchBase Class
#'
#' @description
#' A base class describe a hyper parameter search task.
#'
#' @export
SearchBase <- R6::R6Class(
  classname = "SearchBase",
  inherit = Task,
  public = rlang::list2(
    #' @description
    #' Construct a new `SearchBase` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys.
    #'
    #' @return A new `SearchBase` class object
    initialize = function(options = NULL, keys = NULL) {
      super$initialize(options, keys)

      ## Keep a list of default FitParamSpec values
      private$.set_fit_param_specs_defaults()
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      self$print_task(title = " SearchBase ", key_width = key_width)
      .print_sep()
    },

    #' @description
    #' Get parameters ranking tables.
    #'
    #' @param metric_name A character scalar of metric name. If `NULL`, the
    #' first metric_name in `self$options$measure$metric_names` is used.
    #' @param n A integer scalar of a number of the ranking.
    #'
    #' @return A list of parameters ranking by keys
    get_rank = function(metric_name = NULL, n = 5L) {
      private$.check_if_no_result()
      metric_name <- self$measure$assert_metric_name(metric_name)
      minimize <- self$measure$is_minimize_metric(metric_name)
      .assert_count(n)
      .sort_by_metric(self$result, metric_name, minimize, n)
    },

    #' @description
    #' Get best parameters.
    #'
    #' @param metric_name A character scalar of metric name. If `NULL`, the
    #' first metric_name in `self$options$measure$metric_names` is used.
    #'
    #' @return A list of parameters.
    get_best_param = function(metric_name = NULL) {
      best <- self$get_rank(metric_name, n = 1L)
      param_names <- colnames(self$model$fit_param_specs$get_grid())
      best %>%
        dplyr::select(param_names) %>%
        purrr::transpose() %>%
        purrr::pluck(1L)
    },
  ),
  active = rlang::list2(
    #' @field result A task result.
    result = function() {
      if (is.null(private$.result)) return(NULL)
      ## subset by the current grid
      private$.align_result()
    },
  ),
  private = rlang::list2(
    .set_fit_param_specs_defaults = function() {
      ## Combine list
      defaults <- c(private$.fit_param_specs_defaults,
                    self$model$fit_param_specs$defaults)

      ## keep unique names only
      par_names <- unique(names(defaults))
      res <- purrr::map(par_names, function(par_name) {
        defaults[names(defaults) == par_name][1L]
      }) %>% purrr::flatten()
      private$.fit_param_specs_defaults <- res
    },

    .cv_by_fit_param = function(rowid, ...) {
      fit_param <- list(...)
      fit_param_cols <- purrr::imap_dfc(fit_param, function(value, name) {
        ## Keep list column as is
        if (is.list(value)) value <- list(value)
        tibble::tibble(!!name := value)
      })

      ## Disable options flag
      options <- self$options$clone()
      options$parallel <- FALSE

      ## CV
      cv <- new_cv(options, self$keys, fit_param)
      cv$do(cv_seed = FALSE) # use search_seed

      if (cv$done) {
        score_cols <- tibble::as_tibble(cv$cv_score)
      } else {
        ## If all cv folds fail, set score columns NA
        score_cols <- tibble::tibble(metric = self$measure$metric_names,
                                     value = NA_real_) %>%
          tidyr::pivot_wider(names_from = metric, values_from = value)
      }
      row <- dplyr::bind_cols(fit_param_cols, score_cols)

      ## update progress bar, if enabled
      if (!is.null(private$.progress_bar) &&
          self$show_progress &&
          !self$parallel)
        utils::setTxtProgressBar(private$.progress_bar, rowid)
      row
    },

    .check_if_no_result = function() {
      if (is.null(self$result) || nrow(self$result) == 0L)
        stop("No parameter search result found. first execute self$do().",
             call. = FALSE)
    },

    .align_result = function() {
      res <- private$.result
      grid <- self$model$fit_param_specs$get_grid()

      ## Add params not in the result
      res_names <- colnames(res) %>%
        purrr::discard(~ . %in% self$measure$metric_names)
      grid_names <- colnames(grid)
      add_names <- setdiff(grid_names, res_names)
      if (length(add_names) > 0L) {
        specs <- purrr::map(add_names, ~ self$model$fit_param_specs[.])
        for (spec in specs) {
          res <- dplyr::mutate(res, !!spec$name := spec$default)
        }
        res <- dplyr::select_at(res, dplyr::vars(grid_names,
                                                 dplyr::everything()))
      }

      ## update private result before reducing
      private$.result <- res

      ## delete params not in the grid
      del_names <- setdiff(res_names, grid_names)
      if (length(del_names) > 0L) {
        all_defaults <- private$.fit_param_specs_defaults
        defaults <- all_defaults[names(all_defaults) %in% del_names]
        for (i in seq_along(defaults)) {
          sym <- rlang::sym(names(defaults)[i])
          res <- res %>%
            dplyr::filter(!!sym == defaults[[i]]) %>%
            dplyr::select(-!!sym)
        }
      }
      if (nrow(res) == 0L) NULL else res
    },

    .progress_bar = NULL,
    .fit_param_specs_defaults = list(),
  )
)
