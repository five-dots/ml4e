#' @title Project Class
#'
#' @description
#' A class defines a project specification.
#'
#' @export
Project <- R6::R6Class(
  classname = "Project",
  inherit = Options,
  public = rlang::list2(
    #' @description
    #' Construct a new `Project` class object.
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
    #' @return A new `Project` class object
    initialize = function(formulas = NULL, datasets = NULL, seeds = NULL,
                          models = NULL, measure = NULL, show_progress = TRUE,
                          keep_data = FALSE, parallel = FALSE) {
      super$initialize(formulas, datasets, seeds, models, measure,
                       show_progress, keep_data, parallel)
    },

    #' @description
    #' Print object.
    #'
    #' @return A `Project` object by `invisible(self)`.
    print = function() {
      self$print_options(title = " Project ")
      invisible(self)
    },

    #' @description
    #' Run cross-validation by each key combinations.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #'
    #' @return A `Project` object by `invisible(self)`.
    run_cv = function(...) {
      tab <- self$cv_table
      rows <- tab %>% .filter_or_slice(...) %>% dplyr::filter(!done)
      idx <- .get_matched_index(tab, rows)
      if (length(idx) == 0L) return(invisible(self))

      if (self$show_progress)
        pb <- utils::txtProgressBar(min = 0L, max = length(idx), style = 3L)

      for (i in idx) {
        tab[i, ]$task[[1L]]$do(cv_seed = TRUE)
        if (self$show_progress) utils::setTxtProgressBar(pb, which(idx == i))
      }

      if (self$show_progress && length(idx) > 0L) cat("\n") # break pb
      private$.cv_table <- dplyr::select(tab, -done, -error)
      invisible(self)
    },

    #' @description
    #' Run grid search for parameters set by each key combinations.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param num A integer scalar of how many parameters to be calculated.
    #' If `NULL`, all parameters in param set will be calculated.
    #'
    #' @return A `Project` object by `invisible(self)`.
    run_grid = function(..., num = NULL) {
      .assert_count(num, allow_null = TRUE)

      tab <- self$grid_table
      rows <- tab %>% .filter_or_slice(...) %>% dplyr::filter(!done)
      idx <- .get_matched_index(tab, rows)
      if (length(idx) == 0L) return(invisible(self))

      for (i in idx) {
        tab[i, ]$task[[1L]]$do(num)
        if (self$show_progress && !self$parallel) cat("\n")
      }

      private$.grid_table <- dplyr::select(tab, -done, -progress)
      invisible(self)
    },

    #' @description
    #' Run random search for parameters set by each key combinations.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param num A integer scalar of how many parameters to be calculated.
    #'
    #' @return A `Project` object by `invisible(self)`.
    run_random = function(..., num = NULL) {
      .assert_count(num)

      tab <- self$random_table
      rows <- .filter_or_slice(tab, ...)
      idx <- .get_matched_index(tab, rows)
      if (length(idx) == 0L) return(invisible(self))

      for (i in idx) {
        tab[i, ]$task[[1L]]$do(num)
        if (self$show_progress && !self$parallel) cat("\n")
      }

      private$.random_table <- dplyr::select(tab, -progress)
      invisible(self)
    },

    #' @description
    #' Run bayes search for parameters set by each key combinations.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param metric_name A character scalar of metric name. If `NULL`, the
    #' first metric_name in `self$measure$metric_names` is used.
    #' @param init_points A integer scalar of `init_points`.
    #' @param n_iter A integer scalar of `n_iter`.
    #' @param acq A character scalar of aquisition function. Can be "ucb", "ei"
    #' or "poi".
    #' @param kappa A numeric scalar of `kappa`.
    #' @param eps A numeric scalar of `eps`.
    #' @param kernel A list of kernal parameters.
    #'
    #' @return A `Project` object by `invisible(self)`.
    run_bayes = function(..., metric_name = NULL, init_points = 4L, n_iter = 5L,
                         acq = "ucb", kappa = 2.576, eps = 0.0,
                         kernel = list(type = "exponential", power = 2L)) {
      metric_name <- self$measure$assert_metric_name(metric_name)

      tab <- self$bayes_tables[[metric_name]]
      rows <- tab %>% .filter_or_slice(...) %>% dplyr::filter(!done)
      idx <- .get_matched_index(tab, rows)
      if (length(idx) == 0L) return(invisible(self))

      for (i in idx) {
        task <- tab[i, ]$task[[1L]]
        ## use search result of the same keys as init_grid dt
        exprs <- private$.get_exprs_from_keys(task$keys)
        init_grid_dt <- tryCatch({
          self$get_search_result(!!!exprs) %>%
            dplyr::pull(result) %>% purrr::pluck(1L)
        }, error = function(e) NULL)

        ## init_grid_dt <- self$get_search_result(!!!exprs) %>%
        ##   dplyr::pull(result) %>% purrr::pluck(1)
        task$do(init_grid_dt, init_points, n_iter, acq, kappa, eps, kernel)
        if (self$show_progress) cat("\n")
      }

      ## save as a list element
      tab <- dplyr::select(tab, -done, -error)
      if (is.list(private$.bayes_tables)) {
        private$.bayes_tables[[metric_name]] <- tab
      } else {
        private$.bayes_tables <- rlang::list2(!!metric_name := tab)
      }
      invisible(self)
    },

    #' @description
    #' Get cross-validation scores.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param simplify A logical scalar wheater to drop columns of a signle key.
    #'
    #' @return A `data.frame` class object.
    get_scores = function(..., simplify = TRUE) {
      rows <- self$cv_table %>% .filter_or_slice(...) %>% dplyr::filter(done)
      if (nrow(rows) == 0L) return(NULL)

      scores <- purrr::map_dfr(rows$task, function(t) {
        if (t$has_test_labels) {
          test_score <- tibble::as_tibble(t$test_score) %>%
            dplyr::rename_all(~ paste0("test_", .))
          cv_score <- tibble::as_tibble(t$cv_score) %>%
            dplyr::rename_all(~ paste0("cv_", .))
          dplyr::bind_cols(cv_score, test_score)
        } else {
          tibble::as_tibble(t$cv_score)
        }
      })

      keys <- dplyr::select(rows, -task, -done)
      if (simplify) keys <- private$.simplify_table(keys)
      dplyr::bind_cols(keys, scores)
    },

    #' @description
    #' Get predictions.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #'
    #' @return A list of predictions by keys
    get_preds = function(...) {
      rows <- self$cv_table %>% .filter_or_slice(...) %>% dplyr::filter(done)
      if (nrow(rows) == 0L)
        stop("no cv result matched by the conditions.", call. = FALSE)
      preds <- purrr::map(rows$task, "pred")
      names(preds) <- purrr::map_chr(rows$task, ~ paste(.$keys, collapse = "_"))
      preds
    },

    #' @description
    #' Get stacking data.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param prob A logical scalar wheather to use probability data.
    #'
    #' @return A `data.frame` of stacking data.
    get_stacking_data = function(..., prob = FALSE) {
      preds <- self$get_preds(...)
      if (prob && !any(stringr::str_starts(colnames(preds[[1L]]), ".prob_")))
        stop("predictions must contains columns starts with \".prob_\",",
             " if prob = TRUE.", call. = FALSE)
      id_col <- self$datasets$id_col
      if (is.null(id_col)) id_col <- ".id"
      if (!prob) {
        preds <- purrr::imap(preds, function(pred, name) {
          pred %>%
            dplyr::rename(!!name := .pred) %>%
            dplyr::select(!!id_col, !!self$label_var, !!name)
        })
      } else {
        preds <- purrr::imap(preds, function(pred, name) {
          pred %>%
            dplyr::rename_if(startsWith(names(.), ".prob_"),
                             ~ paste0(., "_", name)) %>%
            dplyr::select_at(dplyr::vars(id_col, self$label_var,
                                         dplyr::starts_with(".prob_")))
        })
      }
      purrr::reduce(preds, dplyr::full_join,
                    by = c(id_col, self$label_var))
    },

    #' @description
    #' Get merged search result.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #'
    #' @return A `data.frame` of search result.
    get_search_result = function(...) {
      ## aggregate grid/random/bayes search result
      ## grid result
      tab_g <- self$grid_table %>%
        dplyr::mutate(g = purrr::map(task, function(t) {
          if (is.null(t$result)) return(NULL)
          t$result %>%
            dplyr::mutate(method = "grid") %>%
            dplyr::select(method, dplyr::everything())
        })) %>%
        dplyr::select(-task, -done, -progress)

      ## random result
      tab_r <- self$random_table %>%
        dplyr::mutate(r = purrr::map(task, function(t) {
          if (is.null(t$result)) return(NULL)
          t$result %>%
            dplyr::mutate(method = "random") %>%
            dplyr::select(method, dplyr::everything())
        })) %>%
        dplyr::select(-task, -progress)

      ## bayes result
      tab_b <- private$.merge_bayes_table() %>%
        dplyr::rename(b = result)

      ## join by keys
      tabs <- list(tab_g, tab_r, tab_b)
      col_names <- purrr::map(tabs, colnames)
      keys <- purrr::reduce(col_names, intersect)
      tab <- purrr::reduce(tabs, dplyr::full_join, by = keys) %>%
        ## bind into one data.frame
        dplyr::mutate(result = purrr::pmap(., function(...) {
          res <- list(...) %>%
            purrr::keep(inherits, "data.frame") %>%
            dplyr::bind_rows()
          if (nrow(res) != 0L) res else NULL
        })) %>%
        dplyr::filter(purrr::map_lgl(result, ~ !is.null(.))) %>%
        dplyr::select(keys, result)
      if (nrow(tab) == 0L) stop("no search result found.", call. = FALSE)

      ## filter rows
      rows <- .filter_or_slice(tab, ...)
      if (nrow(rows) == 0L)
        stop("no search result matched by the conditions.", call. = FALSE)
      rows
    },

    #' @description
    #' Get merged search result by model key.
    #'
    #' @param model_key A character scalar of model key.
    #'
    #' @return A `data.frame` of search result.
    get_search_result_by_model = function(model_key = NULL) {
      .assert_str(model_key)
      self$get_search_result() %>%
        dplyr::filter(model == !!model_key) %>%
        dplyr::select(-model) %>%
        tidyr::unnest(result)
    },

    #' @description
    #' Get paramter ranking.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param metric_name A character scalar of metric name. If `NULL`, the
    #' first  metric_name in `self$measure$metric_names` is used.
    #' @param n A integer scalar of a number of the ranking.
    #'
    #' @return A list of ranking by keys
    get_ranks = function(..., metric_name = NULL, n = 5L) {
      ## assertion
      metric_name <- self$measure$assert_metric_name(metric_name)
      minimize <- self$measure$is_minimize_metric(metric_name)
      n <- .assert_count(n)
      ## rank by search result
      tab <- self$get_search_result(...)
      ranks <- purrr::map(tab$result, function(res) {
        .sort_by_metric(res, metric_name, minimize, n)
      })
      names(ranks) <- purrr::pmap_chr(tab, function(...) {
        list(...) %>% purrr::keep(is.character) %>% paste(collapse = "_")
      })
      ranks
    },

    #' @description
    #' Print parameters ranking.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param metric_name A character scalar of metric name. If NULL, the first
    #' metric_name in `self$measure$metric_names` is used.
    #' @param n A integer scalar of a number of the ranking.
    #'
    #' @return A `Project` object by `invisible(self)`.
    print_ranks = function(..., metric_name = NULL, n = 5L) {
      rank <- self$get_ranks(..., metric_name = metric_name, n = n)
      purrr::iwalk(rank, function(rank, name) {
        .print_sep()
        cat(.str_quote(name), "\n\n")
        print(as.data.frame(rank))
      })
      .print_sep()
      invisible(self)
    },

    #' @description
    #' Get best parameters by grid/bayes search.
    #'
    #' @param ... Filtering expressions passed to [dplyr::filter] or
    #' [dplyr::slice].
    #' @param metric_name A character scalar of metric name. If `NULL`, the
    #' first metric_name in `self$measure$metric_names` is used.
    #'
    #' @return A list of best parameters by keys
    get_best_params = function(..., metric_name = NULL) {
      bests <- self$get_ranks(..., metric_name = metric_name, n = 1L)
      del_cols <- c("rank", "method", self$measure$metric_names)
      purrr::map(bests, function(best) {
        best %>%
          dplyr::select(-dplyr::one_of(del_cols)) %>%
          purrr::transpose() %>%
          purrr::pluck(1L)
      })
    },
  ),
  active = rlang::list2(
    #' @field cv_table A cross-validation table.
    cv_table = function() {
      private$.get_table(".cv_table") %>%
        dplyr::mutate(done = purrr::map_lgl(task, "done")) %>%
        dplyr::mutate(error = purrr::map_chr(task, "error_count_str"))
    },
    #' @field grid_table A grid search table.
    grid_table = function() {
      private$.get_table(".grid_table") %>%
        dplyr::mutate(done = purrr::map_lgl(task, "done")) %>%
        dplyr::mutate(progress = purrr::map_chr(task, "progress_str"))
    },
    #' @field random_table A random search table.
    random_table = function() {
      private$.get_table(".random_table") %>%
        dplyr::mutate(progress = purrr::map_chr(task, "progress_str"))
    },
    #' @field bayes_tables A list of bayesian search tables by `metric_name`.
    bayes_tables = function() {
      metric_names <- self$measure$metric_names
      tables <- purrr::map(metric_names, function(metric_name) {
        private$.get_table(".bayes_tables", metric_name) %>%
          dplyr::mutate(done = purrr::map_lgl(task, "done")) %>%
          dplyr::mutate(error = purrr::map_chr(task, "error"))
      })
      names(tables) <- metric_names
      tables
    },
  ),
  private = rlang::list2(
    .get_table = function(table_name = NULL, metric_name = NULL) {

      ## Table with all keys combinations
      tab <- tidyr::crossing(
        formula = self$formulas$keys,
        dataset = self$datasets$keys,
        resample = self$datasets$resample_calls$keys,
        seed = self$seeds$keys,
        model = self$models$keys
      ) %>%
        ## Model keys in list column
        dplyr::mutate(model_keys = purrr::map(model, function(model_key) {
          tidyr::crossing(
            preproc = self$models[model_key]$preproc_calls$keys,
            fit_param = self$models[model_key]$fit_params$keys,
          )
        })) %>%
        tidyr::unnest(model_keys)

      ## Grid/Random/Bayes do not need "fit_param"
      if (table_name %in% c(".grid_table", ".random_table", ".bayes_tables"))
        tab <- tab %>% dplyr::select(-fit_param) %>% dplyr::distinct()

      ## Join with current table's task
      cur_tabs <- get(table_name, envir = private)
      if (!is.null(metric_name)) {
        cur_tab <- cur_tabs[[metric_name]]
      } else {
        cur_tab <- cur_tabs
      }
      if (!is.null(cur_tab)) {
        keys <- private$.keys
        if (table_name == ".cv_table") keys <- c(keys, "fit_param")
        tab <- dplyr::left_join(tab, cur_tab, by = keys)
      }

      ## Add empty list column for tasks
      if (!rlang::has_name(tab, "task"))
        tab <- dplyr::mutate(tab, task = list(NULL))

      ## Add task
      task_class <- switch(
        table_name,
        ".cv_table" = "CV",
        ".grid_table" = "SearchGrid",
        ".random_table" = "SearchRandom",
        ".bayes_tables" = "SearchBayes",
        stop(.str_quote(table_name), " is not a supported table_name.",
             call. = FALSE)
      )

      res <- tab %>%
        dplyr::mutate(task = purrr::pmap(., function(task, ...) {
          if (inherits(task, task_class)) return(task)
          keys <- unlist(list(...)) # Named character vector keys
          switch(
            task_class,
            "CV"           = new_cv(self, keys),
            "SearchGrid"   = new_search_grid(self, keys),
            "SearchRandom" = new_search_random(self, keys),
            "SearchBayes"  = new_search_bayes(self, keys, metric_name)
          )
        }))

      ## Update private field
      if (!is.null(metric_name)) {
        ## Update as list if bayes_tables
        cur_tabs[[metric_name]] <- res
        assign(table_name, cur_tabs, envir = private)
      } else {
        assign(table_name, res, envir = private)
      }
      res
    },

    .get_exprs_from_keys = function(keys) {
      exprs <- purrr::imap(keys, function(value, param) {
        param <- rlang::sym(param)
        rlang::call2("==", param, value)
      })
      names(exprs) <- NULL
      exprs
    },

    .simplify_table = function(table) {
      ## remove column if it has only one key
      removed_cols <- purrr::map(colnames(table), function(key) {
        val <- dplyr::pull(table, !!key)
        if (length(unique(val)) == 1L) key else NULL
      }) %>%
        purrr::compact() %>%
        unlist()
      dplyr::select(table, -dplyr::one_of(removed_cols))
    },

    .merge_bayes_table = function() {
      ## Aggregate all metrics result
      tabs <- purrr::imap(self$bayes_tables, function(row, name) {
        row %>% dplyr::rename(!!name := task) %>% dplyr::select(-done, -error)
      })
      col_names <- purrr::map(tabs, colnames)
      by <- purrr::reduce(col_names, intersect)
      purrr::reduce(tabs, dplyr::full_join, by = by) %>%
        dplyr::mutate(result = purrr::pmap(., function(...) {
          ## bind task result (except error)
          res <- list(...) %>%
            purrr::keep(inherits, "BayesSearch") %>%
            purrr::map("result") %>%
            purrr::discard(inherits, "condition") %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(method = "bayes") %>%
            dplyr::select(method, dplyr::everything())
          if (nrow(res) != 0L) res else NULL
        })) %>%
        dplyr::select(by, result)
    },

    .keys = c("formula", "dataset", "resample", "seed", "model", "preproc"),
    .cv_table = NULL,
    .grid_table = NULL,
    .random_table = NULL,
    .bayes_tables = NULL,
  )
)

#' Project Class Constructor
#'
#' @rdname Project
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
#' @return A new `Project` class object
#'
#' @export
new_project <- function(formulas = NULL, datasets = NULL, seeds = NULL,
                        models = NULL, measure = NULL, show_progress = TRUE,
                        keep_data = FALSE, parallel = FALSE) {
  Project$new(formulas, datasets, seeds, models, measure, show_progress,
               keep_data, parallel)
}
