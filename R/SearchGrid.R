#' @title SearchGrid Class
#'
#' @description
#' A class defines a grid search task specification.
#'
#' @export
SearchGrid <- R6::R6Class(
  classname = "SearchGrid",
  inherit = SearchBase,
  public = rlang::list2(
    #' @description
    #' Construct a new `SearchGrid` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys.
    #'
    #' @return A new `SearchGrid` class object
    initialize = function(options = NULL, keys = NULL) {
      super$initialize(options, keys)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      self$print_task(title = " Grid Search ", key_width = key_width)
      .print_sep()

      ## Result
      if (!is.null(self$result)) {
        cat("Grid search result ", "[", self$progress_str, "]:", "\n",
            sep = "")
        print(self$result, n = 5L)
        .print_sep()
      }
    },

    #' @description
    #' Run a grid search. The result will be stored in `self$result`.
    #'
    #' @param num A integer scalar of how many parameters to be calculated.
    #' If `NULL`, all parameters in param set will be calculated.
    do = function(num = NULL) {
      if (self$done) return(invisible(self))

      ## Assertion
      if (!self$model$has_fit_param_specs)
        stop("fit_param_specs is not set.", call. = FALSE)
      if (!is.null(num)) .assert_count(num) # NULL or positive integer

      ## Grid
      private$.set_fit_param_specs_defaults()
      grid <- private$.align_grid()
      if (nrow(grid) == 0L) {
        message("all grid already calulated.")
        return(invisible(self))
      }
      if (!is.null(num))
        grid <- dplyr::sample_n(grid, min(num, nrow(grid)))
      grid <- tibble::rowid_to_column(grid) # add rowid to use with progress bar

      ## Message/Timer
      if (self$show_progress) {
        tictoc::tic()
        cat(.str_keys(self$keys), "\n")
      }

      ## Progress bar
      ## This pb is updated on super$.cv_by_fit_param()
      ## If parallel = TRUE, purrr's pb is used (not uitils' pb)
      if (self$show_progress && !self$parallel)
        private$.progress_bar <- utils::txtProgressBar(0L, nrow(grid),
                                                       style = 3L)

      ## Generate grid tasks by fit_param
      if (self$parallel) {
        prev_plan <- future::plan(future::multiprocess)
        on.exit(future::plan(prev_plan))
        ## Suppress furrr's message
        res <- suppressMessages(
          furrr::future_pmap_dfr(
            grid, private$.cv_by_fit_param,
            .progress = self$show_progress && self$parallel)
        )
      } else {
        res <- purrr::pmap_dfr(grid, private$.cv_by_fit_param)
      }

      ## Message/Timer
      if (self$show_progress && !self$parallel) cat("\n") # Break pb
      if (self$show_progress) tictoc::toc()

      ## Update private result
      if (!is.null(private$.result)) super$.align_result()
      private$.result <- dplyr::bind_rows(private$.result, res)
      invisible(self)
    },

    #' @description
    #' Get paramter's counts.
    #' @return A `data.frame` of parameter count.
    get_count_by_param = function() {
      private$.check_if_no_result()
      ## extract parameter columns only
      param_cols <- self$result[, self$model$fit_param_specs$keys]
      param_list <- as.list(param_cols)
      ## count by param names
      purrr::imap_dfr(param_list, function(value, name) {
        counts <- value %>%
          table() %>%
          as.data.frame() %>%
          rlang::set_names(c("value", "count"))
        tibble::tibble(param_name = name, count = list(counts))
      })
    },
  ),
  active = rlang::list2(
    #' @field progress A list of grid calculation progress.
    progress = function() {
      if (self$model$has_fit_param_specs) {
        grid <- self$model$fit_param_specs$get_grid()
        tobe <- nrow(grid)
      } else {
        tobe <- 0L
      }
      done <- if (!is.null(self$result)) nrow(self$result) else 0L
      total <- if (!is.null(private$.result)) nrow(private$.result) else 0L
      list(done = done, tobe = tobe, total = total)
    },
    #' @field progress_str A character of progress.
    progress_str = function() {
      prg <- self$progress
      paste0(prg$done, "/", prg$tobe, " (ttl: ", prg$total, ")")
    },
    #' @field done A logical if finished
    done = function() {
      prg <- self$progress
      prg$done == prg$tobe
    },
  ),
  private = rlang::list2(
    .setdiff = function(grid, result, drop_grid = TRUE) {
      ## combine two df
      all <- dplyr::bind_rows(
        dplyr::mutate(grid, src = "grid"),
        dplyr::mutate(result, src = "result")
      ) %>%
        dplyr::mutate_if(is.character, as.factor)
      ## save factor levels
      levels <- purrr::imap(all, ~ if (is.factor(.x)) levels(.x) else NULL) %>%
        purrr::compact()
      levels$src <- NULL
      ## extract data and convert to integer for fuzzy join
      grid <- all %>%
        dplyr::filter(src == "grid") %>%
        dplyr::mutate_if(is.factor, as.integer) %>%
        dplyr::select(colnames(grid))
      result <- all  %>%
        dplyr::filter(src == "result") %>%
        dplyr::select(-src) %>%
        dplyr::mutate_if(is.factor, as.integer)
      ## set operation
      by <- colnames(grid)
      if (drop_grid) {
        ## drop grid mathced by result
        res <- fuzzyjoin::difference_anti_join(grid, result, by = by,
                                               max_dist = 1e-8)
      } else {
        ## keep result mathced by grid
        res <- fuzzyjoin::difference_semi_join(result, grid, by = by,
                                               max_dist = 1e-8)
      }
      ## convert integer to character
      for (name in colnames(res)) {
        if (name %in% names(levels)) {
          val <- dplyr::pull(res, !!name)
          res[, name] <- levels[[name]][val]
        }
      }
      res
    },

    .align_grid = function() {
      grid <- self$model$fit_param_specs$get_grid()
      res <- private$.result
      if (is.null(res)) return(grid)
      private$.setdiff(grid, res, drop_grid = TRUE)
    },

    .align_result = function() {
      res <- super$.align_result()
      if (is.null(res)) return(NULL)
      grid <- self$model$fit_param_specs$get_grid()
      res <- private$.setdiff(grid, res, drop_grid = FALSE)
      if (nrow(res) == 0L) NULL else res
    },
  )
)

#' SearchGrid Class Constructor
#'
#' @rdname SearchGrid
#'
#' @param options A [Options] class object.
#' @param keys A named character vector of keys.
#'
#' @return A new `SearchGrid` class object
#'
#' @export
new_search_grid <- function(options = NULL, keys = NULL) {
  SearchGrid$new(options, keys)
}
