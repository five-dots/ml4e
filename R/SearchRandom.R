#' @title SearchRandom Class
#'
#' @description
#' A class defines a random search task specification.
#'
#' @export
SearchRandom <- R6::R6Class(
  classname = "SearchRandom",
  inherit = SearchBase,
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new `SearchRandom` class object.
    #'
    #' @param options A [Options] class object.
    #' @param keys A named character vector of keys.
    #'
    #' @return A new `SearchRandom` class object
    initialize = function(options = NULL, keys = NULL) {
      super$initialize(options, keys)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 14L
      self$print_task(title = " Random Search ", key_width = key_width)
      .print_sep()

      ## Result
      if (!is.null(self$result)) {
        cat("Random search result ", "[", self$progress_str, "]:", "\n",
            sep = "")
        print(self$result, n = 5L)
        .print_sep()
      }
    },

    #' @description
    #' Run a random search. The result will be stored in `self$result`.
    #' @param num A integer scalar of how many parameters to be calculated.
    do = function(num = NULL) {
      ## Assertion
      if (!self$model$has_fit_param_specs)
        stop("fit_param_specs is not set.", call. = FALSE)
      .assert_count(num)

      ## Random grid
      private$.set_fit_param_specs_defaults()
      grid <- self$model$fit_param_specs$get_random(num) %>%
        tibble::rowid_to_column()

      ## Message/Timer
      if (self$show_progress) {
        tictoc::tic()
        cat(.str_keys(self$keys), "\n")
      }

      ## Progress bar
      if (self$show_progress && !self$parallel)
        private$.progress_bar <- utils::txtProgressBar(0L, nrow(grid),
                                                       style = 3L)

      ## Generate grid tasks by fit_param
      if (self$parallel) {
        prev_plan <- future::plan(future::multiprocess)
        on.exit(future::plan(prev_plan))
        ## suppress furrr's message
        res <- suppressMessages(
          furrr::future_pmap_dfr(
            grid, private$.cv_by_fit_param,
            .progress = self$show_progress && self$parallel
          )
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
  ),
  active = rlang::list2(
    #' @field progress A list of random search calculation progress.
    progress = function() {
      done <- if (!is.null(self$result)) nrow(self$result) else 0L
      total <- if (!is.null(private$.result)) nrow(private$.result) else 0L
      list(done = done, total = total)
    },
    #' @field progress_str A character of progress.
    progress_str = function() {
      prg <- self$progress
      paste0(prg$done, " (ttl: ", prg$total, ")")
    },
  ),
  private = rlang::list2(
    .align_result = function() {
      res <- super$.align_result()
      if (is.null(res)) return(NULL)

      grid <- self$model$fit_param_specs$get_grid()
      uniques <- purrr::map(as.list(grid), unique)
      exprs <- purrr::imap(uniques, function(value, param) {
        param <- rlang::sym(param)
        if (is.numeric(value)) {
          rlang::call2("between", param, min(value), max(value), .ns = "dplyr")
        } else {
          rlang::call2("%in%", param, value)
        }
      })
      names(exprs) <- NULL
      res <- dplyr::filter(res, !!!exprs)
      if (nrow(res) == 0L) NULL else res
    },
  )
)

#' SearchRandom Class Constructor
#'
#' @rdname SearchRandom
#'
#' @param options A [Options] class object.
#' @param keys A named character vector of keys.
#'
#' @return A new `SearchRandom` class object
#'
#' @export
new_search_random <- function(options = NULL, keys = NULL) {
  SearchRandom$new(options, keys)
}
