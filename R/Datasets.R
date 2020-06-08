#' @title Datasets Class
#'
#' @description
#' Key-value dictionary contains `data.frame` class objects.
#' This class is used to extract train and test data for model input, and also
#' contains [ResampleCalls] class object to generate resamples.
#'
#' @export
Datasets <- R6::R6Class(
  classname = "Datasets",
  inherit = Option,
  public = rlang::list2(
    #' @description
    #' Construct a new `Datasets` class object.
    #' @param ... Key and value pairs.
    #' @param .id_col A character scalar. An id column name contains integer
    #' ids. Leave `NULL` if dataset does not have id column.
    #' @param .test_ids A integer vector specifiying test data. If `id_col` is
    #' not `NULL`, `test_ids` must be `id_col` ranges. If `NULL` `test_ids` are
    #' considered as row numbers.
    #' @param .resample_calls A [ResampleCalls] class object.
    #' @param .value_width A integer scalar of character length. Used as max
    #' width of output value by `self$print()` method.
    #' @return A new `Datasets` class object
    initialize = function(..., .id_col = NULL, .test_ids = NULL,
                          .resample_calls = NULL, .value_width = Inf) {
      private$.id_col <- .assert_str(.id_col, allow_null = TRUE)
      private$.test_ids <- private$.assert_test_ids(.test_ids)
      private$.resample_calls <- .assert_class(.resample_calls, "ResampleCalls",
                                               allow_null = TRUE)
      super$initialize(..., .class = "data.frame", .key_prefix = "dat_",
                       .value_width = .value_width)
    },

    #' @description
    #' Print object.
    print = function() {
      key_width <- 10L
      cat(crayon::bgRed(" Datasets "), "\n")

      ## id_col
      .print_sep()
      .print("ID column", self$id_col, key_width, FALSE, TRUE)

      ## test_ids
      if (!self$has_test_ids) {
        .print("Test IDs", "NULL", key_width, FALSE, FALSE)
      } else {
        tid <- self$test_ids
        test_ids_len <- paste("Length", length(tid))
        ## All sequential numbers
        if (all(diff(tid) == 1L)) {
          test_ids <- ifelse(is.null(tid), "NULL",
                             paste0(min(tid), ":", max(tid)))
        } else {
          width <- options()$width - stringr::str_length(test_ids_len) - 14L
          test_ids <- paste(tid, collapse = ", ") %>%
            stringr::str_trunc(width)
        }
        test_ids_str <- paste0(test_ids_len, " [", test_ids, "]")
        .print("Test IDs", test_ids_str, key_width, FALSE, FALSE)
      }

      ## Datasets
      .print_sep()
      super$print_items(items_name = "Datasets", format_fun = .str_df)

      ## ResampleCalls
      .print_sep()
      if (!is.null(self$resample_calls)) {
        self$resample_calls$print_items(items_name = "Resample calls",
                                        format_fun = .str_call)
        .print_sep()
      }
    },

    #' @description
    #' Adds key-value objects to the dictionary.
    #' @param ... Key and value pairs.
    add = function(...) {
      private$.safely({
        dots <- list(...)
        rlang::exec(super$add, !!!dots)
        private$.check_names()
        private$.check_test_ids()
      })
    },

    #' @description
    #' Get data.
    #' @param key A character or integer scalar of the keys.
    #' @param preproc_call A `call` to convert data.
    #' @return A `data.frame`.
    get = function(key = NULL, preproc_call = NULL) {
      data <- super$get(key)
      if (is.null(preproc_call) || !is.call(preproc_call)) return(data)

      ## apply preproc call except for id_col
      if (self$has_id_col) {
        ids <- dplyr::select(data, self$id_col)
        data <- dplyr::select(data, -self$id_col)
      }
      data_args <- list(data = data)
      data <- rlang::eval_tidy(preproc_call, data_args)

      ## add id_col again
      if (self$has_id_col) data <- dplyr::bind_cols(ids, data)
      data
    },

    #' @description
    #' Get train data.
    #' @param key A character or integer scalar of the keys.
    #' @param preproc_call A `call` to convert data.
    #' @return A `data.frame` of train data.
    get_train = function(key = NULL, preproc_call = NULL) {
      data <- self[key, preproc_call]
      if (!self$has_test_ids) return(data)

      if (self$has_id_col) {
        ## slice by ids
        id_sym <- rlang::sym(self$id_col)
        data %>%
          ## all data returned if test_ids is NULL
          dplyr::filter(!(!!id_sym %in% self$test_ids)) %>%
          dplyr::select(-!!id_sym)
      } else {
        ## slice by row index
        dplyr::slice(data, -self$test_ids)
      }
    },

    #' @description
    #' Get test data.
    #' @param key A character or integer scalar of the keys.
    #' @param preproc_call A `call` to convert data.
    #' @return A `data.frame` of test data.
    get_test = function(key = NULL, preproc_call = NULL) {
      if (!self$has_test_ids)
        stop("test_ids is not set.", call. = FALSE)

      data <- self[key, preproc_call]
      if (self$has_id_col) {
        ## slice by ids
        id_sym <- rlang::sym(self$id_col)
        data %>%
          dplyr::filter(!!id_sym %in% self$test_ids) %>%
          dplyr::select(-!!id_sym)
      } else {
        ## slice by row index
        dplyr::slice(data, self$test_ids)
      }
    },

    #' @description
    #' Get resample data. Internally call `self$resample_calls$get_resample()`.
    #' @param dataset_key A character or integer scalar of the datasets keys.
    #' @param resample_call_key A character or integer scalar of the
    #' resample_calls keys.
    #' @param seed A integer scalar seed.
    #' @param preproc_call A `call` to convert data.
    #' @return A resample of `rset` class object.
    get_resample = function(dataset_key = NULL, resample_call_key = NULL,
                            seed = NULL, preproc_call = NULL) {
      if (!self$has_resample_calls)
        stop("Resampel calls are not set.", call. = FALSE)
      seed <- .assert_seed(seed)
      data <- self$get_train(dataset_key, preproc_call)
      self$resample_calls$get_resample(data, resample_call_key, seed)
    },
  ),
  active = rlang::list2(
    #' @field id_col An id column name.
    id_col = function() private$.id_col,
    #' @field test_ids Test data ids.
    test_ids = function() private$.test_ids,
    #' @field resample_calls A [ResampleCalls] class object.
    resample_calls = function() private$.resample_calls,

    #' @field has_id_col Wheather to have `id_col`.
    has_id_col = function() !is.null(self$id_col),
    #' @field has_test_ids Wheather to have `test_ids`.
    has_test_ids = function() !is.null(self$test_ids),
    #' @field has_resample_calls Wheather to have `resample_calls`.
    has_resample_calls = function() !is.null(self$resample_calls),
  ),
  private = rlang::list2(
    .assert_test_ids = function(test_ids = NULL) {
      if (is.null(test_ids)) return(NULL)

      if (anyNA(test_ids))
        stop("test_ids must not have any NAs.", call. = FALSE)

      if (!rlang::is_integerish(test_ids) || any(test_ids < 1L))
        stop("test_ids must be positive integer vector.", call. = FALSE)

      test_ids
    },

    .check_names = function() {
      purrr::walk(self$values, function(df) {
        ## Check if df contains id_col
        col_names <- colnames(df)
        if (self$has_id_col && !self$id_col %in% col_names)
          stop("data.frame does not contains ", .str_quote(self$id_col),
               " columns.", call. = FALSE)

        ## Check if names are all valid as some model fail with invalid names.
        valid_names <- make.names(col_names)
        invalid_names <- setdiff(col_names, valid_names)
        if (length(invalid_names) > 0L)
          stop(.str_quote(invalid_names),
               " is not valid colnames for model inputs.",
               " Use make.names() to convert to valid names.",
               call. = FALSE)
      })
    },

    .check_test_ids = function() {
      if (!self$has_test_ids) return(NULL)
      if (self$has_id_col) {
        ## use id of smallest data
        max_id <- purrr::map_int(self$values, ~ {
          ids <- dplyr::pull(., self$id_col)
          max(ids)
        }) %>% min()
      } else {
        ## use rows of smallest data
        max_id <- min(purrr::map_int(self$values, nrow))
      }

      if (!(1L <= min(self$test_ids) & max(self$test_ids) <= max_id))
        stop("test_ids must be between 1 to ", max_id, ".", call. = FALSE)
    },

    .test_ids = integer(),
    .id_col = character(),
    .resample_calls = NULL,
  )
)

#' Datasets Class Constructor
#'
#' @rdname Datasets
#'
#' @param ... Key and value pairs.
#' @param .id_col A character scalar. An id column name contains integer ids.
#' Leave `NULL` if dataset does not have id column.
#' @param .test_ids A integer vector specifiying test data. If `id_col`
#' is not `NULL`, `test_ids` must be `id_col` ranges.
#' If `NULL` `test_ids` are considered as row numbers.
#' @param .resample_calls A [ResampleCalls] class object.
#' @param .value_width A integer scalar of character length. Used as max
#' width of output value by `self$print()` method.
#'
#' @return A new [Datasets] class object
#'
#' @export
new_datasets <- function(..., .id_col = NULL, .test_ids = NULL,
                         .resample_calls = NULL, .value_width = Inf) {
  Datasets$new(..., .id_col = .id_col, .test_ids = .test_ids,
               .resample_calls = .resample_calls, .value_width = .value_width)
}
