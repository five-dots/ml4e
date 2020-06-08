
#' Supported specifications by ml4e package
#'
#' @rdname supported_specs
#' @examples
#' supported_engines()
#'
#' @export
supported_engines <- function() unique(model_specs$engine)

#' @rdname supported_specs
#' @param engine A character scalar of model engine.
#' @examples
#' supported_tasks("glm")
#'
#' @export
supported_tasks <- function(engine = NULL) {
  .assert_engine(engine)
  model_specs %>%
    dplyr::filter(.data$engine == !!engine) %>%
    dplyr::pull(.data$tasks) %>%
    purrr::pluck(1)
}

#' @rdname supported_specs
#' @examples
#' supported_metrics()
#'
#' @export
supported_metrics <- function() {
  funs <- pacman::p_functions("yardstick")

  purrr::map_dfr(funs, function(fun) {
    ## Keep functions that meet the conditions
    ##  1. S3 for data.frame
    ##  2. The first 2 args are "data" and "truth"
    df_fun_str <- paste0("yardstick:::", fun, ".data.frame")
    df_fun_expr <- rlang::parse_expr(df_fun_str)

    args <- tryCatch({
      rlang::fn_fmls_names(eval(df_fun_expr))
    }, error = function(e) NULL)
    if (is.null(args) || all(args[1L:2L] != c("data", "truth"))) return(NULL)

    ## Extract attributes
    ##  1. class: supported data types such as "class", "prob" or "numeric"
    ##  2. direction: "minimize" or "maximize"
    fun_str <- paste0("yardstick::", fun)
    fun_expr <- rlang::parse_expr(fun_str)

    attrs <- tryCatch({
      attributes(eval(fun_expr))
    }, error = function(e) NULL)
    if (is.null(attrs)) return(NULL)

    ## Build a data.frame
    type <- stringr::str_split(attrs$class[1L], "_")[[1L]][1L]
    minimize <- attrs$direction == "minimize"
    tibble::tibble(metric = fun, type = type, minimize = minimize)
  })
}

## -----------------------------------------------------------------------------
.labels_from_formula <- function(formula, data) {
  mf <- stats::model.frame(formula, data, na.action = "na.pass")
  stats::model.response(mf)
}

.labels_from_dmatrix <- function(dmatrix) {
  call <- switch(class(dmatrix)[1],
    "xgb.DMatrix" = {
      args <- list(object = dmatrix, name = "label")
      call <- rlang::call2("getinfo", !!!args, .ns = "xgboost")
    },
    "lgb.Dataset" = {
      args <- list(dataset = dmatrix, name = "label")
      call <- rlang::call2("getinfo", !!!args, .ns = "lightgbm")
    }
  )
  rlang::eval_tidy(call, env = rlang::current_env())
}

#' Get family from task type
#'
#' @param task_type A character scalar of task_type.
#' @return A character scalar of family.
#'
#' @examples
#' family_from_task("multiclass") # "multinomial"
#' family_from_task("binary") # "binomial"
#' family_from_task("regression") # "gaussian"
#'
#' @export
family_from_task <- function(task_type) {
  .assert_str(task_type)
  switch(
    task_type,
    "regression" = "gaussian",
    "poisson" = "poisson",
    "binary" = "binomial",
    "multiclass" = "multinomial",
    "survival" = "cox",
    stop("could not find appropreate family.", call. = FALSE)
  )
}

## TODO Keep or delete ?
#' Check if resample has any errors
#'
#' @param resample A \code{rset} object
#'
#' @export
check_resample_levels <- function(resample) {
  check_nlevels <- function(df, id, is_train = TRUE) {
    df <- dplyr::select_if(df, is.factor)
    l <- as.list(df)
    purrr::iwalk(l, function(value, name) {
      nlevels <- nlevels(droplevels(value))
      if (nlevels < 2L)
        cat(paste0(name, ": nlevels=", nlevels,
                    " (", ifelse(is_train, "train", "test"), "@", id, ")"),
            "\n")
    })
  }
  check_test_levels <- function(train, test, id) {
    train <- dplyr::select_if(train, is.factor)
    test <- dplyr::select_if(test, is.factor)
    purrr::walk(colnames(train), function(col_name) {
      train_levels <- levels(droplevels(train[, col_name, drop = TRUE]))
      test_levels <- levels(droplevels(test[, col_name, drop = TRUE]))
      if (!all(test_levels %in% train_levels)) {
        diff <- setdiff(test_levels, train_levels)
        cat(col_name, "@", id, ": ",
            paste0(diff, collapse = ", "),
            "\n", sep = "")
      }
    })
  }
  for (i in 1L:nrow(resample)) {
    row <- resample[i, ]
    rsplit <- row$splits[[1L]]
    id <- row$id
    train <- rsample::analysis(rsplit)
    check_nlevels(train, id, is_train = TRUE)
    test <- rsample::assessment(rsplit)
    check_nlevels(test, id, is_train = FALSE)
    check_test_levels(train, test, id)
  }
}

.as_ratio <- function(x, n) {
  ## return x as is, if x is less than or equal to 1
  ## if x is greater than 1, convert it to ratio
  if (x > 1) x <- x/n
  if (x > 1) x <- 1
  x
}

.filter_or_slice <- function(df, ...) {
  .assert_class(df, "data.frame")
  if (nrow(df) == 0)
    stop("data.frame must not be an empty.", call. = FALSE)

  ## filter
  res <- tryCatch(dplyr::filter(df, ...), error = function(e) NULL)

  ## If fail, then try slice
  if (is.null(res))
    res <- tryCatch(dplyr::slice(df, ...), error = function(e) NULL)

  ## If filter and slice failed, then throw an error
  if (is.null(res))
    stop("Conditions are not valid for dplyr::filter() or dplyr::slice().",
         call. = FALSE)
  res
}

.get_matched_index <- function(df1, df2) {
  .assert_class(df1, "data.frame")
  .assert_class(df2, "data.frame")

  ## omit list columns
  df1 <- dplyr::select_if(df1, ~ !is.list(.))
  df2 <- dplyr::select_if(df2, ~ !is.list(.))

  ## join with rowid
  by <- intersect(colnames(df1), colnames(df2))
  df1 <- tibble::rowid_to_column(df1)
  df <- dplyr::left_join(df2, df1, by = by)

  ## extract rowid only
  dplyr::pull(df, .data$rowid)
}

## TODO Add unit test
.sort_by_metric <- function(df, metric_name, minimize, n = 5L) {
  expr <- rlang::parse_expr(metric_name)
  df %>%
    dplyr::group_modify(~ {
      if (minimize) {
        dplyr::mutate(., rank = dplyr::dense_rank(!!expr))
      } else {
        dplyr::mutate(., rank = dplyr::dense_rank(dplyr::desc(!!expr)))
      }
    }) %>%
    dplyr::filter(dplyr::between(rank, 1L, n)) %>%
    dplyr::select(rank, dplyr::everything()) %>%
    dplyr::arrange(rank)
}
