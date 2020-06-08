
.get_first_arg_name <- function(fun) {
  ## This func is called inside another func and return the first argument name.
  ## Example:
  ##  f(a) is called as f(): -> "a"
  ##  f(a) is called as f(b): -> "b" (b is a variable)
  ##  f(a) is called as f(a = b): -> "b"
  ##  if f does not have any argument, this func return NULL
  call <- sys.call(which = -1)
  if (length(call) > 1 && is.symbol(call[[2]])) {
    rlang::as_string(call[[2]])
  } else {
    names(formals(fun))[1]
  }
}

.assert_str <- function(str = NULL, allow_null = FALSE) {
  allow_null <- .assert_flag(allow_null)
  if (allow_null && is.null(str)) return(NULL)

  arg_name <- .get_first_arg_name(.assert_str)
  if (is.null(str))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (!rlang::is_string(str))
    stop(arg_name, " must be a character scalar.", call. = FALSE)
  str
}

.assert_count <- function(count = NULL, allow_null = FALSE) {
  allow_null <- .assert_flag(allow_null)
  if (allow_null && is.null(count)) return(NULL)

  arg_name <- .get_first_arg_name(.assert_count)
  if (is.null(count))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (anyNA(count) || !rlang::is_scalar_integerish(count) || count < 1)
    stop(arg_name, " must be a positive integer scalar.", call. = FALSE)
  count
}

.assert_flag <- function(flag = NULL) {
  arg_name <- .get_first_arg_name(.assert_flag)
  if (is.null(flag))
    stop(arg_name, " must not be NULL.", call. = FALSE)
  if (any(is.na(flag)) || !rlang::is_bool(flag))
    stop(arg_name, " must be a logical scalar.", call. = FALSE)
  flag
}

.assert_class <- function(object = NULL, class = NULL, allow_null = FALSE) {
  class <- .assert_str(class)
  allow_null <- .assert_flag(allow_null)
  if (allow_null && is.null(object)) return(NULL)

  arg_name <- .get_first_arg_name(.assert_class)
  if (!inherits(object, class))
    stop(arg_name, " must be a ", class, " class.", call. = FALSE)
  object
}

.assert_engine <- function(engine) {
  .assert_str(engine)
  if (!engine %in% supported_engines())
    stop(engine, " is not a supported engine.", call. = FALSE)
  engine
}

.assert_task_type <- function(task_type) {
  .assert_str(task_type)
  valid_task_types <- model_specs$tasks %>% unlist() %>% unique()
  if (!task_type %in% valid_task_types)
    stop(task_type, " is not valid task type.", call. = FALSE)
  task_type
}

.assert_fit_param <- function(fit_param) {
  if (!is.null(fit_param) && (!is.list(fit_param) ||
                              !rlang::is_dictionaryish(fit_param)))
    stop("fit_param must be a named list.", call. = FALSE)
  fit_param
}

.assert_seed <- function(seed) {
  if (!rlang::is_scalar_integerish(seed) || anyNA(seed))
    stop("seed must be a integer scalar.", call. = FALSE)
  seed
}
