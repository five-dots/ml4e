
.str_quote <- function(strings) {
  purrr::map_chr(strings, function(string) {
    paste0("\"", string, "\"")
  }) %>% paste0(collapse = ", ")
}

.str_df <- function(df) {
  col_names <- paste0(colnames(df), collapse = ", ")
  paste0(nrow(df), " x ", ncol(df), " (", col_names, ")")
}

.str_list <- function(l) {
  purrr::imap_chr(l, function(body, name) {
    if (is.list(body) && !is.data.frame(body)) {
      body <- paste0("list(", .str_list(body), ")")
      paste0(name, " = ", body)
    } else {
      ## TODO Support more class
      if (is.character(body)) body <- .str_quote(body)
      if (is.data.frame(body)) body <- "data.frame"
      if (is.function(body)) body <- "function"
      if (is.null(body)) body <- "NULL"
      paste0(name, " = ", body)
    }
  }) %>%
    paste0(collapse = ", ")
}

.str_call <- function(call) {
  deparse(call) %>%
    stringr::str_trim() %>%
    paste(collapse = " ")
}

.str_task <- function(task_type, label_levels) {
  task_type <- .str_quote(task_type)
  if (!is.null(label_levels)) {
    label_levels <- paste0(label_levels, collapse = ", ")
    task_type <- paste0(task_type, " (Levels: ", label_levels, ")")
  }
  task_type
}

.str_keys <- function(keys) {
  purrr::imap_chr(keys, function(val, name) {
    paste0(name, "=", .str_quote(val))
  }) %>% paste0(collapse = ", ")
}

.str_model <- function(model) {
  paste("Engine =", .str_quote(model$engine))
}

## TODO Add unit test
.str_fit_param_spec <- function(spec) {
  if (spec$type == "character") {
    values <- paste0("[", .str_quote(spec$trans_values), "]")
    if (spec$has_default) {
      default <- .str_quote(spec$trans_default)
      values <- paste0(values, " (d=", default, ")")
    }
  } else {
    values <- paste0("[", paste(spec$trans_values, collapse = ", "), "]")
    if (spec$has_default) {
      values <- paste0(values, " (d=", spec$trans_default, ")")
    }
  }
}

.str_split <- function(string, width) {
  len <- stringr::str_length(string)
  i <- 1
  strings <- character()
  while (i <= len) {
    str <- stringr::str_sub(string, i, i+width-1)
    strings <- c(strings, str)
    i <- i+width
  }
  strings
}

.print <- function(key, value, key_width, quote_key = FALSE,
                   quote_value = FALSE, value_width = Inf) {
  width <- options()$width
  key_col_w <- min(key_width, round(width * 0.3)) # max 24
  value_w <- width - key_col_w - 1 # one space

  ## key string width
  if (quote_key) {
    ## two quotation + one semicolon
    key_str_w <- key_col_w - 3
  } else {
    ## one semicolon
    key_str_w <- key_col_w - 1
  }

  if (stringr::str_length(key) > key_str_w)
    key <- stringr::str_trunc(key, key_str_w)
  if (quote_key)
    key <- .str_quote(key)
  key <- paste0(key, ":")
  key <- stringr::str_pad(key, key_col_w, "right")

  ## values
  if (is.null(value)) {
    value <- "NULL"
    quote_value <- FALSE
  }
  if (stringr::str_length(value) > value_width)
    value <- stringr::str_trunc(value, value_width)
  if (quote_value) value <- .str_quote(value)
  values <- .str_split(value, value_w)

  ## first line
  cat(key, " ", values[1], "\n", sep = "")

  ## rest lines
  if (length(values) > 1) {
    for (i in 2:length(values)) {
      pad <- stringr::str_pad("", key_col_w)
      cat(pad, " ", values[i], "\n", sep = "")
    }
  }
}

.print_sep <- function() {
  width <- options()$width
  sep <- stringr::str_dup("-", width)
  cat(sep, "\n")
}
