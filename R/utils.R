#' @include constant.R
#' @include types.R
NULL


func_output_types <- function(func) {
  if (!test_class(func, "Func")) {
    cli_abort("func must be a Func object, but got {.class {class(func)[1]}}.")
  }
  lapply(func$outputs, function(x) x$type)
}

output_types_from_body <- function(body) {
  body[[length(body)]]$inputs$values
}

snake_to_camel <- function(str) {
  paste(capitalize(strsplit(str, "_")[[1]]), collapse = "")
}

capitalize <- function(str) {
  substr(str, 1L, 1L) <- toupper(substr(str, 1L, 1L))
  str
}

get_dims <- function(data) {
  if (is.null(dim(data))) {
    if (length(data) == 1) {
      return(integer())
    } else {
      return(length(data))
    }
  }
  dim(data)
}

maybe_restore_previous_func <- function(func = NULL) {
  if (!is.null(func) && !identical(func, globals[["CURRENT_FUNC"]])) {
    # function has already been returned
    return()
  }

  stash_size <- length(globals[["FUNC_STASH"]])
  if (stash_size) {
    globals[["CURRENT_FUNC"]] <- globals[["FUNC_STASH"]][[stash_size]]
    globals[["FUNC_STASH"]] <- globals[["FUNC_STASH"]][-stash_size]
  } else {
    globals[["CURRENT_FUNC"]] <- NULL
  }
}

format_shapes_msg <- function(prefix, ...) {
  rlang::check_dots_used()
  shapes <- list(...)

  if (!all(names(shapes) != "")) {
    cli_abort("All shapes should be named")
  }

  msgs <- paste0(
    "{.field ",
    names(shapes),
    "}=[",
    sapply(shapes, paste0, collapse = "x"),
    "]"
  )

  paste(prefix, paste(msgs, collapse = ", "))
}

shapevec_repr <- function(shape) {
  shape[is.na(shape)] <- "?"
  sprintf("(%s)", paste(shape, collapse = "x"))
}

vec_repr <- function(x) {
  if (length(x) <= 1L) {
    as.character(x)
  } else {
    paste0("c(", paste0(x, collapse = ", "), ")")
  }
}

ensure_func_vals <- function(x) {
  if (inherits(x, "FuncValue") || !is.list(x)) list(x) else x
}

without <- function(x, indices) {
  if (length(indices)) {
    x[-indices]
  } else {
    x
  }
}
