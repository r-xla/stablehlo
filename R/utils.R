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
  sprintf("(%s)", paste(shape, collapse = ","))
}

camel_to_snake_case <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x)
  tolower(x)
}
