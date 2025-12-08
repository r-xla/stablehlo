#' @include constant.R
#' @include types.R
NULL


func_output_types <- function(func) {
  if (!inherits(func, Func)) {
    cli_abort("func must be a Func object, but got {.class {class(func)[1]}}.")
  }
  lapply(func@outputs@items, function(x) x@type)
}

output_types_from_body <- function(body) {
  body@items[[length(body@items)]]@inputs@values
}

#' @title Represent an R value in stableHLO
#' @description
#' Convert R value to StableHLO string representation
#' @param value (any)\cr
#'  The R value to convert
#' @param dtype (`character(1)`)\cr
#'   The element type to use.
#' @return `character(1)`
r_to_stablehlo_string <- function(value, dtype) {
  if (is.logical(value)) {
    if (value) {
      return("true")
    } else {
      return("false")
    }
  } else if (is.integer(value)) {
    return(as.character(value))
  } else if (is.numeric(value)) {
    if (dtype == "f32") {
      format_double(value, precision = 32)
    } else {
      format_double(value, precision = 64)
    }
  } else {
    return(as.character(value))
  }
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
      return(1L)
    } else if (length(data) == 0) {
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
    cli::cli_abort("All shapes should be named")
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
