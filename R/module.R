#' @include func.R
NULL

#' @title Module
#' @description
#' A StableHLO module containing one or more [`Func`] objects.
#' Modules allow defining multiple named functions, where functions can
#' call each other using [`hlo_call`].
#'
#' Note: Module uses reference semantics like [`Func`].
#' @param funcs (`list()` of [`Func`])\cr
#'   The functions in the module.
#' @return A `Module` object.
#' @export
Module <- function(funcs = list()) {
  lapply(funcs, function(f) {
    if (!test_class(f, "Func")) {
      cli_abort("{.arg funcs} must all be {.cls Func} objects.")
    }
  })

  env <- new.env(parent = emptyenv())
  env$funcs <- funcs

  class(env) <- c("Module", "environment")
  env
}

#' @title Get the current module
#' @description
#' Get the current module created via [`hlo_module`] or [`local_module`].
#' @return A [`Module`] object.
#' @export
.current_module <- function() {
  globals[["CURRENT_MODULE"]] %??%
    cli_abort("No module is currently being built.")
}

#' @title Create a module
#' @description
#' Both functions create a new [`Module`] which is afterwards accessible via [`.current_module()`].
#' Functions created with [`hlo_func`] or [`local_func`] will automatically register into the
#' current module.
#' The module is finalized when a function named `"main"` is returned via [`hlo_return`].
#'
#' Differences between the two functions:
#' * [`local_module`] removes the module when exiting the current scope, whereas [`hlo_module`] does not.
#' * [`hlo_module`] discards the previously built module(s), whereas [`local_module`] does not.
#'
#' @return A [`Module`] object.
#' @export
hlo_module <- function() {
  mod <- Module()
  globals[["CURRENT_MODULE"]] <- mod
  return(mod)
}

#' @rdname hlo_module
#' @param envir (`environment`)\cr
#'   Environment where exit handler will be registered for cleaning up the
#'   [`Module`] if it was not finalized yet.
#' @export
local_module <- function(envir = parent.frame()) {
  mod <- Module()
  if (!is.null(globals[["CURRENT_MODULE"]])) {
    globals[["MODULE_STASH"]] <- c(
      globals[["MODULE_STASH"]],
      list(globals[["CURRENT_MODULE"]])
    )
  }
  globals[["CURRENT_MODULE"]] <- mod

  withr::defer(
    envir = envir,
    {
      maybe_restore_previous_module(mod)
    },
    priority = "first"
  )
  return(mod)
}

register_func_in_module <- function(func) {
  mod <- globals[["CURRENT_MODULE"]]
  if (is.null(mod)) {
    return(invisible(NULL))
  }
  existing_ids <- vapply(mod$funcs, function(f) f$id$id, character(1))
  if (func$id$id %in% existing_ids) {
    cli_abort(c(
      "Function names within a module must be unique.",
      x = "A function named {.val {func$id$id}} already exists in the module."
    ))
  }
  mod$funcs <- c(mod$funcs, list(func))
  invisible(NULL)
}

finalize_module <- function() {
  mod <- globals[["CURRENT_MODULE"]]
  if (is.null(mod)) {
    return(invisible(NULL))
  }
  maybe_restore_previous_module()
  invisible(NULL)
}

maybe_restore_previous_module <- function(mod = NULL) {
  if (!is.null(mod) && !identical(mod, globals[["CURRENT_MODULE"]])) {
    return()
  }

  stash_size <- length(globals[["MODULE_STASH"]])
  if (stash_size) {
    globals[["CURRENT_MODULE"]] <- globals[["MODULE_STASH"]][[stash_size]]
    globals[["MODULE_STASH"]] <- globals[["MODULE_STASH"]][-stash_size]
  } else {
    globals[["CURRENT_MODULE"]] <- NULL
  }
}

#' @export
repr.Module <- function(x, ...) {
  func_reprs <- vapply(
    x$funcs,
    function(f) {
      fr <- repr(f)
      fr <- sub("\n$", "", fr)
      lines <- strsplit(fr, "\n", fixed = TRUE)[[1L]]
      paste0("  ", lines, collapse = "\n")
    },
    character(1)
  )

  paste0(
    "module {\n",
    paste0(func_reprs, collapse = "\n"),
    "\n}\n"
  )
}

#' @export
print.Module <- function(x, ...) {
  cat(repr(x))
}
