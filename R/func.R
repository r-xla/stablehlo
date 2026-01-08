#' @include list_of.R
#' @include types.R
#' @include repr.R
#' @include value_id.R
NULL

#' @title FuncInput
#' @description
#' This represents an input of a [`Func`].
#' @param id ([`ValueId`])\cr
#'   The id of the input.
#' @param type ([`ValueType`])\cr
#'   The type of the input.
#' @param alias (`integer(1)` | `NULL`)\cr
#'   With which output buffer to alias this input.
#' @return (`FuncInput`)
#' @export
FuncInput <- function(id, type, alias = NULL) {
  checkmate::assert_class(id, "ValueId")
  checkmate::assert_class(type, "ValueType")
  if (!is.null(alias)) {
    alias <- as.integer(alias)
    checkmate::assert_int(alias)
  }

  structure(
    list(id = id, type = type, alias = alias),
    class = "FuncInput"
  )
}

#' @export
repr.FuncInput <- function(x, ...) {
  attr_str <- ""
  if (!is.null(x$alias)) {
    idx <- as.integer(x$alias)
    attr_str <- paste0(" {tf.aliasing_output = ", idx, " : i32}")
  }
  paste0(
    repr(x$id),
    ": ",
    repr(x$type),
    attr_str
  )
}

#' @title FuncInputs
#' @description
#' List of [`FuncInput`]s.
#' @param items (`list()` of [`FuncInput`])\cr
#'   The inputs of the function.
#' @return (`FuncInputs`)
#' @export
FuncInputs <- new_list_of("FuncInputs", "FuncInput")

#' @export
repr.FuncInputs <- function(x, ...) {
  paste0(
    "(",
    paste0(vapply(x, repr, character(1)), collapse = ", "),
    ")"
  )
}

#' @title FuncOutput
#' @description
#' This represents an output of a [`Func`].
#' @param type (`ValueType`)\cr
#'   The type of the output.
#' @return (`FuncOutput`)
#' @export
FuncOutput <- function(type) {
  checkmate::assert_class(type, "ValueType")

  structure(
    list(type = type),
    class = "FuncOutput"
  )
}

#' @export
repr.FuncOutput <- function(x, ...) {
  repr(x$type)
}

#' @title FuncOutputs
#' @description
#' List of [`FuncOutput`]s.
#' @param items (`list()` of [`FuncOutput`])\cr
#'   The outputs of the function.
#' @return (`FuncOutputs`)
#' @export
FuncOutputs <- new_list_of("FuncOutputs", "FuncOutput")

#' @export
repr.FuncOutputs <- function(x, ...) {
  if (length(x) <= 1L) {
    paste0(
      "-> ",
      paste0(vapply(x, repr, character(1)), collapse = ", ")
    )
  } else {
    paste0(
      "-> (",
      paste0(vapply(x, repr, character(1)), collapse = ", "),
      ")"
    )
  }
}

#' @title FuncId
#' @description
#' This represents the id of a function.
#' @param id The id of the function.
#' @export
FuncId <- function(id = "main") {
  checkmate::assert_string(id)

  structure(
    list(id = id),
    class = "FuncId"
  )
}

#' @export
repr.FuncId <- function(x, ...) {
  paste0("@", x$id)
}

#' @export
`==.FuncId` <- function(e1, e2) {
  identical(e1$id, e2$id)
}

#' @title FuncBody
#' @description
#' The body of a [`Func`], containing a list of operations.
#' @param items (`list()` of [`Op`])\cr
#'   The operations in the function body.
#' @return (`FuncBody`)
#' @export
FuncBody <- new_list_of("FuncBody", "Op")

#' @export
repr.FuncBody <- function(x, ...) {
  paste0(vapply(x, repr, character(1)), collapse = "\n")
}

#' @title Get the last function created
#' @description
#' Get the last function created (either via [`hlo_func`] or [`local_func`]),
#' which is not returned yet.
#' @return A [`Func`] object.
#' @export
.current_func <- function() {
  globals[["CURRENT_FUNC"]] %??%
    cli_abort("No function is currently being built")
}

#' @title Create a function
#' @description
#' Both functions create a new [`Func`] with the given id which is afterwards accessible via [`.current_func()`].
#' Functions receiving a [`Func`] as an argument (such as [`hlo_input`], [`hlo_add`], ...) usually use
#' [`.current_func()`] by default.
#' You can also directly create a function using [`Func()`], which will *not* be accessible this way.
#'
#' Differences between the two functions:
#' * [`local_func`] removes the function when exiting the current scope, whereas [`hlo_func`] does not.
#' * [`hlo_func`] discards the previously built function(s), whereas [`local_func`] does not:
#'   after a function created by [`local_func`] is either cleaned up automatically (by exiting the scope)
#'   or the function is finalized via [`hlo_return`], the previously built function is restored,
#'   i.e., accessible via [`.current_func()`]. To build nested functions (e.g. to create a closure
#'   that is passed to another op), use [`local_func`] instead of [`hlo_func`].
#'
#' @param id (`character(1)`\cr
#'   The id of the function.
#' @return A [`Func`] object.
#' @export
hlo_func <- function(id = "main") {
  func <- Func(id = FuncId(id))
  globals[["CURRENT_FUNC"]] <- func
  return(func)
}

#' @rdname hlo_func
#' @param envir (`environment`)\cr
#'   Environment where exit handler will be registered for cleaning up the
#'   [`Func`] if it was not returned yet.
#' @export
local_func <- function(id = "main", envir = parent.frame()) {
  func <- Func(FuncId(id))
  if (!is.null(globals[["CURRENT_FUNC"]])) {
    globals[["FUNC_STASH"]] <- c(
      globals[["FUNC_STASH"]],
      list(globals[["CURRENT_FUNC"]])
    )
  }
  globals[["CURRENT_FUNC"]] <- func

  withr::defer(
    envir = envir,
    {
      maybe_restore_previous_func(func)
    },
    priority = "first"
  )
  return(func)
}

#' @title Func
#' @description
#' This represents a function.
#' Note: Func uses reference semantics - modifications to a Func object modify the original.
#' @param id (`FuncId`\cr
#'   The id of the function.
#' @param inputs (`FuncInputs`\cr
#'   The inputs of the function.
#' @param outputs (`FuncOutputs`\cr
#'   The outputs of the function.
#' @param body (`FuncBody`\cr
#'   The body of the function.
#' @return A [`Func`] object.
#' @export
Func <- function(
  id = FuncId(),
  inputs = FuncInputs(),
  outputs = FuncOutputs(),
  body = FuncBody()
) {
  if (is.character(id)) {
    id <- FuncId(id)
  }
  checkmate::assert_class(id, "FuncId")
  checkmate::assert_class(inputs, "FuncInputs")
  checkmate::assert_class(outputs, "FuncOutputs")
  checkmate::assert_class(body, "FuncBody")

  # Use an environment for reference semantics
  env <- new.env(parent = emptyenv())
  env$id <- id
  env$inputs <- inputs
  env$outputs <- outputs
  env$body <- body

  # Return the environment directly with Func class
  class(env) <- c("Func", "environment")
  env
}

#' @export
repr.Func <- function(x, ...) {
  # Func ::= 'func' '.' 'func' FuncId FuncInputs FuncOutputs '{' FuncBody '}'
  paste0(
    "func.func ",
    repr(x$id),
    " ",
    repr(x$inputs),
    " ",
    repr(x$outputs),
    " {\n",
    repr(x$body),
    "\n}\n"
  )
}

#' @export
print.Func <- function(x, ...) {
  cat(repr(x))
}

#' @title OpInputFunc
#' @description
#' This represents a function that can be used as input to an operation.
#' @param inputs (`FuncInputs`)\cr
#'   The inputs of the function.
#' @param body (`FuncBody`)\cr
#'   The body of the function.
#' @return (`OpInputFunc`)
#' @export
OpInputFunc <- function(inputs, body) {
  checkmate::assert_class(inputs, "FuncInputs")
  checkmate::assert_class(body, "FuncBody")

  structure(
    list(inputs = inputs, body = body),
    class = "OpInputFunc"
  )
}

#' @export
repr.OpInputFunc <- function(x, ...) {
  # Don't print parameters if there are none:
  if (length(x$inputs) == 0) {
    return(
      paste0(
        "{\n",
        paste0(
          vapply(
            x$body,
            function(item) repr(item, toplevel = FALSE),
            character(1)
          ),
          collapse = "\n    "
        ),
        "\n}"
      )
    )
  }
  paste0(
    "{\n  ^bb0",
    repr(x$inputs),
    ":\n    ",
    paste0(
      vapply(
        x$body,
        function(item) repr(item, toplevel = FALSE),
        character(1)
      ),
      collapse = "\n    "
    ),
    "\n}"
  )
}

#' @export
`==.OpInputFunc` <- function(e1, e2) {
  e1$inputs == e2$inputs && e1$body == e2$body
}

#' @title OpInputFuncs
#' @description
#' List of [`OpInputFunc`]s.
#' @param items (`list()` of [`OpInputFunc`])\cr
#'   The functions that can be used as inputs to operations.
#' @return (`OpInputFuncs`)
#' @export
OpInputFuncs <- new_list_of("OpInputFuncs", "OpInputFunc")

#' @export
repr.OpInputFuncs <- function(x, ...) {
  if (length(x) == 0) {
    return("")
  }

  paste0(
    "(",
    paste0(vapply(x, repr, character(1)), collapse = ", "),
    ")"
  )
}
