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
  if (!is.null(alias)) {
    alias <- as.integer(alias)
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
  if (nchar(id) > 0L && !grepl("^[a-zA-Z_][a-zA-Z0-9_$.]*$", id)) {
    cli_abort(c(
      "{.arg id} must be a valid identifier.",
      x = "Got {.val {id}}."
    ))
  }

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

# Append a deferred op to the func's buffer. Each op is stored as a
# `list(render, payload)` pair; the line is produced by `render(payload)` at
# repr time (see func_lines()), once value ids can be numbered. The buffer is
# a cons list, so appending is O(1) per op and never touches earlier ops.
func_emit <- function(func, item) {
  buf <- func[["buf"]]
  buf$n <- buf$n + 1L
  buf$stack <- list(item, buf$stack)
}

# Rendered op lines of a Func, in emission order (`character()`). Rendering
# happens here (not at creation) so value ids get their numbers in
# appearance order; ops are therefore rendered front-to-back.
func_lines <- function(func) {
  # Normally repr.Func installs the numbering scope; when func_lines() is
  # called on its own (e.g. directly, in tests) install a transient one so
  # ids still number from %0. Nested calls (region funcs) share the scope.
  if (is.null(globals[["REPR_IDS"]])) {
    push_repr_ids(collect_named_numeric(func))
    on.exit(pop_repr_ids(NULL), add = TRUE)
  }
  buf <- func[["buf"]]
  n <- buf$n
  # The cons list is LIFO (head = last op); materialise in emission order.
  items <- vector("list", n)
  node <- buf$stack
  i <- n
  while (i > 0L) {
    items[[i]] <- node[[1L]]
    node <- node[[2L]]
    i <- i - 1L
  }
  lines <- character(n)
  for (i in seq_len(n)) {
    item <- items[[i]]
    lines[[i]] <- item[[1L]](item[[2L]])
  }
  lines
}

# Integers already claimed by named ids in this func and its region funcs, so
# repr-time numbering can skip exactly those (see id_number()).
collect_named_numeric <- function(func) {
  out <- integer()
  for (inp in func$inputs) {
    id <- inp$id$id
    if (is.character(id) && grepl("^[0-9]+$", id)) {
      v <- suppressWarnings(as.integer(id))
      if (!is.na(v)) {
        out <- c(out, v)
      }
    }
  }
  node <- func[["buf"]]$stack
  while (!is.null(node)) {
    for (cf in node[[1L]][[2L]]$funcs) {
      out <- c(out, collect_named_numeric(cf))
    }
    node <- node[[2L]]
  }
  out
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
  register_func_in_module(func)
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
  register_func_in_module(func)

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
#' @return A [`Func`] object.
#' @export
Func <- function(
  id = FuncId(),
  inputs = FuncInputs(),
  outputs = FuncOutputs()
) {
  if (is.character(id)) {
    id <- FuncId(id)
  }
  checkmate::assert_class(id, "FuncId")
  checkmate::assert_class(inputs, "FuncInputs")
  checkmate::assert_class(outputs, "FuncOutputs")

  # Use an environment for reference semantics.
  # Ops are stored as deferred (render, payload) pairs and rendered at repr
  # time (see func_emit()/func_lines()).
  env <- new.env(parent = emptyenv())
  env$id <- id
  env$inputs <- inputs
  env$outputs <- outputs
  buf <- new.env(parent = emptyenv())
  buf$n <- 0L
  buf$stack <- NULL
  env$buf <- buf

  # Return the environment directly with Func class
  class(env) <- c("Func", "environment")
  env
}

#' @export
repr.Func <- function(x, ...) {
  # Func ::= 'func' '.' 'func' FuncId FuncInputs FuncOutputs '{' Op* '}'
  # Install a fresh value-id numbering scope unless one is already active
  # (region funcs rendered within, or an enclosing print.FuncValue, share it).
  if (is.null(globals[["REPR_IDS"]])) {
    push_repr_ids(collect_named_numeric(x))
    on.exit(pop_repr_ids(NULL), add = TRUE)
  }
  paste0(
    "func.func ",
    repr(x$id),
    " ",
    repr(x$inputs),
    " ",
    repr(x$outputs),
    " {\n",
    paste0(func_lines(x), collapse = "\n"),
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
#' @param lines (`character()`)\cr
#'   The rendered op lines of the function body.
#' @return (`OpInputFunc`)
#' @export
OpInputFunc <- function(inputs, lines) {
  # Inside a region, the trailing func-level `return` becomes
  # `stablehlo.return`.
  n <- length(lines)
  if (n > 0L) {
    lines[[n]] <- sub("^return ", "stablehlo.return ", lines[[n]])
  }

  structure(
    list(inputs = inputs, lines = lines),
    class = "OpInputFunc"
  )
}

#' @export
repr.OpInputFunc <- function(x, ...) {
  body_str <- paste0(x$lines, collapse = "\n    ")
  # Don't print parameters if there are none:
  if (length(x$inputs) == 0) {
    return(paste0("{\n", body_str, "\n}"))
  }
  paste0(
    "{\n  ^bb0",
    repr(x$inputs),
    ":\n    ",
    body_str,
    "\n}"
  )
}

#' @export
`==.OpInputFunc` <- function(e1, e2) {
  e1$inputs == e2$inputs && identical(e1$lines, e2$lines)
}
