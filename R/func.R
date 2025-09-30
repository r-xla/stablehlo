#' @include list_of.R
#' @include types.R
#' @include repr.R
#' @include value_id.R
#' @importFrom S7 new_class new_property method
NULL

FuncInput <- new_class(
  "FuncInput",
  properties = list(
    id = ValueId,
    type = ValueType
  )
)

method(repr, FuncInput) <- function(x) {
  paste0(
    repr(x@id),
    ": ",
    repr(x@type)
  )
}

FuncInputs <- new_list_of("FuncInputs", FuncInput)

method(repr, FuncInputs) <- function(x) {
  paste0(
    "(",
    paste0(sapply(x@items, repr), collapse = ", "),
    ")"
  )
}

FuncOutput <- new_class(
  "FuncOutput",
  properties = list(
    type = ValueType
  )
)

method(repr, FuncOutput) <- function(x) {
  repr(x@type)
}

FuncOutputs <- new_list_of("FuncOutputs", FuncOutput)

method(repr, FuncOutputs) <- function(x) {
  if (length(x@items) <= 1L) {
    paste0(
      "-> ",
      paste0(sapply(x@items, repr), collapse = ", ")
    )
  } else {
    paste0(
      "-> (",
      paste0(sapply(x@items, repr), collapse = ", "),
      ")"
    )
  }
}

#' @title FuncId
#' @description
#' This represents the id of a function.
#' @param id The id of the function.
#' @export
FuncId <- new_class(
  "FuncId",
  properties = list(
    id = S7::class_character
  )
)

method(repr, FuncId) <- function(x) {
  paste0("@", x@id)
}

method(`==`, list(FuncId, FuncId)) <- function(e1, e2) {
  identical(e1@id, e2@id)
}

FuncBody <- new_list_of(
  "FuncBody",
  item_type = Op
)

method(repr, FuncBody) <- function(x) {
  paste0(sapply(x@items, repr), collapse = "\n")
}

#' @title Get the last function created
#' @description
#' Get the last function created (either via [`hlo_func`] or [`local_func`]),
#' which is not returned yet.
#' @return A [`Func`] object.
#' @export
.current_func <- function() {
  globals[["CURRENT_FUNC"]] %??% stop("No function is currently being built")
}

#' @title Create a function
#' @description
#' Both functions create a new [`Func`] with the given id which is afterwards affessible via [`.current_func()`].
#' Functions receiving a [`Func`] as an argument (such as [`hlo_input`], [`hlo_add`], ...) usually use
#' [`.current_func()`] by default.
#'
#' Differences between the two functions:
#' * [`local_func`] removes the function when exiting the current scope, whereas [`hlo_func`] does not.
#' * [`hlo_func`] discards the previously built function(s), whereas [`local_func`] does not:
#'   after a function created by [`local_func`] is either cleaned up automatically (by exiting the scope) or the function
#'   is finalized via [`hlo_return`], the previously built function is restored, i.e., accessible via [`.current_func()`].
#'   To build nested functions (e.g. to create a closure that is passed to another op), use
#'   [`local_func`] instead of [`hlo_func`].
#'
#' To
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
#' @export
local_func <- function(id = "main") {
  func <- hlo_func(id)
  if (!is.null(globals[["CURRENT_FUNC"]])) {
    globals[["FUNC_STASH"]] <- c(globals[["FUNC_STASH"]], list(func))
  }
  globals[["CURRENT_FUNC"]] <- func

  withr::defer(envir = parent.frame(), {
    restore_previous_func()
  })
  return(func)
}

#' @title Func
#' @description
#' This represents a function.
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
Func <- new_class(
  "Func",
  properties = list(
    id = new_property(
      FuncId,
      getter = function(self) self@.env[["id"]],
      setter = function(self, value) {
        self@.env[["id"]] <- value
        self
      }
    ),
    inputs = new_property(
      FuncInputs,
      getter = function(self) self@.env[["inputs"]],
      setter = function(self, value) {
        self@.env[["inputs"]] <- value
        self
      }
    ),
    outputs = new_property(
      FuncOutputs,
      getter = function(self) self@.env[["outputs"]],
      setter = function(self, value) {
        self@.env[["outputs"]] <- value
        self
      }
    ),
    body = new_property(
      FuncBody,
      getter = function(self) self@.env[["body"]],
      setter = function(self, value) {
        self@.env[["body"]] <- value
        self
      }
    ),
    .env = S7::new_S3_class("hashtab")
  ),
  constructor = function(
    id = FuncId(),
    inputs = FuncInputs(),
    outputs = FuncOutputs(),
    body = FuncBody()
  ) {
    env <- hashtab()
    env[["id"]] <- id
    env[["inputs"]] <- inputs
    env[["outputs"]] <- outputs
    env[["body"]] <- body
    new_object(
      S7::S7_object(),
      .env = env
    )
  }
)

method(repr, Func) <- function(x) {
  # Func        ::= 'func' '.' 'func' FuncId FuncInputs FuncOutputs '{' FuncBody '}'
  paste0(
    "func.func ",
    repr(x@id),
    " ",
    repr(x@inputs),
    " ",
    repr(x@outputs),
    " {\n",
    repr(x@body),
    "\n}\n"
  )
}

method(print, Func) <- function(x, ...) {
  cat(repr(x))
}

OpInputFunc <- new_class(
  "OpInputFunc",
  properties = list(
    inputs = FuncInputs,
    body = FuncBody
  )
)

method(repr, OpInputFunc) <- function(x) {
  # Don't print parameters if there are none:
  if (length(x@inputs@items) == 0) {
    return(
      paste0(
        "{\n",
        paste0(
          sapply(x@body@items, repr, toplevel = FALSE),
          collapse = "\n    "
        ),
        "\n}"
      )
    )
  }
  paste0(
    "{\n  ^bb0",
    repr(x@inputs),
    ":\n    ",
    paste0(sapply(x@body@items, repr, toplevel = FALSE), collapse = "\n    "),
    "\n}"
  )
}

method(`==`, list(OpInputFunc, OpInputFunc)) <- function(e1, e2) {
  e1@inputs == e2@inputs && e1@body == e2@body
}

OpInputFuncs <- new_list_of("OpInputFuncs", OpInputFunc)

method(repr, OpInputFuncs) <- function(x) {
  if (length(x@items) == 0) {
    return("")
  }

  paste0(
    "(",
    paste0(sapply(x@items, repr), collapse = ", "),
    ")"
  )
}
