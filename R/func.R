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

Func <- new_class(
  "Func",
  properties = list(
    id = FuncId,
    inputs = FuncInputs,
    outputs = FuncOutputs,
    body = FuncBody
  )
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
