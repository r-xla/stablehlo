#' @importFrom cli format_error
NULL

throw_error <- function(c, call = NULL) {
  call <- call %??% c@call
  if (is.null(call)) {
    call <- sys.call(-1)
  }
  rlang::abort(conditionMessage(c), call = call, condition = c)
}

S3_error_condition <- S7::new_S3_class(
  class = c("error", "condition"),
  constructor = function(.data = list(), message = "", call = NULL) {
    if (!is.list(.data)) .data <- as.list(.data)

    # Store fields as LIST ELEMENTS so base condition tools work
    .data$message <- as.character(message)[1]
    .data$call <- call

    structure(.data, class = c("error", "condition"))
  }
)

#' @export
StablehloError <- S7::new_class(
  "StablehloError",
  parent = S3_error_condition,
  properties = list(
    message = new_property(getter = function(self) self$message, setter = function(self, value) {
      self$message <- value
    }),
    call = new_property(getter = function(self) self$call, setter = function(self, value) {
      self$call <- value
    })
  ),
  constructor = function(message = character(), call = NULL, .data = list()) {
    base <- S3_error_condition$constructor(.data = .data, message = message, call = call)
    S7::new_object(base)
  }
)

method(conditionMessage, StablehloError) <- function(x, ...) {
  x@message
}

#' @title InferenceError
#' @description Base class for type inference errors
#' @inheritParams StablehloError
#' @export
InferenceError <- S7::new_class(
  "InferenceError",
  parent = StablehloError
)

# When a single argument is invalid
ArgumentError <- S7::new_class(
  "InvalidArgumentError",
  parent = StablehloError,
  properties = list(
    arg = new_property(class_character)
  )
)

InvalidIdentifierError <- S7::new_class(
  "InvalidIdentifierError",
  parent = ArgumentError
)

invalid_identifier_error <- function(..., call = sys.call(-1)) {
  throw_error(InvalidIdentifierError(
    ...
  ), call = call)
}

method(conditionMessage, InvalidIdentifierError) <- function(c, ...) {
  format_error(c(
    "Identifiers must start with a letter and contain only letters, numbers, and underscores.",
    i = "Got {c@arg}."
  ))
}

# When two tensors are expected to have the same type, but don't
#' @include types.R
UnequalTensorTypesError <- S7::new_class(
  "UnequalTypesError",
  parent = InferenceError,
  properties = list(
    # named
    args = list_of(TensorType)
  )
)

unequal_tensor_types_error <- function(..., call = sys.call(-1)) {
  throw_error(UnequalTensorTypesError(
    ...
  ), call = call)
}

method(conditionMessage, UnequalTensorTypesError) <- function(c, ...) {
  nms <- names(c@args)
  types <- paste0(vapply(seq_along(c@args), FUN.VALUE = character(1), function(i) {
      paste0(nms[i], "=", repr(c@args[[i]]))
  }), collapse = ", ")
  format_error(c(
    "Expected all arguments to have the same tensor type.",
    i = "Got {types}."
  ))
}

ClassError <- S7::new_class(
  "ClassError",
  parent = ArgumentError,
  properties = list(
    # character vector of class names
    expected = class_character,
    observed = class_character
  )
)

class_error <- function(..., call = sys.call(-1)) {
  throw_error(ClassError(
    ...
  ), call = call)
}

method(conditionMessage, ClassError) <- function(c, ...) {
  format_error(c(
    "Expected {.var {c@arg}} to have class {.or {c@expected}}.",
    i = "Got {.cls {c@observed}}."
  ))
}


TensorError <- S7::new_class(
  "TensorError",
  parent = ArgumentError
)

TensorDTypeError <- S7::new_class(
  "TensorDTypeError",
  parent = ClassError,
  properties = list(
    expected = class_character,
    observed = class_character
  )
)

tensor_dtype_error <- function(..., call = sys.call(-1)) {
  throw_error(TensorDTypeError(
    ...
  ), call = call)
}

method(conditionMessage, TensorDTypeError) <- function(c, ...) {
  format_error(c(
    "Expected {.var {c@arg}} to have dtype {.or {c@expected}}.",
    i = "Got {.cls {c@observed}}."
  ))
}

TensorShapeError <- S7::new_class(
  "TensorShapeError",
  parent = TensorError,
  properties = list(
    expected = new_property(class_integer, setter = function(self, value) {
      self@expected <- as.integer(value)
      self
    }),
    observed = new_property(class_integer, setter = function(self, value) {
      self@observed <- as.integer(value)
      self
    })
  )
)

tensor_shape_error <- function(..., call = sys.call(-1)) {
  throw_error(TensorShapeError(
    ...
  ), call = call)
}

shapevec_repr <- function(shape) {
  sprintf("(%s)", paste0(shape, collapse = ","))
}

method(conditionMessage, TensorShapeError) <- function(c, ...) {
  format_error(c(
    "Expected {.var {c@arg}} to have shape {shapevec_repr(c@expected)}.",
    i = "Got {shapevec_repr(c@observed)}."
  ))
}

ShapeMismatchError <- S7::new_class(
  "ShapeMismatchError",
  parent = InferenceError,
  properties = list(
    arg_lhs = new_property(class_character),
    arg_rhs = new_property(class_character),
    dim_lhs = new_property(class_integer, setter = function(self, value) {
      self@dim_lhs <- as.integer(value)
      self
    }),
    dim_rhs = new_property(class_integer, setter = function(self, value) {
      self@dim_rhs <- as.integer(value)
      self
    }),
    size_lhs = new_property(class_integer, setter = function(self, value) {
      self@size_lhs <- as.integer(value)
      self
    }),
    size_rhs = new_property(class_integer, setter = function(self, value) {
      self@size_rhs <- as.integer(value)
      self
    })
  )
)

shape_mismatch_error <- function(..., call = sys.call(-1)) {
  throw_error(ShapeMismatchError(
    ...
  ), call = call)
}

# TODO: Continue here and test this in test-op-dot_general.R
method(conditionMessage, ShapeMismatchError) <- function(c, ...)
  format_error(c(
    "Dimension {c@dim_lhs} of {.var {c@arg_lhs}} must match dimension {c@dim_rhs} of {.var {c@arg_rhs}}.",
    i = "Got {.var {c@arg_lhs}}[{c@dim_lhs}] = {c@size_lhs} and {.var {c@arg_rhs}}[{c@dim_rhs}] = {c@size_rhs}."
  ))
}
