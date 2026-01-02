#' @importFrom cli format_error
NULL

throw_error <- function(c, call = NULL) {
  call <- call %??% c@call
  if (is.null(call)) {
    call <- sys.call(-1)
  }
  rlang::abort(conditionMessage(c), call = call, condition = c)
}

#' Convert PascalCase to snake_case
#' @param str Character string in PascalCase
#' @return Character string in snake_case
pascal_to_snake <- function(str) {
  # Insert underscore before uppercase letters (except the first one)
  # Then convert to lowercase
  gsub("([a-z])([A-Z])", "\\1_\\2", str) |>
    tolower()
}

#' Factory function to create error helper functions
#' @param error_class The S7 error class
#' @return A function that creates and throws the error
make_error_function <- function(error_class) {
  function(..., call = sys.call(-1)) {
    throw_error(
      error_class(...),
      call = call
    )
  }
}

S3_error_condition <- S7::new_S3_class(
  class = c("error", "condition"),
  constructor = function(.data = list(), message = "", call = NULL) {
    if (!is.list(.data)) {
      .data <- as.list(.data)
    }

    # Store fields as LIST ELEMENTS so base condition tools work
    .data$message <- as.character(message)[1]
    .data$call <- call

    structure(.data, class = c("error", "condition"))
  }
)

#' @title StablehloError
#' @description Base error class for all stablehlo errors
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param .data (`list()`)\cr Additional data to store in the condition
#' @export
StablehloError <- S7::new_class(
  "StablehloError",
  parent = S3_error_condition,
  properties = list(
    message = new_property(
      getter = function(self) self$message,
      setter = function(self, value) {
        self$message <- value
      }
    ),
    call = new_property(
      getter = function(self) self$call,
      setter = function(self, value) {
        self$call <- value
      }
    )
  ),
  constructor = function(message = character(), call = NULL, .data = list()) {
    base <- S3_error_condition$constructor(
      .data = .data,
      message = message,
      call = call
    )
    S7::new_object(base)
  }
)

method(conditionMessage, StablehloError) <- function(c, ...) {
  c@message
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

invalid_identifier_error <- make_error_function(InvalidIdentifierError)

method(conditionMessage, InvalidIdentifierError) <- function(c, ...) {
  format_error(c(
    "Invalid identifier: {.var {c@arg}}.",
    i = "Identifiers must start with a letter and contain only letters, numbers, and underscores."
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

unequal_tensor_types_error <- make_error_function(UnequalTensorTypesError)

method(conditionMessage, UnequalTensorTypesError) <- function(c, ...) {
  nms <- names(c@args)
  types <- paste0(
    vapply(seq_along(c@args), FUN.VALUE = character(1), function(i) {
      paste0(nms[i], " = ", repr(c@args[[i]]))
    }),
    collapse = ", "
  )
  format_error(c(
    "All arguments must have the same tensor type.",
    i = "Got: {types}."
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

class_error <- make_error_function(ClassError)

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

tensor_dtype_error <- make_error_function(TensorDTypeError)

method(conditionMessage, TensorDTypeError) <- function(c, ...) {
  format_error(c(
    "Expected {.var {c@arg}} to have dtype {.or {c@expected}}.",
    i = "Got {.cls {c@observed}}."
  ))
}

TensorNDimsError <- S7::new_class(
  "TensorNDimsError",
  parent = TensorError,
  properties = list(
    expected = new_property(
      class_integer,
      setter = function(self, value) {
        value <- as.integer(value)
        if (length(value) != 2L) {
          cli::cli_abort("expected must be a length-2 integer vector")
        }
        self@expected <- value
        self
      }
    ),
    observed = new_property(class_integer)
  )
)

tensor_ndims_error <- make_error_function(TensorNDimsError)

method(conditionMessage, TensorNDimsError) <- function(c, ...) {
  expected_range <- c@expected
  lower <- expected_range[1L]
  upper <- expected_range[2L]

  # Format the range string
  if (is.na(lower) && is.na(upper)) {
    range_str <- "any number of dimensions" # this should never happen
  } else if (is.na(lower)) {
    range_str <- paste0("less than ", upper, " dimensions")
  } else if (is.na(upper)) {
    range_str <- paste0("at least ", lower, " dimensions")
  } else if (lower == upper - 1L) {
    range_str <- paste0(
      "exactly ",
      lower,
      " dimension",
      if (lower != 1L) "s" else ""
    )
  } else {
    range_str <- paste0(
      "between ",
      lower,
      " and ",
      upper - 1L,
      " dimensions (inclusive)"
    )
  }

  format_error(c(
    "{.var {c@arg}} must have {range_str}.",
    i = "Got {c@observed} dimension{?s}."
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

tensor_shape_error <- make_error_function(TensorShapeError)

shapevec_repr <- function(shape) {
  sprintf("(%s)", paste0(shape, collapse = ","))
}

method(conditionMessage, TensorShapeError) <- function(c, ...) {
  format_error(c(
    "{.var {c@arg}} must have shape {shapevec_repr(c@expected)}.",
    i = "Got shape {shapevec_repr(c@observed)}."
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

shape_mismatch_error <- make_error_function(ShapeMismatchError)

# TODO: Continue here and test this in test-op-dot_general.R
method(conditionMessage, ShapeMismatchError) <- function(c, ...) {
  format_error(c(
    "Dimension {c@dim_lhs} of {.var {c@arg_lhs}} must match dimension {c@dim_rhs} of {.var {c@arg_rhs}}.",
    i = "{.var {c@arg_lhs}} has size {c@size_lhs} at dimension {c@dim_lhs}, but {.var {c@arg_rhs}} has size {c@size_rhs} at dimension {c@dim_rhs}."
  ))
}

#' @title DimensionOutOfRangeError
#' @description Error when a dimension index is outside the valid range [0, ndims)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimension (`integer()`)\cr The dimension index(es) that are out of range (0-based)
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @inheritParams StablehloError
#' @export
DimensionOutOfRangeError <- S7::new_class(
  "DimensionOutOfRangeError",
  parent = ArgumentError,
  properties = list(
    dimension = new_property(class_integer, setter = function(self, value) {
      self@dimension <- as.integer(value)
      self
    }),
    ndims = new_property(class_integer, setter = function(self, value) {
      self@ndims <- as.integer(value)
      self
    })
  )
)

dimension_out_of_range_error <- make_error_function(DimensionOutOfRangeError)

method(conditionMessage, DimensionOutOfRangeError) <- function(c, ...) {
  dims_str <- if (length(c@dimension) == 1L) {
    paste0("dimension index ", c@dimension)
  } else {
    paste0("dimension indices: ", paste0(c@dimension, collapse = ", "))
  }
  format_error(c(
    "{.var {c@arg}} contains invalid dimension index{?es}.",
    i = "Got {dims_str}, but valid range is [0, {c@ndims})."
  ))
}

#' @title DimensionUniquenessError
#' @description Error when dimension indices are not unique
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimensions (`integer()`)\cr The dimension indices that are not unique (0-based)
#' @inheritParams StablehloError
#' @export
DimensionUniquenessError <- S7::new_class(
  "DimensionUniquenessError",
  parent = ArgumentError,
  properties = list(
    dimensions = new_property(class_integer, setter = function(self, value) {
      self@dimensions <- as.integer(value)
      self
    })
  )
)

dimension_uniqueness_error <- make_error_function(DimensionUniquenessError)

method(conditionMessage, DimensionUniquenessError) <- function(c, ...) {
  dims_str <- paste0(c@dimensions, collapse = ", ")
  format_error(c(
    "{.var {c@arg}} contains duplicate dimension indices.",
    i = "Got [{dims_str}]. Each dimension index must appear only once."
  ))
}

#' @title IndexOutOfBoundsError
#' @description Error when an index is outside the valid range [lower, upper)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
#' @inheritParams StablehloError
#' @export
IndexOutOfBoundsError <- S7::new_class(
  "IndexOutOfBoundsError",
  parent = ArgumentError,
  properties = list(
    lower = new_property(class_integer, setter = function(self, value) {
      self@lower <- as.integer(value)
      self
    }),
    upper = new_property(class_integer, setter = function(self, value) {
      self@upper <- as.integer(value)
      self
    })
  )
)

index_out_of_bounds_error <- make_error_function(IndexOutOfBoundsError)

method(conditionMessage, IndexOutOfBoundsError) <- function(c, ...) {
  format_error(c(
    "{.var {c@arg}} contains index{?es} outside the valid range.",
    i = "Valid range is [{c@lower}, {c@upper})."
  ))
}

#' @title Convert 0-based indices to 1-based
#' @description Generic function to convert 0-based indices in error conditions to 1-based
#' @param x Condition object with indices
#' @param ... Additional arguments (not used)
#' @return Condition object with indices converted to 1-based
#' @export
to_one_based <- S7::new_generic("to_one_based", "x")

method(to_one_based, DimensionOutOfRangeError) <- function(x) {
  x@dimension <- x@dimension + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}

method(to_one_based, DimensionUniquenessError) <- function(x) {
  x@dimensions <- x@dimensions + 1L
  x
}

method(to_one_based, IndexOutOfBoundsError) <- function(x) {
  x@lower <- x@lower + 1L
  x@upper <- x@upper + 1L
  x
}

method(to_one_based, ShapeMismatchError) <- function(x) {
  x@dim_lhs <- x@dim_lhs + 1L
  x@dim_rhs <- x@dim_rhs + 1L
  x
}

#' @title SliceIndexError
#' @description Error when slice indices (start_indices, limit_indices) are invalid
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param indices (`integer()`)\cr The invalid indices (0-based)
#' @param index_type (`character(1)`)\cr Type of index: "start" or "limit"
#' @inheritParams StablehloError
#' @export
SliceIndexError <- S7::new_class(
  "SliceIndexError",
  parent = ArgumentError,
  properties = list(
    indices = new_property(class_integer, setter = function(self, value) {
      self@indices <- as.integer(value)
      self
    }),
    index_type = new_property(class_character) # "start" or "limit"
  )
)

slice_index_error <- make_error_function(SliceIndexError)

method(conditionMessage, SliceIndexError) <- function(c, ...) {
  indices_str <- if (length(c@indices) == 1L) {
    paste0("index ", c@indices)
  } else {
    paste0("indices: ", paste0(c@indices, collapse = ", "))
  }
  index_type_label <- if (c@index_type == "start") "start" else "limit"
  format_error(c(
    "{.var {c@arg}} contains invalid {index_type_label} {if (length(c@indices) == 1L) 'index' else 'indices'}.",
    i = "Got {indices_str}."
  ))
}

method(to_one_based, SliceIndexError) <- function(x) {
  x@indices <- x@indices + 1L
  x
}

#' @title PermutationError
#' @description Error when permutation values are invalid (not a valid permutation of dimension indices)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param permutation (`integer()`)\cr The permutation values that are invalid (0-based)
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @inheritParams StablehloError
#' @export
PermutationError <- S7::new_class(
  "PermutationError",
  parent = ArgumentError,
  properties = list(
    permutation = new_property(class_integer, setter = function(self, value) {
      self@permutation <- as.integer(value)
      self
    }),
    ndims = new_property(class_integer, setter = function(self, value) {
      self@ndims <- as.integer(value)
      self
    })
  )
)

permutation_error <- make_error_function(PermutationError)

method(conditionMessage, PermutationError) <- function(c, ...) {
  perm_str <- paste0(c@permutation, collapse = ", ")
  if (c@ndims == 0L) {
    expected_str <- "(empty)"
  } else {
    expected_str <- paste0(seq(0, c@ndims - 1), collapse = ", ")
  }
  format_error(c(
    "{.var {c@arg}} must be a permutation of [0, 1, ..., {c@ndims - 1}].",
    i = "Got [{perm_str}], but expected a permutation of [{expected_str}]."
  ))
}

method(to_one_based, PermutationError) <- function(x) {
  x@permutation <- x@permutation + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}
