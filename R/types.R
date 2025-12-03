#' @include enum.R
#' @include shape.R
#' @include list_of.R
NULL

#' @title BooleanType
#' @description
#' Represents the boolean type.
#' @return `BooleanType`
#' @export
BooleanType <- new_class("BooleanType")
method(repr, BooleanType) <- function(x) {
  "i1"
}

#' @title IntegerType (signed)
#' @description
#' Represents a signed integer type with a given bit width.
#' @param value (`integer(1)`)
#' @return `IntegerType`
#' @export
IntegerType <- new_class(
  "IntegerType",
  properties = list(
    value = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        assert_int(value)
        if (!(value %in% c(8L, 16L, 32L, 64L))) {
          cli_abort("Unsupported signed integer bit width: {value}")
        }
      }
    )
  ),
  constructor = function(value) {
    new_object(S7::S7_object(), value = as.integer(value))
  }
)

method(repr, IntegerType) <- function(x) {
  paste0("i", x@value)
}

#' @title UnsignedType
#' @description
#' Represents an unsigned integer type with a given bit width.
#' @param value (`integer(1)`)
#' @return `UnsignedType`
#' @export
UnsignedType <- new_class(
  "UnsignedType",
  properties = list(
    value = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        assert_int(value)
        if (!(value %in% c(8L, 16L, 32L, 64L))) {
          cli_abort("Unsupported unsigned integer bit width: {value}")
        }
      }
    )
  ),
  constructor = function(value) {
    new_object(S7::S7_object(), value = as.integer(value))
  }
)

method(repr, UnsignedType) <- function(x) {
  paste0("ui", x@value)
}

#' @title FloatType
#' @description
#' Represents a floating point type with a given bit width.
#' @param value (`integer(1)`)
#' @return `FloatType`
#' @export
FloatType <- new_class(
  "FloatType",
  properties = list(
    value = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        assert_int(value)
        if (!(value %in% c(32L, 64L))) {
          cli_abort("Unsupported float bit width: {value}")
        }
      }
    )
  ),
  constructor = function(value) {
    new_object(S7::S7_object(), value = as.integer(value))
  }
)

method(repr, FloatType) <- function(x) {
  paste0("f", x@value)
}

#' @title TensorDataType
#' @description
#' Type union of all possible data types.
#' @export
TensorDataType <- S7::new_union(
  BooleanType,
  IntegerType,
  UnsignedType,
  FloatType
)

#' @title Is TensorDataType
#' @description
#' Check if an object is a [`TensorDataType`].
#' @param x (any)\cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is_dtype <- function(x) {
  if (inherits(x, "S7_object")) {
    S7::S7_inherits(x, IntegerType) ||
      S7::S7_inherits(x, UnsignedType) ||
      S7::S7_inherits(x, FloatType) ||
      S7::S7_inherits(x, BooleanType)
  } else {
    FALSE
  }
}

method(`==`, list(TensorDataType, TensorDataType)) <- function(e1, e2) {
  if (!identical(class(e1), class(e2))) {
    return(FALSE)
  }
  if (inherits(e1, BooleanType)) {
    return(TRUE)
  }
  if (inherits(e1, IntegerType)) {
    return(e1@value == e2@value)
  }
  if (inherits(e1, UnsignedType)) {
    return(e1@value == e2@value)
  }
  if (inherits(e1, FloatType)) {
    return(e1@value == e2@value)
  }
  cli_abort("Unknown TensorDataType")
}

#' @title Convert to TensorDataType
#' @description
#' Convert to TensorDataType.
#' @param x (any)\cr
#'   Object to convert.
#'   Can currently be a string (one of `r roxy_dtypes()`) or a [`TensorDataType`] object.
#' @return `TensorDataType`
#' @export
as_dtype <- S7::new_generic("as_dtype", "x", function(x) {
  S7::S7_dispatch()
})

method(as.character, BooleanType) <- function(x, ...) {
  "i1"
}

method(as.character, IntegerType) <- function(x, ...) {
  repr(x)
}

method(as.character, UnsignedType) <- function(x, ...) {
  repr(x)
}

method(as.character, FloatType) <- function(x, ...) {
  repr(x)
}

method(as_dtype, class_character) <- function(x) {
  if (x %in% c("pred", "i1")) {
    return(BooleanType())
  }
  if (grepl("^i[0-9]+$", x)) {
    return(IntegerType(as.integer(sub("^i", "", x))))
  }
  if (grepl("^ui[0-9]+$", x)) {
    return(UnsignedType(as.integer(sub("^ui", "", x))))
  }
  if (grepl("^f[0-9]+$", x)) {
    return(FloatType(as.integer(sub("^f", "", x))))
  }
  cli_abort("Unsupported dtype: ", x)
}

method(as_dtype, TensorDataType) <- function(x) {
  x
}

method(`!=`, list(TensorDataType, TensorDataType)) <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @title TensorType
#' @description
#' Represents a tensor type with a specific data type and shape.
#' @param dtype ([`TensorDataType`])
#' @param shape ([`Shape`])
#' @return `TensorType`
#' @export
TensorType <- new_class(
  "TensorType",
  properties = list(
    dtype = TensorDataType,
    shape = Shape
  )
)

method(`==`, list(TensorType, TensorType)) <- function(e1, e2) {
  e1@dtype == e2@dtype && e1@shape == e2@shape
}

method(`!=`, list(TensorType, TensorType)) <- function(e1, e2) {
  !(e1 == e2) # nolint
}

method(repr, TensorType) <- function(x) {
  paste0(
    "tensor<",
    repr(x@shape),
    if (length(x@shape@dims) > 0) "x" else "",
    repr(x@dtype),
    ">"
  )
}

TokenType <- new_class("TokenType")

method(repr, TokenType) <- function(x) {
  "!stablehlo.token"
}

#' @title ValueType
#' @description
#' This represents the type of a value.
#' @param type The type of the value.
#' @param shape The shape of the value.
#' @export
ValueType <- new_class(
  "ValueType",
  properties = list(
    type = S7::new_union(
      # Not supported:]
      # - QuantizedTensorType
      # - TupleType (will probably be removed from stablehlo, is only legacy from xla)
      TokenType,
      TensorType
    )
  ),
  constructor = function(type, shape = NULL) {
    if (is.character(type)) {
      return(make_value_type(type, shape = shape))
    }
    new_object(S7::S7_object(), type = type)
  }
)


#' @export
#' @method dtype stablehlo::ValueType
`dtype.stablehlo::ValueType` <- function(x, ...) {
  if (inherits(x@type, TensorType)) {
    x@type@dtype
  } else if (inherits(x@type, TokenType)) {
    stop("ValueType with TokenType has no dtype")
  } else {
    stop("Unsupported ValueType for dtype")
  }
}

#' @export
#' @method shape stablehlo::ValueType
`shape.stablehlo::ValueType` <- function(x, ...) {
  shape(x@type)
}


#' @export
#' @method shape stablehlo::TensorType
`shape.stablehlo::TensorType` <- function(x, ...) {
  x@shape@dims
}

#' @export
#' @method dtype stablehlo::TensorType
`dtype.stablehlo::TensorType` <- function(x, ...) {
  x@dtype
}

method(`==`, list(ValueType, ValueType)) <- function(e1, e2) {
  e1@type == e2@type
}

value_type_union <- S7::new_union(
  TokenType,
  TensorType
)

make_value_type <- function(str, shape = NULL) {
  assert_string(str)
  type <- if (str == "token") {
    TokenType()
  } else {
    if (is.null(shape)) {
      shape <- integer(0)
    }
    dtype <- if (str %in% c("pred", "i1")) {
      BooleanType()
    } else if (grepl("^i[0-9]+$", str)) {
      IntegerType(as.integer(sub("^i", "", str)))
    } else if (grepl("^ui[0-9]+$", str)) {
      UnsignedType(as.integer(sub("^ui", "", str)))
    } else if (grepl("^f[0-9]+$", str)) {
      FloatType(as.integer(sub("^f", "", str)))
    } else {
      .NotYetImplemented()
    }
    TensorType(dtype, Shape(shape))
  }

  ValueType(type)
}

method(repr, ValueType) <- function(x) {
  repr(x@type)
}

#' @title ValueTypes
#' @description
#' List of [`ValueType`]s.
#' @param items (`list()` of [`ValueType`])\cr
#'   The types of the values.
#' @return `ValueTypes`
#' @export
ValueTypes <- new_list_of("ValueTypes", ValueType)
method(repr, ValueTypes) <- function(x) {
  paste0(
    sapply(x@items, repr),
    collapse = ", "
  )
}

check_types_equal <- function(lhs, rhs, ..., msg = NULL) {
  rlang::check_dots_empty()

  if (lhs == rhs) {
    return()
  }

  cli_abort(c(
    x = msg %||% "Expected types to be equal.",
    i = "Got lhs={.val {repr(lhs)}}, rhs={.val {repr(rhs)}}."
  ))
}
