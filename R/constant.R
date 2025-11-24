#' @include types.R
#' @include repr.R
NULL

#' @title TensorConstant
#' @description
#' This represents a constant value.
#' @param data (any)\cr
#'   The value of the constant.
#' @param type ([`TensorType`])\cr
#'   The type of the constant.
#' @return `TensorConstant`
#' @export
TensorConstant <- new_class(
  "TensorConstant",
  properties = list(
    # This can be anything as long as it implements r_to_constant
    # However, for static function inputs, we need to perform some checks on
    # the data, so we need a way to convert it to an R array.
    data = S7::class_any,
    type = TensorType
  )
)

method(repr, TensorConstant) <- function(x, simplify_dense = TRUE) {
  data <- x@data
  type <- x@type

  if (inherits(data, "PJRTBuffer")) {
    if (simplify_dense) {
      cli_abort("formatting PJRTBuffers in dense mode is not implemented")
    }
    value_reprs <- pjrt::format_buffer(data)

    # We rely on format_buffer to return a vector or array with correct dimensions
    # No manual reshaping needed here anymore.

    # Re-use logic from below for formatting arrays/vectors
  } else {
    value_reprs <- if (inherits(type@dtype, FloatType)) {
      format_double(
        data,
        precision = type@dtype@value
      )
    } else if (
      inherits(type@dtype, IntegerType) || inherits(type@dtype, UnsignedType)
    ) {
      as.character(data)
    } else if (inherits(type@dtype, BooleanType)) {
      tolower(as.logical(data))
    }
  }

  # The rest of the function assumes value_reprs is a character vector/array
  # of formatted values, which is exactly what we have now for both cases.

  # Also need to handle dim(data) check for simplify_dense
  # For PJRTBuffer, we use type shape
  data_dims <- if (inherits(data, "PJRTBuffer")) {
    x@type@shape@dims
  } else {
    dim(data)
  }

  if (simplify_dense && length(data_dims) <= 1L) {
    if (length(value_reprs) == 0) {
      return(paste0(
        "array<",
        repr(type@dtype),
        ">"
      ))
    } else {
      return(paste0(
        "array<",
        repr(type@dtype),
        ": ",
        paste(value_reprs, collapse = ", "),
        ">"
      ))
    }
  }

  if (length(value_reprs) == 0) {
    return(paste0(
      "dense<> : ",
      repr(type)
    ))
  }

  # Check if it's an array.
  # For PJRTBuffer, value_reprs will be an array if dims > 1
  is_arr <- if (inherits(data, "PJRTBuffer")) {
    length(data_dims) > 0
  } else {
    is.array(data)
  }

  if (!is_arr) {
    return(paste0(
      "dense<",
      paste(value_reprs, collapse = ", "),
      "> : ",
      repr(type)
    ))
  }

  dim2 <- function(d) {
    if (is.array(d)) {
      dim(d)
    } else {
      length(d)
    }
  }

  # Ensure value_reprs has correct dimensions if it came from non-PJRTBuffer
  if (!inherits(data, "PJRTBuffer")) {
    dim(value_reprs) <- dim2(data)
  }

  f <- function(x) {
    if (is.vector(x) || length(dim2(x)) == 1L) {
      paste0("[", paste(x, collapse = ", "), "]")
    } else {
      paste0("[", paste(apply(x, 1, f), collapse = ", "), "]")
    }
  }

  paste0("dense<", f(value_reprs), "> : ", repr(type))
}

#' @title Constant
#' @description
#' This represents a constant value.
#' @param value ([`TensorConstant`])\cr
#'   The value of the constant.
#' @return `Constant`
#' @export
Constant <- new_class(
  "Constant",
  properties = list(
    value = S7::new_union(
      # It's not clear to me, why we need the other constant types (FloatType) as there are no real
      # scalars
      # TODO: Simplify this
      TensorConstant
    )
  )
)

method(repr, Constant) <- function(x, simplify_dense = TRUE) {
  repr(x@value, simplify_dense = simplify_dense)
}

r_to_constant <- S7::new_generic(
  "r_to_constant",
  "value",
  function(value, dtype = NULL, shape, ...) {
    if (!is.null(dtype)) {
      dtype <- as.character(dtype)
    }
    S7::S7_dispatch()
  }
)

method(r_to_constant, S7::class_logical) <- function(
  value,
  dtype = NULL,
  shape,
  ...
) {
  if (!is.null(dtype) && !(dtype %in% c("i1", "pred"))) {
    cli_abort("Invalid dtype for logical")
  }
  shape <- Shape(shape)

  tensor_type <- TensorType(dtype = BooleanType(), shape = shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_double) <- function(
  value,
  dtype = NULL,
  shape,
  ...
) {
  dtype <- dtype %??% "f32"
  if (dtype %in% c("i8", "i16", "i32", "i64", "ui8", "ui16", "ui32", "ui64")) {
    value <- as.integer(value)
  } else if (dtype %in% c("i1", "pred")) {
    cli_abort("Invalid dtype for double")
  }
  dtype <- as_dtype(dtype)
  shape <- Shape(shape)
  tensor_type <- TensorType(dtype, shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_integer) <- function(
  value,
  dtype = NULL,
  shape,
  ...
) {
  dtype <- dtype %??% "i32"
  if (dtype %in% c("i1", "pred")) {
    cli_abort("Invalid dtype for integer")
  }
  if (dtype %in% c("f32", "f64")) {
    value <- as.double(value)
  }

  dtype <- as_dtype(dtype)

  shape <- Shape(shape)

  tensor_type <- TensorType(dtype, shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::new_S3_class("PJRTBuffer")) <- function(
  value,
  dtype = NULL,
  shape,
  ...
) {
  dtype <- dtype %??% as.character(pjrt::elt_type(value))
  shape <- Shape(shape)

  tensor_type <- TensorType(dtype = as_dtype(dtype), shape = shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_any) <- function(
  value,
  dtype = NULL,
  shape,
  ...
) {
  cli_abort("Unsupported type for r_to_constant: ", class(value)[1])
}


#' @export
#' @method shape stablehlo::TensorConstant
`shape.stablehlo::TensorConstant` <- function(x, ...) {
  x@type@shape@dims
}
