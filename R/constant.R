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

  if (simplify_dense && length(dim(data)) <= 1L) {
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
      "dense<[]> : ",
      repr(type)
    ))
  }

  if (!is.array(data)) {
    return(paste0(
      "dense<",
      paste(value_reprs, collapse = ", "),
      "> : ",
      repr(type)
    ))
  }
  dim2 <- function(data) {
    if (is.array(data)) {
      dim(data)
    } else {
      length(data)
    }
  }
  dim(value_reprs) <- dim2(data)

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
  if (!is.null(dtype) && !(dtype %in% c("f32", "f64"))) {
    cli_abort("Invalid dtype for double")
  }
  dtype <- if (is.null(dtype)) {
    FloatType(32L)
  } else {
    as_dtype(dtype)
  }

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
  valid_types <- c("i8", "i16", "i32", "i64", "ui8", "ui16", "ui32", "ui64")
  if (!is.null(dtype) && !(dtype %in% valid_types)) {
    cli_abort("Invalid dtype for integer")
  }
  dtype <- if (is.null(dtype)) {
    IntegerType(32L)
  } else {
    as_dtype(dtype)
  }

  shape <- Shape(shape)

  tensor_type <- TensorType(dtype, shape)

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
