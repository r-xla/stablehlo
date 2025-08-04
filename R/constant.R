#' @include types.R
#' @include repr.R
NULL

TensorConstant <- new_class(
  "TensorConstant",
  properties = list(
    data = S7::class_any,
    type = TensorType
  ),
  constructor = function(data, type) {
    if (anyNA(data)) {
      stop("NA values are not supported in constants.")
    }
    if (!is.array(data) && length(data) != 1) {
      stop("data must ber an array or a vector of length 1.")
    }
    if (is.logical(data) & !inherits(type@dtype@type, BooleanType)) {
      stop("For logical data, the type must be BooleanType.")
    }
    if (is.double(data) & !inherits(type@dtype@type, FloatType)) {
      stop("For numeric data, the type must be FloatType.")
    }
    if (is.integer(data) & !inherits(type@dtype@type, IntegerType)) {
      stop("For integer data, the type must be IntegerType.")
    }
    if (is.integer(data) && startsWith(type@dtype@type@Value, "ui")) {
      if (!all(data >= 0)) {
        stop("Unsigned integer data must be non-negative.")
      }
    }
    S7::new_object(TensorConstant, data = data, type = type)
  }
)

method(repr, TensorConstant) <- function(x) {
  data <- x@data
  type <- x@type

  value_reprs <- if (inherits(type@dtype@type, FloatType)) {
    format_double(
      data,
      precision = if (type@dtype@type@Value == "f32") 32 else 64
    )
  } else if (inherits(type@dtype@type, IntegerType)) {
    as.character(data)
  } else if (inherits(type@dtype@type, BooleanType)) {
    tolower(as.logical(data))
  }
  if (is.array(data)) {
    dim(value_reprs) <- dim(data)
  }

  f <- function(x) {
    if (length(dim(x)) == 1L & length(x) == 1L) return(x)
    if (length(dim(x)) == 1L || is.vector(x)) {
      paste0("[", paste(x, collapse = ", "), "]")
    } else {
      paste0("[", paste(apply(x, 1, f), collapse = ", "), "]")
    }
  }

  paste0("dense<", f(value_reprs), "> : ", repr(type))
}

Constant <- new_class(
  "Constant",
  properties = list(
    value = S7::new_union(
      # It's not clear to me, why we need the other constant types, I thought
      # stableHLO only has tensors, so we distinguishbetween BooleanConstant and TensorConstant with
      # element type BooleanType?
      TensorConstant
    )
  )
)

method(repr, Constant) <- function(x) {
  repr(x@value)
}

# Create a Constant from R value
r_to_constant <- S7::new_generic(
  "r_to_constant",
  "value",
  function(value, elt_type = NULL, ...) {
    if (!is.array(value) && length(value) != 1L) {
      stop("Either provide an R array or a length 1 vector.")
    }
    S7::S7_dispatch()
  }
)

method(r_to_constant, S7::class_logical) <- function(
  value,
  elt_type = NULL, # is ignored
  ...
) {
  shape <- Shape(
    if (is.array(value)) dim(value) else integer()
  )

  stablehlo_type <- BooleanType()
  element_type_obj <- TensorElementType(type = stablehlo_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_numeric) <- function(
  value,
  elt_type = NULL,
  ...
) {
  elt_type <- if (is.null(elt_type)) {
    FloatType("f32")
  } else {
    string_to_type(elt_type)
  }

  shape <- Shape(
    if (is.array(value)) dim(value) else integer()
  )

  element_type_obj <- TensorElementType(type = elt_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_integer) <- function(
  value,
  elt_type = NULL,
  ...
) {
  # For integer values, create a tensor constant
  # Use provided element_type or default to i32
  elt_type <- if (is.null(elt_type)) {
    IntegerType("i32")
  } else {
    string_to_type(elt_type)
  }

  shape <- Shape(
    if (is.array(value)) dim(value) else integer()
  )

  element_type_obj <- TensorElementType(type = elt_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_any) <- function(
  value,
  elt_type = NULL,
  ...
) {
  stop("Unsupported type for r_to_constant: ", class(value)[1])
}
