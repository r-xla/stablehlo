#' @include types.R
#' @include repr.R
NULL

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

  value_reprs <- if (inherits(type@dtype@type, FloatType)) {
    format_double(
      data,
      precision = if (type@dtype@type@value == "f32") 32 else 64
    )
  } else if (inherits(type@dtype@type, IntegerType)) {
    as.character(data)
  } else if (inherits(type@dtype@type, BooleanType)) {
    tolower(as.logical(data))
  }

  if (simplify_dense && length(shape(data)) <= 1L) {
    return(paste0(
      "array<",
      repr(type@dtype),
      ": ",
      paste(value_reprs, collapse = ", "),
      ">"
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
  dim(value_reprs) <- shape(data)

  f <- function(x) {
    if (length(shape(x)) == 1L || is.vector(x)) {
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
      # It's not clear to me, why we need the other constant types (FloatType) as there are no real
      # scalars
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
  function(value, dtype = NULL, ...) {
    S7::S7_dispatch()
  }
)

method(r_to_constant, S7::class_logical) <- function(
  value,
  dtype = NULL, # is ignored
  ...
) {
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  if (!is.null(dtype) && dtype != "pred") {
    stop("Invalid dtype for logical")
  }
  shape <- Shape(
    if (is.array(value)) shape(value) else integer()
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

method(r_to_constant, S7::class_double) <- function(
  value,
  dtype = NULL,
  ...
) {
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  if (!is.null(dtype) && !(dtype %in% c("f32", "f64"))) {
    stop("Invalid dtype for double")
  }
  dtype <- if (is.null(dtype)) {
    FloatType("f32")
  } else {
    string_to_type(dtype)
  }

  shape <- Shape(
    if (is.array(value)) shape(value) else integer()
  )

  element_type_obj <- TensorElementType(type = dtype)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_integer) <- function(
  value,
  dtype = NULL,
  ...
) {
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  valid_types <- c("i8", "i16", "i32", "i64", "ui8", "ui16", "ui32", "ui64")
  if (!is.null(dtype) && !(dtype %in% valid_types)) {
    stop("Invalid dtype for integer")
  }
  dtype <- if (is.null(dtype)) {
    IntegerType("i32")
  } else {
    string_to_type(dtype)
  }

  shape <- Shape(
    if (is.array(value)) shape(value) else integer()
  )

  element_type_obj <- TensorElementType(type = dtype)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_any) <- function(
  value,
  dtype = NULL,
  ...
) {
  stop("Unsupported type for r_to_constant: ", class(value)[1])
}

method(dim, TensorConstant) <- function(x) {
  x@type@shape@dims
}
