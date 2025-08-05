#' @include types.R
#' @include repr.R
NULL

TensorConstant <- new_class(
  "TensorConstant",
  properties = list(
    # This can be anything as long as it implements r_to_constant
    data = S7::class_any,
    type = TensorType
  )
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
  } else {
    return(paste0(
      "dense<",
      paste(value_reprs, collapse = ", "),
      "> : ",
      repr(type)
    ))
  }

  f <- function(x) {
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

r_to_constant <- S7::new_generic(
  "r_to_constant",
  "value",
  function(value, elt_type = NULL, ...) {
    S7::S7_dispatch()
  }
)

method(r_to_constant, S7::class_logical) <- function(
  value,
  elt_type = NULL, # is ignored
  ...
) {
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  if (!is.null(elt_type) && elt_type != "pred") {
    stop("Invalid elt_type for logical")
  }
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

method(r_to_constant, S7::class_double) <- function(
  value,
  elt_type = NULL,
  ...
) {
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  if (!is.null(elt_type) && !(elt_type %in% c("f32", "f64"))) {
    stop("Invalid elt_type for double")
  }
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
  if (!is.array(value) && length(value) != 1L) {
    stop("Either provide an R array or a length 1 vector.")
  }
  valid_types <- c("i8", "i16", "i32", "i64", "ui8", "ui16", "ui32", "ui64")
  if (!is.null(elt_type) && !(elt_type %in% valid_types)) {
    stop("Invalid elt_type for integer")
  }
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
