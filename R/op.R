#' @include list_of.R
#' @include value_id.R
#' @include types.R
#' @include constant.R
#' @include func.R
#'
NULL

#' @title OpMnemonic
#' @description
#' This represents the mnemonic of an operation.
#' @param value (`character(1)`)\cr
#'   The mnemonic of the operation.
#' @return `OpMnemonic`
#' @export
OpMnemonic <- new_enum(
  "stablehlo_OpMnemonic",
  c(
    "return",
    "abs",
    "add",
    "after_all",
    "all_gather",
    "all_reduce",
    "all_to_all",
    "and",
    "atan2",
    "batch_norm_grad",
    "batch_norm_inference",
    "batch_norm_training",
    "bitcast_convert",
    "broadcast_in_dim",
    "case",
    "cbrt",
    "ceil",
    "cholesky",
    "clamp",
    "collective_broadcast",
    "collective_permute",
    "compare",
    "complex",
    "composite",
    "concatenate",
    "constant",
    "convert",
    "convolution",
    "cosine",
    "count_leading_zeros",
    "custom_call",
    "divide",
    "dot_general",
    "dynamic_broadcast_in_dim",
    "dynamic_conv",
    "dynamic_gather",
    "dynamic_iota",
    "dynamic_pad",
    "dynamic_reshape",
    "dynamic_slice",
    "dynamic_update_slice",
    "exponential",
    "exponential_minus_one",
    "fft",
    "floor",
    "gather",
    "get_dimension_size",
    "get_tuple_element",
    "if",
    "imag",
    "infeed",
    "iota",
    "is_finite",
    "log",
    "log_plus_one",
    "logistic",
    "map",
    "maximum",
    "minimum",
    "multiply",
    "negate",
    "not",
    "optimization_barrier",
    "or",
    "outfeed",
    "pad",
    "partition_id",
    "popcnt",
    "power",
    "real",
    "recv",
    "reduce",
    "reduce_precision",
    "reduce_scatter",
    "reduce_window",
    "remainder",
    "replica_id",
    "reshape",
    "reverse",
    "rng",
    "rng_bit_generator",
    "round_nearest_afz",
    "round_nearest_even",
    "rsqrt",
    "scatter",
    "select",
    "select_and_scatter",
    "send",
    "shift_left",
    "shift_right_arithmetic",
    "shift_right_logical",
    "sign",
    "sine",
    "slice",
    "sort",
    "sqrt",
    "subtract",
    "tan",
    "tanh",
    "transpose",
    "triangular_solve",
    "tuple",
    "uniform_dequantize",
    "uniform_quantize",
    "while",
    "xor"
  )
)

#' @export
`==.stablehlo_OpMnemonic` <- function(e1, e2) {
  e1$value == e2$value
}

#' @export
`!=.stablehlo_OpMnemonic` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
repr.stablehlo_OpMnemonic <- function(x, ...) {
  x$value
}

#' @title OpName
#' @description
#' This represents the name of an operation, containing a mnemonic.
#' @param mnemonic ([`OpMnemonic`])\cr
#'   The mnemonic of the operation.
#' @return (`OpName`)
#' @export
OpName <- function(mnemonic) {
  checkmate::assert_class(mnemonic, "stablehlo_OpMnemonic")

  structure(
    list(mnemonic = mnemonic),
    class = "stablehlo_OpName"
  )
}

#' @export
`==.stablehlo_OpName` <- function(e1, e2) {
  e1$mnemonic == e2$mnemonic
}

#' @export
repr.stablehlo_OpName <- function(x, ...) {
  paste0("\"stablehlo.", repr(x$mnemonic), "\"")
}

# Attribute Types ----------------------------------------------------------

#' @title OpInputAttr
#' @description
#' Base class for operation input attributes.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @return (`OpInputAttr`)
#' @export
OpInputAttr <- function(name) {
  checkmate::assert_string(name)

  structure(
    list(name = name),
    class = "stablehlo_OpInputAttr"
  )
}

#' @title ScalarAttr
#' @description
#' An attribute holding a scalar value with an associated dtype.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`numeric(1)` or `logical(1)`)\cr
#'   The scalar value.
#' @param dtype ([`TensorDataType`])\cr
#'   The dtype of the scalar (e.g., `IntegerType(32)`, `FloatType(32)`, `BooleanType()`).
#' @return `ScalarAttr`
#' @export
ScalarAttr <- function(name, value, dtype) {
  checkmate::assert_string(name)
  assert_tensor_dtype(dtype)

  structure(
    list(name = name, value = value, dtype = dtype),
    class = c("stablehlo_ScalarAttr", "stablehlo_OpInputAttr")
  )
}

#' @export
repr.stablehlo_ScalarAttr <- function(x, simplify_dense = TRUE, ...) {
  type_repr <- repr(x$dtype)
  value_repr <- if (inherits(x$dtype, "stablehlo_BooleanType")) {
    sprintf("%s : %s", tolower(as.character(x$value)), type_repr)
  } else if (
    inherits(x$dtype, "stablehlo_IntegerType") ||
      inherits(x$dtype, "stablehlo_UnsignedType")
  ) {
    sprintf("%d : %s", as.integer(x$value), type_repr)
  } else {
    precision <- x$dtype$value
    sprintf(
      "%s : %s",
      format_double(as.double(x$value), precision = precision),
      type_repr
    )
  }
  paste0(x$name, " = ", value_repr)
}

#' @title BoolAttr
#' @description
#' An attribute holding a boolean value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`logical(1)`)\cr
#'   The boolean value.
#' @return `BoolAttr`
#' @export
BoolAttr <- function(name, value) {
  checkmate::assert_string(name)
  checkmate::assert_flag(value)

  structure(
    list(name = name, value = value),
    class = c("stablehlo_BoolAttr", "stablehlo_OpInputAttr")
  )
}

#' @export
repr.stablehlo_BoolAttr <- function(x, simplify_dense = TRUE, ...) {
  paste0(x$name, " = ", tolower(as.character(x$value)))
}

#' @title StringAttr
#' @description
#' An attribute holding a string value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`character(1)`)\cr
#'   The string value.
#' @return `StringAttr`
#' @export
StringAttr <- function(name, value) {
  checkmate::assert_string(name)
  checkmate::assert_string(value)

  structure(
    list(name = name, value = value),
    class = c("stablehlo_StringAttr", "stablehlo_OpInputAttr")
  )
}

#' @export
repr.stablehlo_StringAttr <- function(x, simplify_dense = TRUE, ...) {
  sprintf("%s = \"%s\"", x$name, x$value)
}

#' @title ConstantAttr
#' @description
#' An attribute holding a constant value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`Constant`)\cr
#'   The value of the attribute.
#' @param simplify_dense (`logical(1)`)\cr
#'   Whether to simplify dense representation. Set to `FALSE` for multi-dimensional arrays.
#' @return (`ConstantAttr`)
#' @export
ConstantAttr <- function(name, value, simplify_dense = TRUE) {
  checkmate::assert_string(name)
  checkmate::assert_class(value, "stablehlo_Constant")
  checkmate::assert_flag(simplify_dense)

  structure(
    list(name = name, value = value, simplify_dense = simplify_dense),
    class = c("stablehlo_ConstantAttr", "stablehlo_OpInputAttr")
  )
}

#' @export
repr.stablehlo_ConstantAttr <- function(x, simplify_dense = TRUE, ...) {
  # TODO: This should be handled nicer
  use_simplify <- x$simplify_dense && simplify_dense
  paste0(
    x$name,
    " = ",
    repr(x$value, simplify_dense = use_simplify)
  )
}

#' @title Create a ConstantAttr from R values
#' @description
#' Helper function to create a ConstantAttr from R values.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (any)\cr
#'   The R value to convert to a constant.
#' @param dtype (`character(1)` | `NULL`)\cr
#'   The dtype of the constant. If NULL, inferred from value.
#' @param shape (`integer()` | `NULL`)\cr
#'   The shape of the constant. If NULL, inferred from value.
#' @param simplify_dense (`logical(1)`)\cr
#'   Whether to simplify dense representation. Set to `FALSE` for multi-dimensional arrays.
#' @return (`ConstantAttr`)
#' @export
constant_attr <- function(
  name,
  value,
  dtype = NULL,
  shape = NULL,
  simplify_dense = TRUE
) {
  if (is.null(shape)) {
    shape <- if (length(value) == 1L) integer() else length(value)
  }
  constant <- r_to_constant(value, dtype = dtype, shape = shape)
  ConstantAttr(name = name, value = constant, simplify_dense = simplify_dense)
}

#' @title OpInputValue
#' @description
#' This represents a value that can be used as input to an operation.
#' @param id ([`ValueId`])\cr
#'   The id of the value.
#' @return (`OpInputValue`)
#' @export
OpInputValue <- function(id) {
  checkmate::assert_class(id, "stablehlo_ValueId")

  structure(
    list(id = id),
    class = "stablehlo_OpInputValue"
  )
}

#' @export
repr.stablehlo_OpInputValue <- function(x, ...) {
  repr(x$id)
}

#' @export
`==.stablehlo_OpInputValue` <- function(e1, e2) {
  e1$id == e2$id
}

#' @title OpInputValues
#' @description
#' List of [`OpInputValue`]s.
#' @param items (`list()` of [`OpInputValue`])\cr
#'   The values that can be used as inputs to operations.
#' @return (`OpInputValues`)
#' @export
OpInputValues <- new_list_of(
  "stablehlo_OpInputValues",
  "stablehlo_OpInputValue"
)

#' @export
repr.stablehlo_OpInputValues <- function(x, ...) {
  paste0(vapply(x$items, repr, character(1)), collapse = ", ")
}

#' @title OpInputAttrs
#' @description
#' List of [`OpInputAttr`]s.
#' @param items (`list()` of [`OpInputAttr`])\cr
#'   The attributes that can be used as inputs to operations.
#' @return (`OpInputAttrs`)
#' @export
OpInputAttrs <- new_list_of("stablehlo_OpInputAttrs", "stablehlo_OpInputAttr")

#' @export
repr.stablehlo_OpInputAttrs <- function(x, simplify_dense = TRUE, ...) {
  if (length(x$items) == 0) {
    return("")
  }

  a <- vapply(
    x$items,
    function(item) repr(item, simplify_dense = simplify_dense),
    character(1)
  ) |>
    paste(collapse = ",\n")

  paste0(" {\n", a, "\n}")
}

#' @title OpInputs
#' @description
#' This represents all the inputs to an operation, including values, functions, and attributes.
#' @param values ([`OpInputValues`])\cr
#'   The values used as inputs.
#' @param funcs ([`OpInputFuncs`])\cr
#'   The functions used as inputs.
#' @param attrs ([`OpInputAttrs`])\cr
#'   The attributes used as inputs.
#' @param custom_attrs (`list`)\cr
#'   Custom attributes. Use this attributes that require custom formatting.
#' @return (`OpInputs`)
#' @export
OpInputs <- function(
  values,
  funcs = OpInputFuncs(),
  attrs = OpInputAttrs(),
  custom_attrs = list()
) {
  checkmate::assert_class(values, "stablehlo_OpInputValues")
  checkmate::assert_class(funcs, "stablehlo_OpInputFuncs")
  checkmate::assert_class(attrs, "stablehlo_OpInputAttrs")
  checkmate::assert_list(custom_attrs)

  structure(
    list(
      values = values,
      funcs = funcs,
      attrs = attrs,
      custom_attrs = custom_attrs
    ),
    class = "stablehlo_OpInputs"
  )
}

#' @export
repr.stablehlo_OpInputs <- function(x, simplify_dense = TRUE, ...) {
  paste0(
    "(",
    repr(x$values),
    ")",
    repr(x$funcs),
    repr(x$attrs, simplify_dense = simplify_dense)
  )
}

#' @export
`==.stablehlo_OpInputs` <- function(e1, e2) {
  e1$values == e2$values &&
    e1$funcs == e2$funcs &&
    e1$attrs == e2$attrs
}

#' @title OpOutput
#' @description
#' This represents an output of an operation.
#' @param id ([`ValueId`])\cr
#'   The id of the output.
#' @return (`OpOutput`)
#' @export
OpOutput <- function(id = ValueId()) {
  checkmate::assert_class(id, "stablehlo_ValueId")

  structure(
    list(id = id),
    class = "stablehlo_OpOutput"
  )
}

#' @export
`==.stablehlo_OpOutput` <- function(e1, e2) {
  e1$id == e2$id
}

#' @export
repr.stablehlo_OpOutput <- function(x, ...) {
  repr(x$id)
}

#' @title OpOutputs
#' @description
#' List of [`OpOutput`]s.
#' @param items (`list()` of [`OpOutput`])\cr
#'   The outputs of an operation.
#' @return (`OpOutputs`)
#' @export
OpOutputs <- new_list_of("stablehlo_OpOutputs", "stablehlo_OpOutput")

#' @export
repr.stablehlo_OpOutputs <- function(x, ...) {
  if (length(x$items) == 0L) {
    return("")
  } else {
    paste0(vapply(x$items, repr, character(1)), collapse = ", ")
  }
}

#' @title OpSignature
#' @description
#' This represents the signature of an operation, defining its input and output types.
#' @param input_types ([`ValueTypes`])\cr
#'   The types of the inputs.
#' @param output_types ([`ValueTypes`])\cr
#'   The types of the outputs.
#' @return (`OpSignature`)
#' @export
OpSignature <- function(input_types, output_types) {
  checkmate::assert_class(input_types, "stablehlo_ValueTypes")
  checkmate::assert_class(output_types, "stablehlo_ValueTypes")

  structure(
    list(input_types = input_types, output_types = output_types),
    class = "stablehlo_OpSignature"
  )
}

#' @export
repr.stablehlo_OpSignature <- function(x, ...) {
  paste0(
    "(",
    repr(x$input_types),
    ")",
    " -> ",
    "(",
    repr(x$output_types),
    ")"
  )
}

#' @export
`==.stablehlo_OpSignature` <- function(e1, e2) {
  e1$input_types == e2$input_types &&
    e1$output_types == e2$output_types
}

#' @title Op
#' @description
#' This represents a StableHLO operation.
#' @param name ([`OpName`])\cr
#'   The name of the operation.
#' @param inputs ([`OpInputs`])\cr
#'   The inputs to the operation.
#' @param outputs ([`OpOutputs`])\cr
#'   The outputs of the operation.
#' @param signature ([`OpSignature`])\cr
#'   The signature of the operation.
#' @return (`Op`)
#' @export
Op <- function(name, inputs, outputs, signature) {
  checkmate::assert_class(name, "stablehlo_OpName")
  checkmate::assert_class(inputs, "stablehlo_OpInputs")
  checkmate::assert_class(outputs, "stablehlo_OpOutputs")
  checkmate::assert_class(signature, "stablehlo_OpSignature")

  structure(
    list(
      name = name,
      inputs = inputs,
      outputs = outputs,
      signature = signature
    ),
    class = "stablehlo_Op"
  )
}

#' Create a new Op subclass
#' @param classname Name of the new Op class
#' @param mnemonic The operation mnemonic
#' @return Constructor function for the Op subclass
#' @keywords internal
new_Op <- function(classname, mnemonic) {
  # Return a constructor function
  function(inputs, outputs, signature) {
    base_op <- Op(
      name = OpName(OpMnemonic(mnemonic)),
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )
    class(base_op) <- c(classname, "stablehlo_Op")
    base_op
  }
}

#' @export
repr.stablehlo_Op <- function(x, toplevel = TRUE, simplify_dense = TRUE, ...) {
  paste0(
    repr(x$outputs),
    " = ",
    repr(x$name),
    " ",
    repr(x$inputs, simplify_dense = simplify_dense),
    ": ",
    repr(x$signature)
  )
}

#' @export
`==.stablehlo_Op` <- function(e1, e2) {
  e1$name == e2$name &&
    e1$inputs == e2$inputs &&
    e1$outputs == e2$outputs &&
    e1$signature == e2$signature
}

#' @export
`!=.stablehlo_Op` <- function(e1, e2) {
  !(e1 == e2)
}
