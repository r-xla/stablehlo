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
  "OpMnemonic",
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

method(`==`, list(OpMnemonic, OpMnemonic)) <- function(e1, e2) {
  e1@value == e2@value
}

method(`!=`, list(OpMnemonic, OpMnemonic)) <- function(e1, e2) {
  !(e1 == e2)
}
method(repr, OpMnemonic) <- function(x) {
  x@value
}

#' @title OpName
#' @description
#' This represents the name of an operation, containing a mnemonic.
#' @param mnemonic ([`OpMnemonic`])\cr
#'   The mnemonic of the operation.
#' @return (`OpName`)
#' @export
OpName <- new_class(
  "OpName",
  properties = list(
    mnemonic = OpMnemonic
  )
)

method(`==`, list(OpName, OpName)) <- function(e1, e2) {
  e1@mnemonic == e2@mnemonic
}

method(repr, OpName) <- function(x) {
  paste0("\"stablehlo.", repr(x@mnemonic), "\"")
}

# Attribute Types ----------------------------------------------------------

#' @title OpInputAttr
#' @description
#' Base class for operation input attributes.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @return (`OpInputAttr`)
#' @export
OpInputAttr <- new_class(
  "OpInputAttr",
  properties = list(
    name = S7::class_character
  )
)

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
ScalarAttr <- new_class(
  "ScalarAttr",
  parent = OpInputAttr,
  properties = list(
    value = S7::class_any,
    dtype = TensorDataType
  )
)

method(repr, ScalarAttr) <- function(x, simplify_dense = TRUE) {
  type_repr <- repr(x@dtype)
  value_repr <- if (S7::S7_inherits(x@dtype, BooleanType)) {
    sprintf("%s : %s", tolower(as.character(x@value)), type_repr)
  } else if (
    S7::S7_inherits(x@dtype, IntegerType) ||
      S7::S7_inherits(x@dtype, UnsignedType)
  ) {
    sprintf("%d : %s", as.integer(x@value), type_repr)
  } else {
    precision <- x@dtype@value
    sprintf(
      "%s : %s",
      format_double(as.double(x@value), precision = precision),
      type_repr
    )
  }
  paste0(x@name, " = ", value_repr)
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
BoolAttr <- new_class(
  "BoolAttr",
  parent = OpInputAttr,
  properties = list(
    value = S7::class_logical
  )
)

method(repr, BoolAttr) <- function(x, simplify_dense = TRUE) {
  paste0(x@name, " = ", tolower(as.character(x@value)))
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
StringAttr <- new_class(
  "StringAttr",
  parent = OpInputAttr,
  properties = list(
    value = S7::class_character
  )
)

method(repr, StringAttr) <- function(x, simplify_dense = TRUE) {
  sprintf("%s = \"%s\"", x@name, x@value)
}

#' @title ConstantAttr
#' @description
#' An attribute holding a constant value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`Constant`)\cr
#'   The value of the attribute.
#' @return (`ConstantAttr`)
#' @export
ConstantAttr <- new_class(
  "ConstantAttr",
  parent = OpInputAttr,
  properties = list(
    value = Constant
  )
)

method(repr, ConstantAttr) <- function(x, simplify_dense = TRUE) {
  paste0(
    x@name,
    " = ",
    repr(x@value, simplify_dense = simplify_dense)
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
#' @return (`ConstantAttr`)
#' @export
constant_attr <- function(name, value, dtype = NULL, shape = NULL) {
  if (is.null(shape)) {
    shape <- if (length(value) == 1L) integer() else length(value)
  }
  constant <- r_to_constant(value, dtype = dtype, shape = shape)
  ConstantAttr(name = name, value = constant)
}

#' @title OpInputValue
#' @description
#' This represents a value that can be used as input to an operation.
#' @param id ([`ValueId`])\cr
#'   The id of the value.
#' @return (`OpInputValue`)
#' @export
OpInputValue <- new_class(
  "OpInputValue",
  properties = list(
    id = ValueId
  )
)

method(repr, OpInputValue) <- function(x) {
  repr(x@id)
}

method(`==`, list(OpInputValue, OpInputValue)) <- function(e1, e2) {
  e1@id == e2@id
}

#' @title OpInputValues
#' @description
#' List of [`OpInputValue`]s.
#' @param items (`list()` of [`OpInputValue`])\cr
#'   The values that can be used as inputs to operations.
#' @return (`OpInputValues`)
#' @export
OpInputValues <- new_list_of("OpInputValues", OpInputValue)

method(repr, OpInputValues) <- function(x) {
  paste0(sapply(x@items, repr), collapse = ", ")
}

#' @title OpInputAttrs
#' @description
#' List of [`OpInputAttr`]s.
#' @param items (`list()` of [`OpInputAttr`])\cr
#'   The attributes that can be used as inputs to operations.
#' @return (`OpInputAttrs`)
#' @export
OpInputAttrs <- new_list_of("OpInputAttrs", OpInputAttr)
method(repr, OpInputAttrs) <- function(
  x,
  simplify_dense = TRUE
) {
  if (length(x@items) == 0) {
    return("")
  }

  a <- vapply(x@items, repr, character(1), simplify_dense = simplify_dense) |>
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
OpInputs <- new_class(
  "OpInputs",
  properties = list(
    values = OpInputValues,
    funcs = OpInputFuncs,
    attrs = OpInputAttrs,
    custom_attrs = S7::class_list
  )
)


method(repr, OpInputs) <- function(
  x,
  simplify_dense = TRUE
) {
  paste0(
    "(",
    repr(x@values),
    ")",
    repr(x@funcs),
    repr(x@attrs, simplify_dense = simplify_dense)
  )
}

method(`==`, list(OpInputs, OpInputs)) <- function(e1, e2) {
  e1@values == e2@values &&
    e1@funcs == e2@funcs &&
    e1@attrs == e2@attrs
}

#' @title OpOutput
#' @description
#' This represents an output of an operation.
#' @param id ([`ValueId`])\cr
#'   The id of the output.
#' @return (`OpOutput`)
#' @export
OpOutput <- new_class(
  "OpOutput",
  properties = list(
    id = ValueId
  ),
  constructor = function(id = ValueId()) {
    new_object(
      S7::S7_object(),
      id = id
    )
  }
)

method(`==`, list(OpOutput, OpOutput)) <- function(e1, e2) {
  e1@id == e2@id
}

method(repr, OpOutput) <- function(x) {
  repr(x@id)
}

#' @title OpOutputs
#' @description
#' List of [`OpOutput`]s.
#' @param items (`list()` of [`OpOutput`])\cr
#'   The outputs of an operation.
#' @return (`OpOutputs`)
#' @export
OpOutputs <- new_list_of("OpOutputs", OpOutput)

method(repr, OpOutputs) <- function(x) {
  if (length(x@items) == 0L) {
    return("")
  } else {
    paste0(sapply(x@items, repr), collapse = ", ")
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
OpSignature <- new_class(
  "OpSignature",
  properties = list(
    input_types = ValueTypes,
    output_types = ValueTypes
  )
)

method(repr, OpSignature) <- function(x) {
  paste0(
    "(",
    repr(x@input_types),
    ")",
    " -> ",
    "(",
    repr(x@output_types),
    ")"
  )
}

method(`==`, list(OpSignature, OpSignature)) <- function(e1, e2) {
  e1@input_types == e2@input_types &&
    e1@output_types == e2@output_types
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
Op <- new_class(
  "Op",
  properties = list(
    name = OpName,
    inputs = OpInputs,
    outputs = OpOutputs,
    signature = OpSignature
  )
)

new_Op <- function(classname, mnemonic) {
  new_class(
    classname,
    parent = Op,
    constructor = function(inputs, outputs, signature) {
      new_object(
        Op(
          name = OpName(OpMnemonic(mnemonic)),
          inputs = inputs,
          outputs = outputs,
          signature = signature
        )
      )
    }
  )
}

method(repr, Op) <- function(x, toplevel = TRUE, simplify_dense = TRUE) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = simplify_dense),
    ": ",
    repr(x@signature)
  )
}

method(`==`, list(Op, Op)) <- function(e1, e2) {
  e1@name == e2@name &&
    e1@inputs == e2@inputs &&
    e1@outputs == e2@outputs &&
    e1@signature == e2@signature
}

method(`!=`, list(Op, Op)) <- function(e1, e2) {
  !(e1 == e2)
}
