#' @include list_of.R
#' @include value_id.R
#' @include types.R
#' @include constant.R
#' @include func.R
#'
NULL

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
  e1@Value == e2@Value
}

method(`!=`, list(OpMnemonic, OpMnemonic)) <- function(e1, e2) {
  !(e1 == e2)
}
method(repr, OpMnemonic) <- function(x) {
  x@Value
}

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

OpInputValues <- new_list_of("OpInputValues", OpInputValue)

method(repr, OpInputValues) <- function(x) {
  paste0(
    "(",
    paste0(sapply(x@items, repr), collapse = ", "),
    ")"
  )
}

OpInputAttrName <- new_class(
  "OpInputAttrName",
  properties = list(
    value = class_character
  )
)
method(repr, OpInputAttrName) <- function(x) {
  x@value
}

OpInputAttrValue <- new_class(
  "OpInputAttrValue",
  properties = list(
    value = Constant
  )
)
method(repr, OpInputAttrValue) <- function(x) {
  repr(x@value)
}

OpInputAttr <- new_class(
  "OpInputAttr",
  properties = list(
    name = OpInputAttrName,
    value = OpInputAttrValue
  )
)
method(repr, OpInputAttr) <- function(x) {
  paste0(
    repr(x@name),
    " = ",
    repr(x@value)
  )
}

OpInputAttrs <- new_list_of("OpInputAttrs", OpInputAttr)
method(repr, OpInputAttrs) <- function(x) {
  if (length(x@items) == 0) {
    return("")
  }

  a <- vapply(x@items, repr, character(1)) |>
    paste(collapse = ", ")

  paste0("{ ", a, " }")
}

OpInputs <- new_class(
  "OpInputs",
  properties = list(
    values = OpInputValues,
    funcs = OpInputFuncs,
    attrs = OpInputAttrs
  )
)

method(repr, OpInputs) <- function(x) {
  paste0(
    repr(x@values),
    repr(x@funcs),
    repr(x@attrs)
  )
}

method(`==`, list(OpInputs, OpInputs)) <- function(e1, e2) {
  e1@values == e2@values &&
    e1@funcs == e2@funcs &&
    e1@attrs == e2@attrs
}

OpOutput <- new_class(
  "OpOutput",
  properties = list(
    id = ValueId
  ),
  constructor = function(value_id = ValueId()) {
    new_object(
      OpOutput,
      id = value_id
    )
  }
)

method(`==`, list(OpOutput, OpOutput)) <- function(e1, e2) {
  e1@id == e2@id
}

method(repr, OpOutput) <- function(x) {
  repr(x@id)
}

OpOutputs <- new_list_of("OpOutputs", OpOutput)

method(repr, OpOutputs) <- function(x) {
  if (length(x@items) == 0L) {
    return("")
  } else {
    paste(
      paste0(sapply(x@items, repr), collapse = ", "),
      "="
    )
  }
}

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
        Op,
        name = OpName(OpMnemonic(mnemonic)),
        inputs = inputs,
        outputs = outputs,
        signature = signature
      )
    }
  )
}

method(repr, Op) <- function(x, toplevel = TRUE) {
  paste0(
    repr(x@outputs),
    repr(x@name),
    repr(x@inputs),
    ":",
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
