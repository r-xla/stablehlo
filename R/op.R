#' @include list_of.R
#' @include value_id.R
#' @include types.R
NULL

OpMnemonic <- new_enum(
  "OpMnemonic",
  c(
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
method(repr, OpMnemonic) <- function(x) {
  x@Value
}

OpName <- new_class(
  "OpName",
  properties = list(
    mnemonic = OpMnemonic
  )
)

method(repr, OpName) <- function(x) {
  paste0("\"stablehlo.", repr(x@mnemonic), "\"")
}

OpInputValue <- new_class(
  "OpInputValue",
  properties = list(
    id = ValueId
  )
)

OpInputValues <- new_list_of("OpInputValues", OpInputValue)

method(repr, OpInputValues) <- function(x) {
  paste0(
    "(",
    paste0(sapply(x@items, repr), collapse = ", "),
    ")"
  )
}

OpInputFuncs <- new_class("OpInputFuncs")
method(repr, OpInputFuncs) <- function(x) {
  # TODO!
  return("")
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
  if (length(x@items) == 0) return("")
  
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

OpOutput <- new_class(
  "OpOutput",
  properties = list(
    id = ValueId
  )
)

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

Op <- new_class(
  "Op",
  properties = list(
    name = OpName,
    inputs = OpInputs,
    outputs = OpOutputs | NULL,
    signature = OpSignature
  )
)
method(repr, Op) <- function(x) {
  paste0(
    repr(x@outputs),
    repr(x@name),
    repr(x@inputs),
    ":",
    repr(x@signature)
  )
}
