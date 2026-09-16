#' @include op.R hlo.R type_inference.R utils.R
NULL

OpAbs <- new_Op("OpAbs", "abs")


#' @rdname hlo_abs
#' @export
infer_types_abs <- function(operand) {
  # (I1): signed integer or floating-point operands (complex is not supported
  # yet). `HLO_SInt` in the ODS, not `HLO_Int`, so unsigned is out -- the
  # shared `infer_types_numeric_uni()` admits `uint`, which is why this op
  # needs its own inference.
  assert_vt_has_ttype(operand, "float", "int")
  ValueTypes(list(operand))
}

hlo_abs_impl <- hlo_fn(OpAbs, infer_types_abs)

#' @templateVar mnemonic abs
#' @template op
#' @export
hlo_abs <- function(operand, output_types = NULL) {
  hlo_abs_impl(values = list(operand = operand), output_types = output_types)
}
