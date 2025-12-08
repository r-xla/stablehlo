#' @include op.R hlo.R type_inference.R
NULL

OpIsFinite <- new_Op("IsFinite", "is_finite")

#' @rdname hlo_is_finite
#' @export
infer_types_is_finite <- function(operand) {
  assert_vt_is_tensor(operand)
  ValueTypes(list(
    make_value_type("pred", shape(operand))
  ))
}

hlo_is_finite_impl <- hlo_fn(OpIsFinite, infer_types_is_finite)

#' @templateVar mnemonic is_finite
#' @template op
#' @export
hlo_is_finite <- function(operand) {
  hlo_is_finite_impl(values = list(operand = operand))
}
