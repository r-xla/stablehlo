#' @include op.R hlo.R type_inference.R
NULL

OpIsFinite <- new_Op("IsFinite", "is_finite")

#' @rdname hlo_is_finite
#' @export
infer_types_is_finite <- function(operand) {
  assert_vt_has_ttype(operand, "float")
  # (C1)
  ValueTypes(list(
    make_vt("pred", shape(operand))
  ))
}

hlo_is_finite_impl <- hlo_fn(OpIsFinite, infer_types_is_finite)

#' @templateVar mnemonic is_finite
#' @template op
#' @export
hlo_is_finite <- function(operand, output_types = NULL) {
  hlo_is_finite_impl(
    values = list(operand = operand),
    output_types = output_types
  )
}
