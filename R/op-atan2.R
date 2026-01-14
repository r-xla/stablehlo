#' @include op.R hlo.R type_inference.R
NULL

OpAtan2 <- new_Op("OpAtan2", "atan2")

#' @rdname hlo_atan2
#' @export
infer_types_atan2 <- infer_types_float_biv

hlo_atan2_impl <- hlo_fn(OpAtan2, infer_types_atan2)

#' @templateVar mnemonic atan2
#' @template op
#' @export
hlo_atan2 <- function(lhs, rhs) {
  hlo_atan2_impl(values = list(lhs = lhs, rhs = rhs))
}
