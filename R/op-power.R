#' @include op.R hlo.R type_inference.R
NULL

OpPower <- new_Op("OpPower", "power")

#' @rdname hlo_power
#' @export
infer_types_power <- infer_types_numeric_biv

hlo_power_impl <- hlo_fn(OpPower, infer_types_power)

#' @templateVar mnemonic power
#' @template op
#' @export
hlo_power <- function(lhs, rhs) {
  hlo_power_impl(values = list(lhs = lhs, rhs = rhs))
}
