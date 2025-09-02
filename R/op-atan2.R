#' @include op.R hlo.R utils.R
NULL

OpAtan2 <- new_Op("OpAtan2", "atan2")

hlo_atan2_impl <- hlo_fn(OpAtan2, infer_types_generic_biv)

#' @templateVar mnemonic atan2
#' @template op
#' @export
hlo_atan2 <- function(lhs, rhs) {
  hlo_atan2_impl(values = list(lhs = lhs, rhs = rhs))
}
