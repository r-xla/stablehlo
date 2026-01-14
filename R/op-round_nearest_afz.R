#' @include op.R hlo.R type_inference.R
NULL

OpRoundNearestAfz <- new_Op("OpRoundNearestAfz", "round_nearest_afz")

#' @rdname hlo_round_nearest_afz
#' @export
infer_types_round_nearest_afz <- infer_types_float_uni

hlo_round_nearest_afz_impl <- hlo_fn(
  OpRoundNearestAfz,
  infer_types_round_nearest_afz
)

#' @templateVar mnemonic round_nearest_afz
#' @template op
#' @export
hlo_round_nearest_afz <- function(operand) {
  hlo_round_nearest_afz_impl(values = list(operand = operand))
}
