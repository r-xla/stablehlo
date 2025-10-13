#' @include op.R hlo.R type_inference.R
NULL

OpRoundNearestAfz <- new_Op("OpRoundNearestAfz", "round_nearest_afz")

hlo_round_nearest_afz_impl <- hlo_fn(
  OpRoundNearestAfz,
  infer_types_generic_uni
)

#' @templateVar mnemonic round_nearest_afz
#' @template op
#' @export
hlo_round_nearest_afz <- function(operand) {
  hlo_round_nearest_afz_impl(values = list(operand = operand))
}
