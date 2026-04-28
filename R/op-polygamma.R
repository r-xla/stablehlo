#' @include op.R hlo.R type_inference.R
NULL

OpPolygamma <- new_Op("OpPolygamma", "polygamma", dialect = "chlo")

#' @rdname hlo_polygamma
#' @export
infer_types_polygamma <- function(n, x) {
  infer_types_float_biv(n, x)
}

hlo_polygamma_impl <- hlo_fn(OpPolygamma, infer_types_polygamma)

#' @templateVar mnemonic polygamma
#' @template op_chlo
#' @export
hlo_polygamma <- function(n, x) {
  hlo_polygamma_impl(values = list(n = n, x = x))
}
