#' @include op.R hlo.R type_inference.R
NULL

OpBesselI1e <- new_Op("OpBesselI1e", "bessel_i1e", dialect = "chlo")

#' @rdname hlo_bessel_i1e
#' @export
infer_types_bessel_i1e <- infer_types_float_uni

hlo_bessel_i1e_impl <- hlo_fn(OpBesselI1e, infer_types_bessel_i1e)

#' @templateVar mnemonic bessel_i1e
#' @template op_chlo
#' @export
hlo_bessel_i1e <- function(operand, output_types = NULL) {
  hlo_bessel_i1e_impl(
    values = list(operand = operand),
    output_types = output_types
  )
}
