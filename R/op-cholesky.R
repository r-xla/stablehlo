#' @include op.R hlo.R
NULL

Cholesky <- new_Op("Cholesky", "cholesky")

# binary ops
infer_types_cholesky <- function(a, lower) {
  stopifnot(identical(dim(a)[-1], dim(a)[-2]))
  stopifnot(inherits(a@type, TensorType))
  ValueTypes(list(a))
}

hlo_cholesky_impl <- hlo_fn(Cholesky, infer_types_cholesky)

#' @templateVar mnemonic cholesky
#' @template op
#' @export
hlo_cholesky <- function(a, lower) {
  hlo_cholesky_impl(values = list(a = a), attrs = list(lower = lower))
}
