assert_tensor_constant <- function(
  x,
  ndims = NULL,
  elt_type = NULL,
  null_ok = FALSE
) {
  if (is.null(x) && null_ok) {
    return()
  }
  if (!inherits(x, Constant)) {
    cli::cli_abort("x must be a Constant")
  }
  if (!inherits(x@value, TensorConstant)) {
    cli::cli_abort("tnsr must be a TensorConstant")
  }
  if (!is.null(ndims) && length(dim(x@value)) != ndims) {
    cli::cli_abort("tnsr must have {ndims} dimensions")
  }

  if (!is.null(elt_type) && x@value@type@elt_type@type@Value != elt_type) {
    cli::cli_abort("tnsr must have element type {elt_type}")
  }
}
