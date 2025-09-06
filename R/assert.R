assert_tensor_constant <- function(
  x,
  ndims = NULL,
  dtype = NULL,
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
  if (!is.null(ndims) && length(shape(x@value)) != ndims) {
    cli::cli_abort("tnsr must have {ndims} dimensions")
  }

  if (!is.null(dtype) && x@value@type@dtype@type@Value != dtype) {
    cli::cli_abort("tnsr must have element type {dtype}")
  }
}
