##' @include op.R hlo.R
#NULL
#
#OpCholesky <- new_Op("OpCholesky", "cholesky")
#
#infer_types_cholesky <- function(
#  operand,
#  lower
#) {
#  stopifnot(inherits(operand@type, TensorType))
#
#  # Extract operand dims and rank
#  operand_dims <- shape(operand)
#  rank <- length(operand_dims)
#
#  # (C2) 2 <= rank(a)
#  if (rank < 2) {
#    cli_abort("operand needs to have at least rank = 2")
#  }
#
#  # (C3) dim(a, -2) = dim(a, -1)
#  if (operand_dims[rank] != operand_dims[rank - 1]) {
#    cli_abort("matrices have to be symetric")
#  }
#
#  ValueTypes(list(
#    ValueType(
#      TensorType(
#        dtype = operand@type@dtype,
#        shape = Shape(operand_dims)
#      )
#    )
#  ))
#}
#
#hlo_cholesky_impl <- hlo_fn(
#  OpCholesky,
#  infer_types_cholesky
#)
#
##' @templateVar mnemonic cholesky
##' @template op
##' @export
#hlo_cholesky <- function(
#  operand,
#  lower
#) {
#  hlo_cholesky_impl(
#    values = list(operand = operand),
#    custom_attrs = list(lower = as.logical(lower))
#  )
#}
#
#method(repr, OpCholesky) <- function(x, ...) {
#  paste0(
#    repr(x@outputs, ...),
#    " = ",
#    repr(x@name, ...),
#    " ",
#    repr(x@inputs, simplify_dense = TRUE, ...),
#    paste0(
#      "{\nlower = ",
#      tolower(as.character(x@inputs@custom_attrs$lower)),
#      "\n}"
#    ),
#    ": ",
#    repr(x@signature, ...)
#  )
#}
#
