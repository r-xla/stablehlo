#' @include op.R hlo.R
NULL

OpCholesky <- new_Op("OpCholesky", "cholesky")

infer_types_cholesky <- function(
  operand,
  lower
) {
  assert_vt_is_tensor(operand)
  assert_vt_has_ttype(operand, "FloatType")
  assert_const(lower, dtype = BooleanType(), shape = integer())

  operand_dims <- shape(operand)
  rank <- length(operand_dims)

  # (C2)
  if (rank < 2) {
    cli_abort(c(
      "{.arg operand} needs to have at least rank = 2",
      x = "Got rank = {rank}."
    ))
  }

  # (C3) dim(a, -2) = dim(a, -1)
  if (operand_dims[rank] != operand_dims[rank - 1]) {
    cli_abort(c(
      "The operand must be symmetric in the last two dimensions",
      x = "Got {shapevec_repr(operand_dims)}."
    ))
  }

  # (C1)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(operand_dims)
      )
    )
  ))
}

hlo_cholesky_impl <- hlo_fn(
  OpCholesky,
  infer_types_cholesky
)

#' @templateVar mnemonic cholesky
#' @template op
#' @details
#' The values of the other half of the matrix are not guaranteed and backend dependent.
#' @export
hlo_cholesky <- function(
  operand,
  lower
) {
  hlo_cholesky_impl(
    values = list(operand = operand),
    attrs = list(
      BoolAttr(name = "lower", value = as.logical(lower))
    )
  )
}
