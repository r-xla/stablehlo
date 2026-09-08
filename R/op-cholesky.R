#' @include op.R hlo.R
NULL

OpCholesky <- new_Op("OpCholesky", "cholesky")

infer_types_cholesky <- function(
  operand,
  lower
) {
  assert_vt_is_tensor(operand)
  assert_vt_has_ttype(operand, "float")
  assert_const(lower, dtype = as_dtype("bool"), shape = integer())

  operand_dims <- shape(operand)
  rank <- length(operand_dims)

  # (C2)
  if (rank < 2) {
    cli_abort(c(
      "{.arg operand} needs to have at least rank = 2",
      x = "Got rank = {rank}."
    ))
  }

  # (C3) dim(a, -2) = dim(a, -1). Refused only when both sizes are known and
  # differ; if either is dynamic the matrix may well be square at run time.
  if (must_ne(operand_dims[rank], operand_dims[rank - 1L])) {
    cli_abort(c(
      "{.arg operand} must be symmetric in the last two dimensions",
      x = "Got shape {shapevec_repr(operand_dims)}."
    ))
  }

  # The two trailing axes are equal by (C3), so each refines the other: a
  # `tensor<?x4xf32>` operand has a `tensor<4x4xf32>` result.
  result_dims <- operand_dims
  square <- shape_meet(
    operand_dims[rank - 1L],
    operand_dims[rank],
    arg1 = "operand",
    arg2 = "operand"
  )
  result_dims[c(rank - 1L, rank)] <- square

  # (C1)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(result_dims)
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
  lower,
  output_types = NULL
) {
  hlo_cholesky_impl(
    values = list(operand = operand),
    output_types = output_types,
    attrs = list(
      BoolAttr(name = "lower", value = as.logical(lower))
    )
  )
}
