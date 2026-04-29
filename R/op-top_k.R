#' @include op.R hlo.R
NULL

OpTopK <- new_Op("OpTopK", "top_k", dialect = "chlo")

#' @rdname hlo_top_k
#' @export
infer_types_top_k <- function(operand, k) {
  assert_vt_is_tensor(operand)
  assert_vt_has_ttype(
    operand,
    "FloatType",
    "IntegerType",
    "UIntegerType"
  )
  assert_const(k, dtype = IntegerType(64L), shape = integer())
  k <- k$data

  operand_shape <- shape(operand)
  rank <- length(operand_shape)

  if (rank < 1L) {
    cli_abort(c(
      "{.arg operand} must have rank >= 1.",
      x = "Got rank {.val {rank}}."
    ))
  }
  if (k < 1L) {
    cli_abort(c(
      "{.arg k} must be a positive integer.",
      x = "Got {.val {k}}."
    ))
  }
  last_dim <- operand_shape[[rank]]
  if (k > last_dim) {
    cli_abort(c(
      "{.arg k} must not exceed the size of the last dimension of {.arg operand}.",
      x = "Got k = {.val {k}} and last dimension size {.val {last_dim}}."
    ))
  }

  result_shape <- operand_shape
  result_shape[[rank]] <- as.integer(k)

  values_type <- ValueType(
    TensorType(dtype = operand$type$dtype, shape = Shape(result_shape))
  )
  indices_type <- ValueType(
    TensorType(dtype = IntegerType(32L), shape = Shape(result_shape))
  )

  ValueTypes(list(values_type, indices_type))
}

hlo_top_k_impl <- hlo_fn(OpTopK, infer_types_top_k)

#' @templateVar mnemonic top_k
#' @templateVar not_func_variables k
#' @template op_chlo
#' @param operand ([`FuncValue`])\cr
#'   Tensor of integer, unsigned integer, or floating-point type with rank >= 1.
#' @param k (`integer(1)`)\cr
#'   Number of top elements to return along the last dimension. Must satisfy
#'   `1 <= k <= dim(operand, -1)`.
#' @return A `list()` of two [`FuncValue`]s: the top-k values (same dtype as
#'   `operand`) and their indices into the last dimension (dtype `i32`). Ties
#'   are broken by lower index first.
#' @export
hlo_top_k <- function(operand, k) {
  hlo_top_k_impl(
    values = list(operand = operand),
    attrs = list(
      ScalarAttr(
        name = "k",
        value = as.integer(k),
        dtype = IntegerType(64L)
      )
    ),
    simplify = FALSE
  )
}
