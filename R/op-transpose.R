#' @include op.R hlo.R
NULL

OpTranspose <- new_Op("OpTranspose", "transpose")

#' @rdname hlo_transpose
#' @export
infer_types_transpose <- function(
  operand,
  permutation
) {
  assert_vt_is_tensor(operand)
  assert_const(permutation, dtype = as_dtype("i64"), naxes = 1L)

  operand_dims <- shape(operand)
  num_dims <- length(operand_dims)

  perm_values <- permutation$data

  # (C2) `permutation` is a permutation of `range(rank(operand))`. Compared as
  # a sorted vector, not with `setequal()`: sets ignore multiplicity and
  # length, so `c(0, 1, 1)` on a rank-2 operand passed, and (C3) below then
  # built a rank-3 result out of a rank-2 operand.
  # `as.integer()` because `identical()` compares storage type too, and a
  # permutation constant built from doubles is still a valid permutation.
  if (!identical(sort(as.integer(perm_values)), seq_len(num_dims) - 1L)) {
    error_permute_index(
      arg = "permutation",
      permutation = perm_values,
      expected = seq_len(num_dims) - 1L
    )
  }

  # (C3)
  result_dims <- operand_dims[perm_values + 1L]

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

hlo_transpose_impl <- hlo_fn(OpTranspose, infer_types_transpose)

#' @templateVar mnemonic transpose
#' @template op
#' @export
hlo_transpose <- function(
  operand,
  permutation,
  output_types = NULL
) {
  perm_int <- as.integer(permutation)
  hlo_transpose_impl(
    values = list(operand = operand),
    output_types = output_types,
    attrs = list(
      constant_attr(
        "permutation",
        perm_int,
        dtype = "i64",
        shape = length(perm_int)
      )
    )
  )
}
