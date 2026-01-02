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

  operand_dims <- shape(operand)
  num_dims <- length(operand_dims)

  perm_values <- permutation@value@data

  if (num_dims == 0) {
    if (length(perm_values) != 0) {
      cli_abort("Length of permutation must be 0 for scalar operands")
    }
    return(ValueTypes(list(
      ValueType(
        TensorType(
          dtype = operand@type@dtype,
          shape = Shape(integer())
        )
      )
    )))
  }

  # (C2) permutation is a permutation of range(rank(operand))
  if (length(perm_values) != num_dims) {
    cli_abort("Length of permutation must equal rank of operand")
  }

  # Check if any permutation values are out of range
  if (any(perm_values < 0L | perm_values >= num_dims)) {
    invalid_dims <- perm_values[perm_values < 0L | perm_values >= num_dims]
    dimension_out_of_range_error(
      arg = "permutation",
      dimension = invalid_dims,
      ndims = num_dims
    )
  }

  expected_perm <- seq(0, num_dims - 1)
  if (!setequal(perm_values, expected_perm)) {
    permutation_error(
      arg = "permutation",
      permutation = perm_values,
      ndims = num_dims
    )
  }

  # (C3) shape(result) = dim(operand, permutation...)
  # Convert 0-based permutation to 1-based for R indexing
  result_dims <- operand_dims[perm_values + 1L]

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand@type@dtype,
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
  permutation
) {
  hlo_transpose_impl(
    values = list(operand = operand),
    attrs = list(
      constant_attr(
        "permutation",
        as.integer(permutation),
        dtype = "i64",
        shape = c()
      )
    )
  )
}
