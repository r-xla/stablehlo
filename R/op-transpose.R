#' @include op.R hlo.R
NULL

OpTranspose <- new_Op("OpTranspose", "transpose")

infer_types_transpose <- function(
  operand,
  permutation
) {
  stopifnot(inherits(operand@type, TensorType))

  # Extract operand dimensions
  operand_dims <- shape(operand)
  rank <- length(operand_dims)

  # Extract permutation values from the constant
  perm_values <- permutation@value@data

  # (C2) permutation is a permutation of range(rank(operand))
  if (length(perm_values) != rank) {
    cli::cli_abort("Length of permutation must equal rank of operand")
  }

  # Check if permutation is valid (contains all values from 0 to rank-1)
  expected_perm <- seq(0, rank - 1)
  if (!setequal(perm_values, expected_perm)) {
    cli::cli_abort("permutation must be a permutation of range(rank(operand))")
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
  # Convert permutation to tensor constant
  perm_attr <- hlo_tensor(
    as.integer(permutation),
    dtype = "i64",
    func = Func()
  )

  hlo_transpose_impl(
    values = list(operand = operand),
    attrs = list(permutation = perm_attr)
  )
}

method(repr, OpTranspose) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}
