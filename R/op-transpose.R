#' @include op.R hlo.R
NULL

OpTranspose <- new_Op("OpTranspose", "transpose")

infer_types_transpose <- function(
  operand,
  permutation
) {
  stopifnot(inherits(operand@type, TensorType))

  operand_dims <- shape(operand)
  rank <- length(operand_dims)

  perm_values <- permutation@value@data

  if (rank == 0) {
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
  if (length(perm_values) != rank) {
    cli_abort("Length of permutation must equal rank of operand")
  }

  expected_perm <- seq(0, rank - 1)
  if (!setequal(perm_values, expected_perm)) {
    cli_abort("permutation must be a permutation of range(rank(operand))")
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
  perm_attr <- hlo_tensor(
    as.integer(permutation),
    # permutation might be integer()
    shape = length(permutation),
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
