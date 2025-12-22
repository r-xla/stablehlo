#' @include op.R hlo.R
NULL

OpTriangularSolve <- new_Op("OpTriangularSolve", "triangular_solve")

#' @rdname hlo_triangular_solve
#' @export
infer_types_triangular_solve <- function(
  a,
  b,
  left_side,
  lower,
  unit_diagonal,
  transpose_a
) {
  assert_vts_are_tensors(a, b)
  assert_vts_have_same_dtype(a, b)

  a_dims <- shape(a)
  b_dims <- shape(b)
  rank_a <- length(a_dims)
  rank_b <- length(b_dims)

  # (C2) 2 <= rank(a) = rank(b) = R
  if (rank_a < 2) {
    cli_abort("'a' must have rank >= 2")
  }
  if (rank_a != rank_b) {
    cli_abort("'a' and 'b' must have the same rank")
  }

  # (C3) dim(a, -2) = dim(a, -1) (a must be square)
  if (a_dims[rank_a] != a_dims[rank_a - 1]) {
    cli_abort("'a' must be a square matrix (last two dimensions must be equal)")
  }

  # (C3) shape(a)[:-3] = shape(b)[:-3] (batch dimensions must match)
  if (rank_a > 2) {
    a_batch <- a_dims[seq_len(rank_a - 2)]
    b_batch <- b_dims[seq_len(rank_b - 2)]
    if (!identical(a_batch, b_batch)) {
      cli_abort("Batch dimensions of 'a' and 'b' must match")
    }
  }

  # (C3) dim(a, -1) = dim(b, left_side ? -2 : -1)
  a_size <- a_dims[rank_a]
  b_relevant_dim <- if (left_side) b_dims[rank_b - 1] else b_dims[rank_b]
  if (a_size != b_relevant_dim) {
    cli_abort(sprintf(
      "Dimension mismatch: dim(a, -1) = %d but dim(b, %s) = %d",
      a_size,
      if (left_side) "-2" else "-1",
      b_relevant_dim
    ))
  }

  # Validate transpose_a
  valid_transpose <- c("NO_TRANSPOSE", "TRANSPOSE", "ADJOINT")
  if (!transpose_a %in% valid_transpose) {
    cli_abort(sprintf(
      "'transpose_a' must be one of: %s",
      paste(valid_transpose, collapse = ", ")
    ))
  }

  # (C4) result has the same type as b
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = b@type@dtype,
        shape = Shape(b_dims)
      )
    )
  ))
}

hlo_triangular_solve_impl <- hlo_fn(
  OpTriangularSolve,
  infer_types_triangular_solve
)

#' @templateVar mnemonic triangular_solve
#' @template op
#' @param left_side (`logical(1)`)\cr
#'   If `TRUE`, solve `op(a) * x = b`. If `FALSE`, solve `x * op(a) = b`.
#' @param lower (`logical(1)`)\cr
#'   If `TRUE`, use lower triangle of `a`. If `FALSE`, use upper triangle.
#' @param unit_diagonal (`logical(1)`)\cr
#'   If `TRUE`, assume diagonal elements of `a` are 1.
#' @param transpose_a (`character(1)`)\cr
#'   One of `"NO_TRANSPOSE"`, `"TRANSPOSE"`, or `"ADJOINT"`.
#' @export
hlo_triangular_solve <- function(
  a,
  b,
  left_side,
  lower,
  unit_diagonal,
  transpose_a
) {
  hlo_triangular_solve_impl(
    values = list(a = a, b = b),
    attrs = list(
      BoolAttr(name = "left_side", value = as.logical(left_side)),
      BoolAttr(name = "lower", value = as.logical(lower)),
      BoolAttr(name = "unit_diagonal", value = as.logical(unit_diagonal))
    ),
    custom_attrs = list(
      transpose_a = transpose_a
    )
  )
}

method(repr, OpTriangularSolve) <- function(
  x,
  toplevel = TRUE,
  simplify_dense = TRUE,
  ...
) {
  # Build attributes string - all attrs including transpose_a go inside braces
  attrs_str <- vapply(
    x@inputs@attrs@items,
    repr,
    character(1),
    simplify_dense = simplify_dense
  )
  transpose_attr <- sprintf(
    "transpose_a = #stablehlo<transpose %s>",
    x@inputs@custom_attrs$transpose_a
  )
  all_attrs <- paste(c(attrs_str, transpose_attr), collapse = ",\n")

  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " (",
    repr(x@inputs@values),
    ") {\n",
    all_attrs,
    "\n}: ",
    repr(x@signature)
  )
}
