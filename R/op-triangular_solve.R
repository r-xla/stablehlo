#' @include op.R hlo.R
NULL

render_triangular_solve <- function(ctx) {
  attrs_str <- vapply(
    ctx$attrs,
    repr,
    character(1)
  )
  transpose_attr <- sprintf(
    "transpose_a = #stablehlo<transpose %s>",
    ctx$custom_attrs$transpose_a
  )
  all_attrs <- paste(c(attrs_str, transpose_attr), collapse = ",\n")

  paste0(
    ctx$outputs_str,
    " = \"stablehlo.triangular_solve\" (",
    ctx$values_str,
    ") {\n",
    all_attrs,
    "\n}: ",
    ctx$sig_str
  )
}

OpTriangularSolve <- new_Op(
  "OpTriangularSolve",
  "triangular_solve",
  render = render_triangular_solve
)

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
  # (I1), (I2): floating-point operands only (complex is not supported yet)
  assert_vts_are_tensors(a, b)
  assert_vt_has_ttype(a, "float")
  assert_vt_has_ttype(b, "float")
  # (C1)
  assert_vts_have_same_dtype(a, b)
  assert_const(left_side, dtype = as_dtype("bool"), shape = integer())
  assert_const(lower, dtype = as_dtype("bool"), shape = integer())
  assert_const(unit_diagonal, dtype = as_dtype("bool"), shape = integer())
  left_side <- left_side$data
  lower <- lower$data
  unit_diagonal <- unit_diagonal$data

  a_dims <- shape(a)
  b_dims <- shape(b)
  rank_a <- length(a_dims)
  rank_b <- length(b_dims)

  # (C2)
  if (rank_a < 2) {
    cli_abort(c(
      "{.arg a} must have rank >= 2",
      x = "Got rank {rank_a}."
    ))
  }
  if (rank_a != rank_b) {
    cli_abort(c(
      "{.arg a} and {.arg b} must have the same rank",
      x = "Got ranks {rank_a} and {rank_b}."
    ))
  }

  # (C3) Square only when it is certainly not: a dynamic trailing axis may
  # match at run time.
  if (must_ne(a_dims[rank_a], a_dims[rank_a - 1L])) {
    cli_abort(c(
      "{.arg a} must be a square matrix (last two dimensions must be equal)",
      x = "Got shape {shapevec_repr(a_dims)}."
    ))
  }

  if (rank_a > 2) {
    a_batch <- a_dims[seq_len(rank_a - 2)]
    b_batch <- b_dims[seq_len(rank_b - 2)]
    # Batch axes agree; each refines the other, so `batch` below is the most
    # the two operands together know and is what the result carries.
    if (any(must_ne(a_batch, b_batch))) {
      cli_abort(c(
        "Batch dimensions of {.arg a} and {.arg b} must match",
        x = "Got shapes {shapevec_repr(a_batch)} and {shapevec_repr(b_batch)}."
      ))
    }
    b_dims[seq_len(rank_b - 2)] <- shape_meet(
      a_batch,
      b_batch,
      arg1 = "a",
      arg2 = "b"
    )
  }

  # (C3) `a`'s two trailing axes were just required to be equal, so meet them:
  # with `a = tensor<3x?xf32>` the square size is provably 3, and reading
  # `a_dims[rank_a]` alone would report `?` and defer a decidable check.
  a_size <- shape_meet(
    a_dims[rank_a - 1L],
    a_dims[rank_a],
    arg1 = "a",
    arg2 = "a"
  )
  b_axis <- if (left_side) rank_b - 1L else rank_b
  if (must_ne(a_size, b_dims[b_axis])) {
    cli_abort(c(
      "Dimension mismatch",
      x = "Got shapes {shapevec_repr(a_dims)} and {shapevec_repr(b_dims)}."
    ))
  }
  # `a`'s square size and this axis of `b` are equal, so a dynamic one on
  # either side is pinned by the other.
  b_dims[b_axis] <- shape_meet(
    a_size,
    b_dims[b_axis],
    arg1 = "a",
    arg2 = "b"
  )

  valid_transpose <- c("NO_TRANSPOSE", "TRANSPOSE", "ADJOINT")
  if (!test_choice(transpose_a, valid_transpose)) {
    cli_abort(c(
      "{.arg transpose_a} must be one of: {.val {valid_transpose}}.",
      x = "Got {transpose_a}."
    ))
  }

  # (C4)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = b$type$dtype,
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
#' @templateVar not_func_variables left_side,lower,unit_diagonal,transpose_a
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
  transpose_a,
  output_types = NULL
) {
  hlo_triangular_solve_impl(
    values = list(a = a, b = b),
    output_types = output_types,
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
