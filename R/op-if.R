#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

#' @rdname hlo_if
#' @export
infer_types_if <- function(pred, true_branch, false_branch) {
  assert_vt_has_ttype(pred, "bool", shape = integer())
  out_types1 <- ValueTypes(func_output_types(true_branch))
  out_types2 <- ValueTypes(func_output_types(false_branch))
  if (length(out_types1) != length(out_types2)) {
    cli_abort(c(
      "{.arg true_branch} and {.arg false_branch} must have the same number of outputs.",
      x = "Got {length(out_types1)} and {length(out_types2)}."
    ))
  }
  # Only one branch runs, so the result knows an axis only if *both* branches
  # know it and agree -- the join, not the meet. Taking the meet here (or
  # simply returning the true branch's types, as this did) would report an axis
  # as `3` on the strength of one branch while the other admits any size.
  #
  # A dtype or rank difference is still a hard error, and so are two known but
  # different sizes: StableHLO requires the branches to agree, and widening
  # those to `?` would launder a program bug into a dynamic shape.
  joined <- vector("list", length(out_types1))
  for (i in seq_along(out_types1)) {
    t1 <- out_types1[[i]]
    t2 <- out_types2[[i]]
    compatible <- t1$type$dtype == t2$type$dtype &&
      length(shape(t1)) == length(shape(t2)) &&
      !any(must_ne(shape(t1), shape(t2)))
    if (!compatible) {
      error_unequal_types(
        arg1 = "output_types(true_branch)",
        arg2 = "output_types(false_branch)",
        index = i - 1L,
        expected = "must have the same type",
        actual1 = t1,
        actual2 = t2
      )
    }
    joined[[i]] <- ValueType(
      TensorType(
        dtype = t1$type$dtype,
        shape = Shape(shape_join(
          shape(t1),
          shape(t2),
          arg1 = "true_branch",
          arg2 = "false_branch"
        ))
      )
    )
  }
  ValueTypes(joined)
}

hlo_if_impl <- hlo_fn(OpIf, infer_types_if)

#' @templateVar mnemonic if
#' @templateVar not_func_variables simplify
#' @template op
#' @template param_simplify
#' @export
hlo_if <- function(pred, true_branch, false_branch, simplify = TRUE) {
  hlo_if_impl(
    values = list(pred = pred),
    funcs = list(
      true_branch = true_branch,
      false_branch = false_branch
    ),
    simplify = simplify
  )
}
