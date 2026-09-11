#' @include op.R hlo.R
NULL

# `select`'s short assembly form names only two types -- pred's and
# `on_true`'s -- so `on_true` stands in for `on_false` and for the result. That
# holds only while all three are the same type. Since inference *unifies* the
# operands rather than requiring equality, they need not be: one may know an
# axis the others leave dynamic, and emitting the short form then produces MLIR
# that does not parse ("use of value expects different type than prior uses").
# So fall back to the generic form, exactly as `render_op_default()` does when
# its own types disagree.
render_select <- function(ctx) {
  value_strs <- c(ctx$in_type_strs[-1L], ctx$out_type_strs)
  if (all(value_strs == value_strs[[1L]])) {
    return(paste0(
      ctx$outputs_str,
      " = stablehlo.select ",
      ctx$values_str,
      " : ",
      ctx$in_type_strs[[1L]],
      ", ",
      ctx$in_type_strs[[2L]]
    ))
  }
  paste0(
    ctx$outputs_str,
    " = \"stablehlo.select\" (",
    ctx$values_str,
    "): ",
    ctx$sig_str
  )
}

OpSelect <- new_Op("OpSelect", "select", render = render_select)

#' @rdname hlo_select
#' @export
infer_types_select <- function(
  pred,
  on_true,
  on_false
) {
  # (C2)
  assert_vts_are_tensors(on_true = on_true, on_false = on_false)
  result <- unify_vt(on_true, on_false)
  assert_vt_has_ttype(pred, "bool")

  # (C1) A scalar `pred` broadcasts; any other rank must be the result's, and
  # its sizes only have to be *possible* -- a dynamic axis on either side is
  # checked when the sizes are known, at run time.
  dims <- shape(result)
  if (naxes(pred) != 0L) {
    pred_dims <- shape(pred)
    if (length(pred_dims) != length(dims)) {
      cli_abort(c(
        "rank of {.arg pred} must be 0 or equal to rank of {.arg on_true}",
        x = "Got shapes {shapevec_repr(pred_dims)} and {shapevec_repr(dims)}."
      ))
    }
    if (any(provably_ne(pred_dims, dims))) {
      cli_abort(c(
        "{.arg pred} must have the same shape as {.arg on_true}.",
        x = "Got shapes {shapevec_repr(pred_dims)} and {shapevec_repr(dims)}."
      ))
    }
    # `unify_shapes()` rather than a hand-rolled `ifelse()`: the rank and the
    # definite clash were just checked in select's own words, so all this can
    # still do is refine -- and unification stays defined in one place.
    dims <- unify_shapes(dims, pred_dims, arg_a = "on_true", arg_b = "pred")
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = on_true$type$dtype,
        shape = Shape(dims)
      )
    )
  ))
}

hlo_select_impl <- hlo_fn(
  OpSelect,
  infer_types_select
)

#' @templateVar mnemonic select
#' @template op
#' @export
hlo_select <- function(
  pred,
  on_true,
  on_false,
  output_types = NULL
) {
  hlo_select_impl(
    values = list(pred = pred, on_true = on_true, on_false = on_false),
    output_types = output_types
  )
}
