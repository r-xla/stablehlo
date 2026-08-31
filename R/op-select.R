#' @include op.R hlo.R
NULL

# select always uses assembly format with two types: pred_type, value_type
render_select <- function(ctx) {
  paste0(
    ctx$outputs_str,
    " = stablehlo.select ",
    ctx$values_str,
    " : ",
    ctx$in_type_strs[[1L]],
    ", ",
    ctx$in_type_strs[[2L]]
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
  result <- vt_meet(on_true, on_false)
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
    if (any(must(pred_dims != dims))) {
      cli_abort(c(
        "{.arg pred} must have the same shape as {.arg on_true}.",
        x = "Got shapes {shapevec_repr(pred_dims)} and {shapevec_repr(dims)}."
      ))
    }
    dims <- ifelse(is.na(dims), pred_dims, dims)
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
