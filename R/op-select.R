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
  assert_vt_equal(on_true, on_false)
  assert_vt_has_ttype(pred, "BooleanType")

  # (C1)
  if (ndims(pred) != 0 && !identical(shape(pred), shape(on_true))) {
    cli_abort(c(
      "rank of {.arg pred} must be 0 or equal to rank of {.arg on_true}",
      x = "Got shapes {shapevec_repr(shape(pred))} and {shapevec_repr(shape(on_true))}."
    ))
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = on_true$type$dtype,
        shape = Shape(shape(on_true))
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
