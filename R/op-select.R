#' @include op.R hlo.R
NULL

OpSelect <- new_Op("OpSelect", "select")

#' @rdname hlo_select
#' @export
infer_types_select <- function(
  pred,
  on_true,
  on_false
) {
  assert_vt_equal(on_true, on_false)
  assert_vt_has_ttype(pred, BooleanType)

  if (ndims(pred) != 0 && !identical(shape(pred), shape(on_true))) {
    cli_abort("rank of pred must be 0 or equal to rank of on_true")
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = on_true@type@dtype,
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
  on_false
) {
  hlo_select_impl(
    values = list(pred = pred, on_true = on_true, on_false = on_false)
  )
}

method(repr, OpSelect) <- function(x, ...) {
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
