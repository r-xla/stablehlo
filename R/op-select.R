#' @include op.R hlo.R
NULL

OpSelect <- new_Op("OpSelect", "select")

infer_types_select <- function(
  pred,
  on_true,
  on_false
) {
  stopifnot(inherits(pred@type, TensorType))
  assert_one_of(pred@type@dtype, BooleanType)
  stopifnot(inherits(on_true@type, TensorType))
  stopifnot(inherits(on_false@type, TensorType))

  result_dims <- shape(on_true)

  # (C0) implicit constraints: rank(on_true) = rank(on_false)
  if (!identical(shape(on_true), shape(on_false))) {
    cli_abort("on_false and on_true must have same dimension")
  }

  # (C1) rank(pred) = 0 or shape(pred) = shape(on_true)
  if (length(shape(pred)) != 0 && !identical(shape(pred), shape(on_true))) {
    cli_abort("rank of pred must be 0 or equal to rank of on_true")
  }

  # (C2) baseline_type(on_true) = baseline_type(on_false) = baseline_type(result)
  if (!identical(on_true@type@dtype, on_false@type@dtype)) {
    cli_abort("element types of on_true and on_false must be equal")
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = on_true@type@dtype,
        shape = Shape(result_dims)
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

method(repr, OpSelect) <- function(x) {
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
