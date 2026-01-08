#' @include op.R hlo.R
NULL

OpCompare <- new_Op("OpCompare", "compare")

#' @rdname hlo_compare
#' @export
infer_types_compare <- function(
  lhs,
  rhs,
  comparison_direction,
  compare_type
) {
  assert_vts_are_tensors(lhs, rhs)
  assert_vt_equal(lhs, rhs)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = BooleanType(),
        shape = Shape(shape(lhs))
      )
    )
  ))
}

hlo_compare_impl <- hlo_fn(
  OpCompare,
  infer_types_compare
)

#' @templateVar mnemonic compare
#' @template op
#' @export
hlo_compare <- function(
  lhs,
  rhs,
  comparison_direction,
  compare_type
) {
  hlo_compare_impl(
    values = list(lhs = lhs, rhs = rhs),
    custom_attrs = list(
      comparison_direction = comparison_direction,
      compare_type = compare_type
    )
  )
}

#' @export
repr.OpCompare <- function(
  x,
  toplevel = TRUE,
  simplify_dense = TRUE,
  ...
) {
  paste0(
    repr(x$outputs),
    " = stablehlo.compare ",
    x$inputs$custom_attrs$comparison_direction,
    ", ",
    repr(x$inputs$values),
    ", ",
    x$inputs$custom_attrs$compare_type,
    " : ",
    repr(x$signature)
  )
}
