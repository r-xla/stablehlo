#' @include op.R hlo.R
NULL

OpAfterAll <- new_Op("OpAfterAll", "after_all")

#' @rdname hlo_after_all
#' @export
infer_types_after_all <- function(...) {
  # (I1) `inputs` is a "variadic number of `token`". Unchecked, a tensor is
  # rendered straight into the operand list and only MLIR refuses it.
  inputs <- list(...)
  for (i in seq_along(inputs)) {
    if (!inherits(inputs[[i]]$type, "TokenType")) {
      error_unexpected_list_type(
        arg = "inputs",
        index = i - 1L,
        expected = "must be tokens",
        actual = repr(inputs[[i]]$type)
      )
    }
  }
  ValueTypes(list(ValueType(TokenType())))
}

hlo_after_all_impl <- hlo_fn(OpAfterAll, infer_types_after_all)

#' @templateVar mnemonic after_all
#' @template op
#' @export
hlo_after_all <- function(..., output_types = NULL) {
  hlo_after_all_impl(values = list(...), output_types = output_types)
}
