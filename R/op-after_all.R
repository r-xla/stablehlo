#' @include op.R hlo.R
NULL

OpAfterAll <- new_Op("OpAfterAll", "after_all")

#' @rdname hlo_after_all
#' @export
infer_types_after_all <- function(...) {
  ValueTypes(list(ValueType(TokenType())))
}

hlo_after_all_impl <- hlo_fn(OpAfterAll, infer_types_after_all)

#' @templateVar mnemonic after_all
#' @template op
#' @export
hlo_after_all <- function(...) {
  hlo_after_all_impl(values = list(...))
}
