#' @include op.R hlo.R
NULL

AfterAll <- new_Op("AfterAll", "after_all")

infer_types_after_all <- function(...) {
  ValueTypes(list(ValueType(TokenType())))
}

hlo_after_all_impl <- hlo_fn(AfterAll, infer_types_after_all)

#' @title After all
#' @param ... [`FuncVariable`]
#' @return [`FuncVariable`]
#' @export
hlo_after_all <- function(...) {
  hlo_after_all_impl(values = list(...))
}
