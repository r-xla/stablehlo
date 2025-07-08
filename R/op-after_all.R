#' @include op.R api.R
NULL

AfterAll <- new_Op("AfterAll", "after_all")

infer_types_after_all <- function(...) {
  ValueTypes(list(ValueType(TokenType())))
}
