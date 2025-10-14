#' @include op.R hlo.R
NULL

OpWhile <- new_Op("OpWhile", "while")

infer_types_while <- function(..., cond, body) {
  value_types <- list(...)

  if (length(value_types) == 0L) {
    cli_abort("hlo_while must have at least one operand")
  }

  # (C1) cond has type (T0, ..., TN-1) -> tensor<i1>
  stopifnot(length(cond@outputs@items) == 1L)
  cond_out <- cond@outputs@items[[1L]]@type
  stopifnot(inherits(cond_out@type, TensorType))
  stopifnot(length(cond_out@type@shape@dims) == 0L)
  stopifnot(identical(cond_out@type@dtype, BooleanType()))

  # (C2) body has type (T0, ..., TN-1) -> (T0, ..., TN-1)
  body_out_types <- lapply(body@outputs@items, function(x) x@type)
  stopifnot(length(body_out_types) == length(value_types))
  for (i in seq_along(value_types)) {
    stopifnot(body_out_types[[i]] == value_types[[i]])
  }

  # (C3) result types equal operand types
  ValueTypes(value_types)
}

hlo_while_impl <- hlo_fn(OpWhile, infer_types_while)

#' @templateVar mnemonic while
#' @template op
#' @export
hlo_while <- function(..., cond, body) {
  hlo_while_impl(
    values = list(...),
    funcs = list(cond = cond, body = body)
  )
}
