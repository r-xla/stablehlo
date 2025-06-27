#' @include types.R
NULL

Tracer <- new_class(
  "Tracer",
  properties = list(
    type = ValueType
  )
)

method(repr, Tracer) <- function(x) {
  paste0(
    "Tracer<",
    repr(x@type),
    ">"
  )
}


