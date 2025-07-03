#' @param ... integer
broadcast_shapes <- function(...) {
  if (...length() == 1L) {
    return(...elt(1))
  }

  .broadcast <- function(dim_x, dim_y) {
    if (length(dim_x) > length(dim_y)) {
      dim_y = c(rep(1L, length(dim_x) - length(dim_y)), dim_y)
    } else if (length(dim_x) < length(dim_y)) {
      dim_x = c(rep(1L, length(dim_y) - length(dim_x)), dim_x)
    }
    sapply(seq_along(dim_x), function(i) {
      dx = dim_x[[i]]
      dy = dim_y[[i]]
      if (dx != dy && dx != 1L && dy != 1L) {
        stop("Not broadcastable")
      }
      max(dx, dy)
    })
  }
  Reduce(.broadcast, list(...))
}

#' @param x,y ValueType
#' @return ValueType
infer_types_add <- function(x, y) {
  stopifnot(inherits(x@type, TensorType))
  stopifnot(inherits(y@type, TensorType))
  # TODO: Does stablehlo do implicit type casting?
  stopifnot(x@type@dtype == x@type@dtype)

  shapeout = broadcast_shapes(x@type@shape@dims, y@type@shape@dims)

  ValueType(TensorType(
    x@type@dtype,
    Shape(shapeout)
  ))
}