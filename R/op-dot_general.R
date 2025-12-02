OpDotGeneral <- new_Op("OpDotGeneral", "dot_general")

#' @param dot_dimension_numbers (`DotDimensionNumbers`)\cr
#'   The dot dimension number.
#' @rdname hlo_dot_general
#' @export
infer_types_dot_general <- function(
  lhs,
  rhs,
  dot_dimension_numbers
) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type@dtype == rhs@type@dtype)
  dim_lhs <- shape(lhs)
  dim_rhs <- shape(rhs)
  contracting_dims <- dot_dimension_numbers@contracting_dims
  batching_dims <- dot_dimension_numbers@batching_dims

  # stableHLO uses 0-based indexing
  dim_merge1 <- dim_lhs[contracting_dims[[1L]] + 1L]
  dim_merge2 <- dim_rhs[contracting_dims[[2L]] + 1L]
  dim_batch1 <- dim_lhs[batching_dims[[1L]] + 1L]
  dim_batch2 <- dim_rhs[batching_dims[[2L]] + 1L]
  if (!identical(dim_merge1, dim_merge2)) {
    cli_abort(c(
      x = "Contracting dimensions must be the same",
      i = format_shapes_msg("Got:", lhs = dim_lhs, rhs = dim_rhs)
    ))
  }
  if (!identical(dim_batch1, dim_batch2)) {
    cli_abort(c(
      x = "Batching dimensions must be the same",
      i = format_shapes_msg("Got:", lhs = dim_lhs, rhs = dim_rhs)
    ))
  }

  ii1 <- c(contracting_dims[[1L]], batching_dims[[1L]])
  dim_lhs_remaining <- if (length(ii1)) {
    dim_lhs[-(ii1 + 1L)]
  } else {
    dim_lhs
  }
  ii2 <- c(contracting_dims[[2L]], batching_dims[[2L]])
  dim_rhs_remaining <- if (length(ii2)) {
    dim_rhs[-(ii2 + 1L)]
  } else {
    dim_rhs
  }
  out_dim <- c(dim_batch1, dim_lhs_remaining, dim_rhs_remaining)

  ValueTypes(list(
    ValueType(TensorType(dtype = lhs@type@dtype, shape = Shape(out_dim)))
  ))
}

#' @title DotDimensionNumbers
#' @description
#' Represents the dot dimension numbers.
#' @param contracting_dims (`integer()`)\cr
#'   The contracting dimensions.
#' @param batching_dims (`integer()` | `NULL`)\cr
#'   The batching dimensions.
#' @export
DotDimensionNumbers <- new_class(
  "DotDimensionNumbers",
  properties = list(
    contracting_dims = S7::class_any,
    batching_dims = S7::class_any
  )
)

dot_general_impl <- hlo_fn(OpDotGeneral, infer_types_dot_general)

#' @templateVar mnemonic dot_general
#' @template op
#' @export
hlo_dot_general <- function(
  lhs,
  rhs,
  contracting_dims,
  batching_dims = NULL
) {
  dot_general_impl(
    values = list(
      lhs = lhs,
      rhs = rhs
    ),
    custom_attrs = list(
      dot_dimension_numbers = DotDimensionNumbers(
        contracting_dims = contracting_dims,
        batching_dims = batching_dims
      )
    )
  )
}

method(repr, DotDimensionNumbers) <- function(x) {
  str <- sprintf(
    "contracting_dims = [%s] x [%s]",
    paste0(x@contracting_dims[[1L]], collapse = ", "),
    paste0(x@contracting_dims[[2L]], collapse = ", ")
  )
  if (is.null(x@batching_dims)) {
    return(str)
  }
  sprintf(
    "batching_dims = [%s] x [%s], %s",
    paste0(x@batching_dims[[1L]], collapse = ", "),
    paste0(x@batching_dims[[2L]], collapse = ", "),
    str
  )
}

method(repr, OpDotGeneral) <- function(x, ...) {
  paste0(
    repr(x@outputs),
    " = ",
    "stablehlo.dot_general ",
    repr(x@inputs@values),
    ", ",
    repr(x@inputs@custom_attrs$dot_dimension_numbers),
    ": ",
    repr(x@signature)
  )
}
