#' @include op.R hlo.R
NULL

# The func-level `return` line; OpInputFunc rewrites it to
# `stablehlo.return` when the func is embedded as a region.
render_return <- function(ctx) {
  paste0(
    "return ",
    ctx$values_str,
    " : ",
    paste0(ctx$in_type_strs, collapse = ", ")
  )
}

# Technically this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
OpReturn <- new_Op("OpReturn", "return", render = render_return)

#' @rdname hlo_return
#' @export
infer_types_return <- function(...) {
  assert_vts_are_tensors(...)
  ValueTypes()
}


hlo_return_impl <- hlo_fn(OpReturn, infer_types_return, TRUE)

#' @title Return Values
#' @description
#' Specifies the return values of a [`Func`] and finalize it.
#' @param ... ([`FuncValue`])\cr
#'   Return values. There must be at least one.
#' @template param_func
#' @return ([`Func`])
#' @export
hlo_return <- function(..., func = .current_func()) {
  dots <- list(...)
  if (!length(dots)) {
    stop("hlo_return must have at least one argument")
  }
  output_count <- length(dots)
  alias_indices <- vapply(
    func$inputs[
      !vapply(func$inputs, \(x) is.null(x$alias), logical(1))
    ],
    \(x) as.integer(x$alias),
    integer(1)
  )
  if (any(alias_indices < 0L | alias_indices >= output_count)) {
    error_index_out_of_bounds(
      arg = "alias_indices",
      lower = 0L,
      upper = output_count
    )
  }
  if (anyDuplicated(alias_indices)) {
    cli_abort(c(
      "Alias indices must be unique",
      x = "Got {vec_repr(alias_indices)}."
    ))
  }
  func <- hlo_return_impl(values = dots)
  maybe_restore_previous_func()
  if (func$id == FuncId("main")) {
    finalize_module()
  }
  return(func)
}
