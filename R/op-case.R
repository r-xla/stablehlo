#' @include op.R hlo.R
NULL

OpCase <- new_Op("OpCase", "case")

#' @rdname hlo_case
#' @export
infer_types_case <- function(index, ...) {
  branches <- list(...)

  # (I1) index is 0-dim si32
  stopifnot(inherits(index@type, TensorType))
  stopifnot(length(index@type@shape@dims) == 0L)
  stopifnot(identical(index@type@dtype, IntegerType(32L)))

  # (C1) 0 < size(branches)
  stopifnot(length(branches) > 0L)

  # (C2) input_types(branches...) = [] and
  # (C3) same(output_types(branches...))
  get_branch_out_types <- function(branch) {
    stopifnot(inherits(branch, Func))
    stopifnot(length(branch@inputs@items) == 0L)
    func_output_types(branch)
  }

  out_types_list <- lapply(branches, get_branch_out_types)

  for (i in seq_along(out_types_list[-1L])) {
    stopifnot(identical(out_types_list[[i]], out_types_list[[1L]]))
  }

  ValueTypes(out_types_list[[1L]])
}

hlo_case_impl <- hlo_fn(OpCase, infer_types_case)

#' @templateVar mnemonic case
#' @template op
#' @export
hlo_case <- function(index, ...) {
  branches <- list(...)
  hlo_case_impl(
    values = list(index = index),
    funcs = branches
  )
}
