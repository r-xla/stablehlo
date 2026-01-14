#' @include op.R hlo.R
NULL

OpCase <- new_Op("OpCase", "case")

#' @rdname hlo_case
#' @export
infer_types_case <- function(index, ...) {
  branches <- list(...)
  assert_vt_has_ttype(index, IntegerType(32L))

  if (length(branches) == 0L) {
    cli_abort("branches must be a non-empty list")
  }

  # (C2)
  get_branch_out_types <- function(branch) {
    if (!test_class(branch, "Func")) {
      cli_abort("branches must be a list of Func objects")
    }
    if (length(branch$inputs) != 0L) {
      cli_abort("branch functions must not have inputs")
    }
    func_output_types(branch)
  }

  out_types_list <- lapply(branches, get_branch_out_types)

  # (C3)
  for (i in seq_along(out_types_list[-1L])) {
    if (!identical(out_types_list[[i]], out_types_list[[1L]])) {
      cli_abort("all branch functions must have the same output types")
    }
  }

  # (C4)
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
