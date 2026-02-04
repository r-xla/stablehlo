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
  get_branch_out_types <- function(branch, index) {
    if (!test_class(branch, "Func")) {
      error_unexpected_list_type(
        arg = "branches",
        index = index,
        expected = "must be a Func",
        actual = class(branch)[1]
      )
    }
    if (length(branch$inputs) != 0L) {
      n <- length(branch$inputs)
      error_unexpected_list_type(
        arg = "branches",
        index = index,
        expected = "must not have inputs",
        actual = paste0(n, " input", if (n != 1L) "s"),
        call = call
      )
    }
    func_output_types(branch)
  }

  out_types_list <- lapply(seq_along(branches), function(i) {
    get_branch_out_types(branches[[i]], i - 1L)
  })

  # (C3)
  if (length(unique(out_types_list)) != 1L) {
    branch_types <- vapply(out_types_list, function(types) {
      vapply(types, repr, character(1))
    }, character(1))
    cli_abort(c(
      "All branch functions must have the same output types.",
      x = "Got {branch_types}."
    ))
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
