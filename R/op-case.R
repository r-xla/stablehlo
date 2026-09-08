#' @include op.R hlo.R
NULL

OpCase <- new_Op("OpCase", "case")

#' @rdname hlo_case
#' @export
infer_types_case <- function(index, ...) {
  branches <- list(...)
  assert_vt_has_ttype(index, as_dtype("i32"))

  if (length(branches) == 0L) {
    cli_abort("branches must be a non-empty list")
  }

  # (C2)
  get_branch_out_types <- function(branch, index) {
    if (!inherits(branch, "Func")) {
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

  # (C3) `unique()` compares the types structurally, which is too strict once a
  # size can be dynamic: one branch returning `tensor<?xf32>` where another
  # returns `tensor<3xf32>` is a legal `case`, and its result is `?`. So the
  # branches only have to be *compatible*, and the result is the join of them
  # all -- an axis is known only where every branch knows it and they agree.
  # Folding the join is what makes that transitive; comparing each branch
  # against the first would not be.
  error_branches_differ <- function(call = rlang::caller_env()) {
    # nolint next
    branch_types <- vapply(
      out_types_list,
      function(types) {
        paste(vapply(types, repr, character(1)), collapse = ", ")
      },
      character(1)
    )
    cli_abort(
      c(
        "All branch functions must have the same output types.",
        x = "Got {branch_types}."
      ),
      call = call
    )
  }

  n_out <- length(out_types_list[[1L]])
  if (!all(lengths(out_types_list) == n_out)) {
    error_branches_differ()
  }
  # (C3) `same(output_types(branches...))` -- equality across every branch,
  # not a join of them. See the note in `infer_types_if()`: widening a branch
  # that knows a size against one that does not is both a spec violation and
  # something IREE cannot lower.
  for (k in seq_len(n_out)) {
    reference <- out_types_list[[1L]][[k]]
    for (ts in out_types_list[-1L]) {
      if (ts[[k]] != reference) {
        error_branches_differ()
      }
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
