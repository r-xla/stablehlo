# Returns a constructor for a typed list. The constructor checks that `items`
# is a list and runs the optional `validator` (which returns `NULL` on success
# or a `cli_abort()`-style message), but does not check the class of each
# element -- this keeps it cheap enough for hot paths like IR construction.
# `item_class` is documentation only.
new_list_of <- function(class_name, item_class, validator = NULL) {
  classes <- c(class_name, "list_of", "list")
  # Two closure bodies rather than one with a `!is.null(validator)` guard: the
  # unvalidated constructor then has no `validator` call to skip at run time,
  # and `codetools` sees no call to a non-function binding.
  if (is.null(validator)) {
    return(function(items = list()) {
      if (!is.list(items)) {
        cli_abort(
          "`items` must be a list, not {.cls {class(items)[[1L]]}}"
        )
      }
      structure(items, class = classes)
    })
  }
  function(items = list()) {
    if (!is.list(items)) {
      cli_abort(
        "`items` must be a list, not {.cls {class(items)[[1L]]}}"
      )
    }
    err <- validator(items)
    if (!is.null(err)) {
      cli_abort(err)
    }
    structure(items, class = classes)
  }
}

#' @export
`==.list_of` <- function(e1, e2) {
  length(e1) == length(e2) &&
    all(
      vapply(
        seq_along(e1),
        function(i) {
          e1[[i]] == e2[[i]]
        },
        logical(1)
      )
    )
}

#' @export
`!=.list_of` <- function(e1, e2) {
  length(e1) != length(e2) ||
    any(
      vapply(
        seq_along(e1),
        function(i) {
          e1[[i]] != e2[[i]]
        },
        logical(1)
      )
    )
}

#' @export
length.list_of <- function(x) {
  length(unclass(x))
}
