#' @include repr.R
NULL

#' Create a new list_of class
#'
#' @param class_name Name of the new class
#' @param item_class Name of the class items should be (for documentation)
#' @param validator Optional function to validate items
#' @return Constructor function for the list_of subclass
#' @keywords internal
new_list_of <- function(class_name, item_class, validator = NULL) {
  # Return a constructor function
  function(items = list()) {
    checkmate::assert_list(items, item_class)

    # Run custom validator if provided
    if (!is.null(validator)) {
      validator <- get("validator") # r-cmd-check NOTE: undefined global
      err <- validator(items)
      if (!checkmate::test_null(err)) {
        cli_abort(err)
      }
    }

    structure(
      list(items = items),
      class = c(class_name, "list_of")
    )
  }
}

#' @export
`==.list_of` <- function(e1, e2) {
  length(e1$items) == length(e2$items) &&
    all(
      vapply(
        seq_along(e1$items),
        function(i) {
          e1$items[[i]] == e2$items[[i]]
        },
        logical(1)
      )
    )
}

#' @export
`!=.list_of` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
length.list_of <- function(x) {
  length(x$items)
}
