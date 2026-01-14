#' @include repr.R
#' @include assert.R
NULL

#' @title ValueId
#' @description
#' This represents the name of a [`ValueType`].
#' @param id (`character(1)` or [`environment`])\cr
#'   Either a fixed name or an environment.
#'   If using an environment (default), the name will be generated automatically
#'   when calling [`repr()`], i.e. the first value id will be `%0`, the second `%1`, etc..
#' @return (`ValueId`)
#' @export
ValueId <- function(id = NULL) {
  if (!is.null(id)) {
    assert_valid_id(id)
  } else {
    id <- new.env(size = 0L)
  }

  structure(
    list(id = id),
    class = "ValueId"
  )
}

#' @export
`==.ValueId` <- function(e1, e2) {
  identical(e1$id, e2$id)
}

#' @export
`!=.ValueId` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
repr.ValueId <- function(x, ...) {
  name <- if (is.environment(x$id)) {
    repr_env2name(x$id)
  } else {
    x$id
  }
  paste0("%", name)
}

repr_env2name <- function(x) {
  # fmt: skip
  FUNC_ENV$vars[[x]] %??% {
    str <- as.character(FUNC_ENV$counter)
    FUNC_ENV$counter <- FUNC_ENV$counter + 1L
    FUNC_ENV$vars[[x]] <- str
    str
  }
}
