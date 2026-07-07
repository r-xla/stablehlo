#' @include repr.R
#' @include assert.R
NULL

#' @title ValueId
#' @description
#' This represents the name of a [`ValueType`].
#' @param id (`character(1)` or `NULL`)\cr
#'   Either a fixed name or `NULL` (default). A `NULL` id is assigned a numeric
#'   name lazily when the program is rendered ([`repr()`]), in the order ids
#'   first appear: `%0`, `%1`, ..., skipping any integer already claimed by a
#'   named id in the same program (e.g. an input called `"2"`).
#' @return (`ValueId`)
#' @export
ValueId <- function(id = NULL) {
  if (is.null(id)) {
    # Auto ids carry a mutable cell; the printed number is assigned at repr
    # time (see repr.ValueId / id_number), not at creation time.
    structure(list(id = new.env(parent = emptyenv())), class = "ValueId")
  } else {
    assert_valid_id(id)
    structure(list(id = id), class = "ValueId")
  }
}

#' @export
`==.ValueId` <- function(e1, e2) {
  identical(e1$id, e2$id)
}

#' @export
# jarl-ignore comparison_negation: != must delegate to == for S3 consistency
`!=.ValueId` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
repr.ValueId <- function(x, ...) {
  render_id(x)
}

# Render a single ValueId as "%name"/"%N", bypassing S3 dispatch (hot path).
render_id <- function(id) {
  cell <- id$id
  if (is.character(cell)) {
    paste0("%", cell)
  } else {
    paste0("%", id_number(cell))
  }
}

# --- Repr-time value-id numbering -------------------------------------------
#
# Auto ids are numbered when first rendered, sharing one counter per program
# (installed by repr.Func in globals[["REPR_IDS"]]). `taken` holds the integers
# already used by named ids, so an auto id never collides with a name like
# `%2`: that specific integer is skipped, without inflating the counter for a
# large name such as `%1000000`.

new_repr_ids <- function(taken = integer()) {
  st <- new.env(parent = emptyenv())
  st$map <- utils::hashtab()
  st$counter <- 0L
  st$taken <- taken
  st
}

# Install a numbering scope, returning the previously active one so a caller
# can restore it via pop_repr_ids() on exit.
push_repr_ids <- function(taken) {
  prev <- globals[["REPR_IDS"]]
  globals[["REPR_IDS"]] <- new_repr_ids(taken)
  prev
}

pop_repr_ids <- function(prev) {
  globals[["REPR_IDS"]] <- prev
}

# Number of an auto id's cell, assigned on first request and cached. Outside
# a numbering scope (a bare repr() of a single ValueId, e.g. for display)
# there is nothing to number against, so show %0 without leaking global state.
id_number <- function(cell) {
  st <- globals[["REPR_IDS"]]
  if (is.null(st)) {
    stop("Internal error")
  }
  cached <- st$map[[cell]]
  if (!is.null(cached)) {
    return(cached)
  }
  n <- st$counter
  taken <- st$taken
  while (n %in% taken) {
    n <- n + 1L
  }
  st$counter <- n + 1L
  s <- as.character(n)
  st$map[[cell]] <- s
  s
}

# Render a list of ValueIds as a comma-separated "%a, %b" operand/result list.
ids_str <- function(ids) {
  if (length(ids) == 0L) {
    return("")
  }
  paste0(vapply(ids, render_id, character(1)), collapse = ", ")
}
