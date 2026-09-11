#' @include repr.R
NULL

#' @title Shape
#' @description
#' Represents the shape of a tensor: the size of each axis, `NA` where a size
#' is only known at run time.
#'
#' A `Shape` *is* its integer vector, with a class attached, so `length()` is
#' the rank and `[` selects axes without unwrapping anything first.
#' @param dims (`integer()`)
#' @return `Shape`
#' @export
Shape <- function(dims = integer()) {
  sizes <- suppressWarnings(as.integer(dims))

  # `NA` here *means* dynamic, and every check downstream is built to accept it
  # and defer. So an `NA` that `as.integer()` invented -- from a value out of
  # integer range, an `Inf`/`NaN` produced by inference arithmetic, or a
  # non-numeric -- would not fail: it would quietly become a plausible-looking
  # dynamic program that only breaks at compile time. Catching it costs one
  # `anyNA()` on the way past, and nothing at all for a shape with no `NA`.
  if (anyNA(sizes)) {
    # Which `NA`s were asked for. `is.na()` is TRUE for `NaN` too, and `NaN`
    # is a conversion accident (inference arithmetic that divided by zero),
    # not a dynamic axis -- so it has to be excluded here.
    asked_for <- is.na(dims)
    if (is.double(dims)) {
      asked_for <- asked_for & !is.nan(dims)
    }
    invented <- is.na(sizes) & !asked_for
    if (any(invented)) {
      cli_abort(c(
        "{.arg dims} must be axis sizes representable as integers.",
        i = "{.val {NA}} is how a dynamic axis is spelled, so it cannot also
             stand for a size that could not be converted.",
        x = "Got {.val {dims[invented]}}."
      ))
    }
  }

  if (any(sizes[!is.na(sizes)] < 0L)) {
    cli_abort("Dimensions must be >= 0")
  }

  structure(sizes, class = "Shape")
}

# Comparing shapes with an operator is refused rather than answered. Once an
# axis size can be unknown (`NA`), "are these two shapes equal" is three
# questions that disagree on exactly the cases that matter, and an operator
# cannot say which one was meant:
#
#   identity        Is `?` the same *type* as `?`, and a different type from
#                   `3`? This is what buffer aliasing and donation need, and
#                   `identical()` is what answers it.
#   satisfiability  Could `?` be `3` at run time? This is what a constraint
#                   check needs -- refuse only what is certainly wrong -- and
#                   `possibly()` and its relations answer it.
#   provability     Is `?` *provably* `3`? No -- and two dynamic axes are not
#                   provably equal to each other either, since they may hold
#                   different sizes. `provably()` and its relations answer it.
#
# So the caller has to pick one, in writing, at the call site.
#
# The methods stay registered rather than being removed: a `Shape` is an
# integer vector, so with no method `==` would fall through to the elementwise
# default and quietly return a vector where a caller expects one answer.
error_shape_comparison <- function(op, call = rlang::caller_env()) {
  cli_abort(
    c(
      "{.code {op}} is not defined for a {.cls Shape}.",
      i = "An axis size may be {.val {NA}} (unknown until run time), and then
           whether two shapes are {.q equal} depends on whether you are asking
           about type identity or about what the sizes could be at run time.",
      i = "For type identity, compare the axis sizes:
           {.code identical(unclass(x), unclass(y))}.",
      i = "For satisfiability, which is what an inference constraint wants,
           unify the axis sizes with {.fun unify_shapes}: it refuses only a
           definite clash and returns the most-refined shape otherwise."
    ),
    call = call
  )
}

#' @export
`==.Shape` <- function(e1, e2) {
  error_shape_comparison("==")
}

#' @export
`!=.Shape` <- function(e1, e2) {
  error_shape_comparison("!=")
}

#' @export
repr.Shape <- function(x, ...) {
  dims <- unclass(x)
  if (length(dims) == 0L) {
    return("")
  }
  dims[is.na(dims)] <- "?"
  paste0(dims, collapse = "x")
}

#' @export
format.Shape <- function(x, ...) {
  paste0("(", repr(x), ")")
}

#' @export
print.Shape <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
#' @method dtype Constant
dtype.Constant <- function(x, ...) {
  x$type$dtype
}

#' @export
#' @method dtype FuncValue
dtype.FuncValue <- function(x, ...) {
  dtype(x$value_type)
}
