#' @include conditions.R
NULL

# Reasoning about axis sizes that are only known at run time.
#
# A shape is an `integer()` in which `NA_integer_` means "dynamic":
# a size the program learns at run time, rendered as `?` in the MLIR type.
# R's `NA` propagation is already the semantics we want for *arithmetic* --
# `sum(c(3L, NA))` is `NA`, which is exactly concatenate's on-axis size -- and
# `identical()` is already the structural identity that type equality needs.
# The one gap is comparison, where R answers `NA` and then refuses to branch on
# it, which is why tracing a dynamic program today dies with "missing value
# where TRUE/FALSE needed" from inside an inference function.
#
# Three relations close that gap, and the whole point is that they are
# different questions and must not be spelled the same way:
#
#   may_*()    Could this hold at run time?  Used by the constraint checks:
#              refuse only what is *certainly* wrong and leave the rest to the
#              runtime, which can see the sizes.
#   identical()  Are these the same type?  Used for type identity (buffer
#              aliasing, `output_types`), where `?` must *not* match a known
#              size. Unchanged, and deliberately so.
#   shape_meet() What is the most we know?  Used to build result shapes: it
#              errors on a definite clash and otherwise returns the refined
#              shape, so a dynamic operand meeting a static one yields the
#              static size.

# A predicate over axis sizes is false only when every operand is known and the
# predicate is false on those known values.
may <- function(x) is.na(x) | x

# The dual: true only when the relation is known to hold. For a compiler
# decision that is unsound unless a relation provably holds.
must <- function(x) !is.na(x) & x

# The relations in use, each `may()` or `must()` composed with an operator.
# Only the ones with a call site live here: the rest of the family is a line
# each and arrives with the op that needs it, so that reading this file tells
# you which questions the inference functions actually ask.
#
# `must_ne` is the workhorse. Anything that reads "if these sizes differ, that
# is an error" becomes `must_ne`, never `!may_eq`: the two are not complements,
# because for `?` against `3` both "may be equal" and "may differ" are true.
may_eq <- function(a, b) may(a == b)
may_ge <- function(a, b) may(a >= b)
must_eq <- function(a, b) must(a == b)
must_ne <- function(a, b) must(a != b)
must_gt <- function(a, b) must(a > b)

# Element counts, for the ops that relate a shape to a *total size* rather than
# axis by axis (reshape).
#
# `prod()` over a vector containing `NA` is already `NA`, which is the right
# answer -- the count is unknown -- so the only thing needed is the comparison
# discipline below: two counts are certainly unequal only when both are known
# and they differ. A dynamic axis on either side defers the check to the
# runtime.
#
# The empty shape is a scalar, whose element count is 1 (`prod(integer())`),
# which is what reshape between `tensor<1xf32>` and `tensor<f32>` relies on.
shape_nelts <- function(shape) prod(shape)

must_nelts_ne <- function(a, b) must(shape_nelts(a) != shape_nelts(b))

# The most-refined shape consistent with both `a` and `b`, or an
# error if no vector is. `?` meets a known size to that size, which is how a
# program that mixes a dynamic operand with a static one keeps a static result
# type.
#
# Rank is never part of the meet: a shape's `length()` is a compile-time constant
# everywhere (we do not support unranked tensors), so a rank mismatch is a hard
# error rather than something to defer.
shape_meet <- function(
  a,
  b,
  arg1 = "lhs",
  arg2 = "rhs",
  call = rlang::caller_env()
) {
  if (length(a) != length(b)) {
    cli_abort(
      c(
        "{.arg {arg1}} and {.arg {arg2}} must have the same rank.",
        x = "Got shapes {shapevec_repr(a)} and {shapevec_repr(b)}."
      ),
      call = call
    )
  }
  if (length(a) == 0L) {
    return(integer())
  }
  clash <- must(a != b)
  if (any(clash)) {
    axis <- which(clash)[[1L]] - 1L
    error_dim_size_mismatch(
      arg1 = arg1,
      arg2 = arg2,
      dim1 = axis,
      dim2 = axis,
      shape1 = a,
      shape2 = b,
      call = call
    )
  }
  ifelse(is.na(a), b, a)
}

# The meet of a list of shapes. This -- not a pairwise `may_eq` --
# is what a "these must all agree" check becomes: `may_eq` is not transitive,
# so folding it would accept `(3, ?, 4)` because each shape may match the
# first. `shape_meet` is associative, so the fold both validates and produces the
# result shape.
shapes_meet <- function(shapes, arg = "inputs", call = rlang::caller_env()) {
  Reduce(
    function(acc, s) shape_meet(acc, s, arg1 = arg, arg2 = arg, call = call),
    shapes
  )
}

# The meet of two tensor ValueTypes: same dtype, same rank, axis sizes met.
# The refined type is returned, so `add(tensor<?xf32>, tensor<3xf32>)` has type
# `tensor<3xf32>`.
#
# A mismatch is reported as a whole-type error rather than a per-axis one:
# `shape_meet()`'s per-axis message earns its keep where many shapes are folded
# together (reduce, concatenate), but for a two-operand op showing both types
# says more, and it is the wording anvl rewrites into its own vocabulary.
vt_meet <- function(
  x,
  y,
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  a <- shape(x)
  b <- shape(y)
  compatible <- x$type$dtype == y$type$dtype &&
    length(a) == length(b) &&
    !any(must(a != b))
  if (!compatible) {
    cli_abort(
      c(
        "{.arg {arg_x}} and {.arg {arg_y}} must have the same tensor type.",
        x = "Got {.val {x$type}} and {.val {y$type}}."
      ),
      call = call
    )
  }
  sizes <- if (length(a) == 0L) integer() else ifelse(is.na(a), b, a)
  ValueType(TensorType(dtype = x$type$dtype, shape = Shape(sizes)))
}

# The two relations control flow needs, and they run in opposite directions to
# `shape_meet`. Worth being explicit about why, because reaching for the meet
# here would be unsound in one case and merely wrong in the other.
#
# `vt_refines(x, y)` -- "x says at least as much as y". Same dtype, same rank,
# and every axis y knows, x knows and agrees on; where y is `?`, x may be
# anything. This is what a `while` body's output must satisfy against the
# declared carried type. The asymmetry is the point: a loop that declares
# `tensor<?xf32>` and whose body produces `tensor<3xf32>` is fine, because the
# loop simply forgets what one iteration happened to know. The reverse -- a
# loop declaring `tensor<3xf32>` whose body produces `tensor<?xf32>` -- is not
# fine, because the body may produce a 4 on some iteration and the declared
# type would be a claim we cannot back. `may_eq` would accept both.
vt_refines <- function(x, y) {
  a <- shape(x)
  b <- shape(y)
  if (x$type$dtype != y$type$dtype || length(a) != length(b)) {
    return(FALSE)
  }
  # Every axis known in `b` must be known and equal in `a`.
  known_in_b <- !is.na(b)
  !anyNA(a[known_in_b]) && all(a[known_in_b] == b[known_in_b])
}

# `shape_join()` -- the least-refined shape both `a` and `b` refine, i.e. what
# is still true whichever of them a value came from. An axis survives only if
# both agree on it; otherwise it widens to `?`.
#
# This is the *dual* of shape_meet, and `if` / `case` are where it belongs: only
# one branch runs, so a result axis is known only when both branches know it
# and say the same thing. Using the meet there would be plain wrong -- it would
# report an axis as `3` on the strength of one branch alone.
#
# Two known-but-different sizes stay an error rather than widening to `?`:
# StableHLO requires the branches to agree, and silently widening would turn a
# program bug into a dynamic shape.
shape_join <- function(
  a,
  b,
  arg1 = "lhs",
  arg2 = "rhs",
  call = rlang::caller_env()
) {
  if (length(a) != length(b)) {
    cli_abort(
      c(
        "{.arg {arg1}} and {.arg {arg2}} must have the same rank.",
        x = "Got shapes {shapevec_repr(a)} and {shapevec_repr(b)}."
      ),
      call = call
    )
  }
  if (length(a) == 0L) {
    return(integer())
  }
  clash <- must_ne(a, b)
  if (any(clash)) {
    axis <- which(clash)[[1L]] - 1L
    error_dim_size_mismatch(
      arg1 = arg1,
      arg2 = arg2,
      dim1 = axis,
      dim2 = axis,
      shape1 = a,
      shape2 = b,
      call = call
    )
  }
  ifelse(is.na(a) | is.na(b), NA_integer_, a)
}

# A shape vector that may carry dynamic axes. `assert_shapevec()` stays strict
# at every one of its call sites: where StableHLO lets a size vary it does so
# with a separate op that takes the sizes as an *operand*, so `NA` reaches an
# inference function only as a result-type hint, never as a rendered attribute.
assert_shapevec_dyn <- function(x) {
  assert_integerish(x, lower = 0)
}
