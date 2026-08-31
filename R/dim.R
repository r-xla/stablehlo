#' @include conditions.R
NULL

# Reasoning about dimensions that are only known at run time.
#
# A dimension vector is an `integer()` in which `NA_integer_` means "dynamic":
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
#   dim_meet() What is the most we know?  Used to build result shapes: it
#              errors on a definite clash and otherwise returns the refined
#              shape, so a dynamic operand meeting a static one yields the
#              static size.

# A predicate over dimensions is false only when every operand is known and the
# predicate is false on those known values.
may <- function(x) is.na(x) | x

# The dual: true only when the relation is known to hold. For a compiler
# decision that is unsound unless a relation provably holds.
must <- function(x) !is.na(x) & x

# The equality and ordering predicates the checks need today. The rest of the
# family (`may_ne`, `may_lt`, ...) is `may()` composed with the operator and is
# a line each; they arrive with the ops that need them rather than sitting here
# unused.
may_eq <- function(a, b) may(a == b)
may_ge <- function(a, b) may(a >= b)

# The most-refined dimension vector consistent with both `a` and `b`, or an
# error if no vector is. `?` meets a known size to that size, which is how a
# program that mixes a dynamic operand with a static one keeps a static result
# type.
#
# Rank is never part of the meet: `length(dims)` is a compile-time constant
# everywhere (we do not support unranked tensors), so a rank mismatch is a hard
# error rather than something to defer.
dim_meet <- function(
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

# The meet of a list of dimension vectors. This -- not a pairwise `may_eq` --
# is what a "these must all agree" check becomes: `may_eq` is not transitive,
# so folding it would accept `(3, ?, 4)` because each shape may match the
# first. `dim_meet` is associative, so the fold both validates and produces the
# result shape.
shapes_meet <- function(shapes, arg = "inputs", call = rlang::caller_env()) {
  Reduce(
    function(acc, s) dim_meet(acc, s, arg1 = arg, arg2 = arg, call = call),
    shapes
  )
}

# The meet of two tensor ValueTypes: same dtype, same rank, dimensions met.
# The refined type is returned, so `add(tensor<?xf32>, tensor<3xf32>)` has type
# `tensor<3xf32>`.
#
# A mismatch is reported as a whole-type error rather than a per-axis one:
# `dim_meet()`'s per-axis message earns its keep where many shapes are folded
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
  dims <- if (length(a) == 0L) integer() else ifelse(is.na(a), b, a)
  ValueType(TensorType(dtype = x$type$dtype, shape = Shape(dims)))
}

# A shape vector that may carry dynamic axes. `assert_shapevec()` stays strict
# at every one of its call sites: where StableHLO lets a size vary it does so
# with a separate op that takes the sizes as an *operand*, so `NA` reaches an
# inference function only as a result-type hint, never as a rendered attribute.
assert_shapevec_dyn <- function(x) {
  assert_integerish(x, lower = 0)
}
