#' @include conditions.R
NULL

# Reasoning about axis sizes that are only known at run time.
#
# A shape is an `integer()` in which `NA_integer_` means "dynamic":
# a size the program learns at run time, rendered as `?` in the MLIR type.
# R's `NA` propagation is the semantics we want for *arithmetic* --
# `sum(c(3L, NA))` is `NA`, which is exactly concatenate's on-axis size -- and
# `identical()` is the structural identity that type equality needs. Comparison
# is the gap: R answers `NA` and then refuses to branch on it, so an inference
# function that compares sizes directly dies with "missing value where
# TRUE/FALSE needed".
#
# Three relations close that gap, and the whole point is that they are
# different questions and must not be spelled the same way:
#
#   possibly_*()    Could this hold at run time?  Used by the constraint checks:
#              refuse only what is *certainly* wrong and leave the rest to the
#              runtime, which can see the sizes.
#   identical()  Are these the same type?  Used for type identity (buffer
#              aliasing, `output_types`), where `?` must *not* match a known
#              size.
#   unify_shapes() What is the most we know?  Used to build result shapes: it
#              errors on a definite clash and otherwise returns the refined
#              shape, so a dynamic operand meeting a static one yields the
#              static size.

# Could this hold at run time? False only when every operand is known and the
# predicate is false on those known values.
possibly <- function(x) is.na(x) | x

# Does this provably hold? True only when the relation is known to hold, which
# is what a compiler decision needs when it is unsound otherwise.
provably <- function(x) !is.na(x) & x

# The two are duals, not complements: `possibly(x)` is `!provably(!x)`. So
# "possibly equal" is the negation of *provably unequal*, never of "possibly
# unequal" -- for `?` against `3`, both of those are true at once.
#
# That is the distinction the whole design rests on, and it is why anything
# reading "if these sizes differ, that is an error" becomes `provably_ne`, the
# workhorse of the family, and never a negated "possibly equal".
#
# The relations in use, each `possibly()` or `provably()` composed with an
# operator. Only the ones with a call site live here: the rest of the family is
# a line each and arrives with the op that needs it, so that reading this file
# tells you which questions the inference functions actually ask.
possibly_ge <- function(a, b) possibly(a >= b)
provably_eq <- function(a, b) provably(a == b)
provably_ne <- function(a, b) provably(a != b)
provably_gt <- function(a, b) provably(a > b)

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
#
# A known 0 is the one case `prod()` alone gets wrong: it annihilates, so a
# shape holding a 0 beside a `?` has provably 0 elements whatever the `?`
# turns out to be, where `prod(c(NA, 0L))` answers `NA` and defers a decidable
# check.
shape_nelts <- function(shape) {
  if (any(shape == 0L, na.rm = TRUE)) 0L else prod(shape)
}

provably_nelts_ne <- function(a, b) provably(shape_nelts(a) != shape_nelts(b))

# The most-refined shape consistent with both `a` and `b`, or an error if no
# shape is. `?` unifies with a known size to give that size, which is how a
# program that mixes a dynamic operand with a static one keeps a static result
# type.
#
# Rank is never unified: a shape's `length()` is a compile-time constant
# everywhere (we do not support unranked tensors), so a rank mismatch is a hard
# error rather than something to defer.
#
# Unification is associative, commutative and idempotent, which is what lets
# `unify_all_shapes()` fold it over a whole set.
unify_shapes <- function(
  a,
  b,
  arg_a = "lhs",
  arg_b = "rhs",
  call = rlang::caller_env()
) {
  if (length(a) != length(b)) {
    cli_abort(
      c(
        "{.arg {arg_a}} and {.arg {arg_b}} must have the same rank.",
        x = "Got shapes {shapevec_repr(a)} and {shapevec_repr(b)}."
      ),
      call = call
    )
  }
  if (length(a) == 0L) {
    return(integer())
  }
  clash <- provably(a != b)
  if (any(clash)) {
    axis <- which(clash)[[1L]] - 1L
    error_dim_size_mismatch(
      arg1 = arg_a,
      arg2 = arg_b,
      dim1 = axis,
      dim2 = axis,
      shape1 = a,
      shape2 = b,
      call = call
    )
  }
  ifelse(is.na(a), b, a)
}

# Unify a whole set of shapes. This -- not a pairwise "may be equal" -- is what
# a "these must all agree" check becomes: "may be equal" is not transitive, so
# folding it would accept `(3, ?, 4)` because each shape may match the first.
# Unification is associative, so the fold both validates and produces the
# result shape.
unify_all_shapes <- function(
  shapes,
  arg = "inputs",
  call = rlang::caller_env()
) {
  Reduce(
    function(acc, s) {
      if (length(acc) != length(s)) {
        # `error_dim_size_mismatch()` rather than a plain `cli_abort()`: the
        # ops that fold with this catch `ErrorDimSizeMismatch` to restate the
        # failure in their own words, and a rank mismatch is exactly the case
        # they most need to restate -- the shapes reaching here may be a
        # projection of the operands (concatenate folds the off-axis sizes).
        error_dim_size_mismatch(
          arg1 = arg,
          arg2 = arg,
          dim1 = 0L,
          dim2 = 0L,
          shape1 = acc,
          shape2 = s,
          call = call
        )
      }
      unify_shapes(acc, s, arg_a = arg, arg_b = arg, call = call)
    },
    shapes
  )
}

# Unify two tensor ValueTypes: same dtype, same rank, axis sizes unified.
# The refined type is returned, so `add(tensor<?xf32>, tensor<3xf32>)` has type
# `tensor<3xf32>`.
#
# A mismatch is reported as a whole-type error rather than a per-axis one:
# `unify_shapes()`'s per-axis message earns its keep where many shapes are folded
# together (reduce, concatenate), but for a two-operand op showing both types
# says more, and it is the wording anvl rewrites into its own vocabulary.
unify_vt <- function(
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
    !any(provably(a != b))
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

# There is deliberately no dual of this -- no operation giving the *least*
# specific type that every branch satisfies -- even though control flow looks
# like it wants one.
#
# The reasoning that suggests them is sound as far as it goes: only one branch
# of an `if` runs, so a result axis is known only where every branch knows it
# and they agree -- the widening, not the unification; and a `while` body may
# legitimately
# produce a type more refined than the carried one, since the loop forgets the
# extra knowledge next iteration.
#
# StableHLO does not permit either. SPEC requires equality: `if` (C2)
# `output_types(true_branch) = output_types(false_branch)`, `case` (C3)
# `same(output_types(branches...))`, `while` (C2) `body` has type
# `(T0, ..., TN-1) -> (T0, ..., TN-1)`. And a widened result is not merely
# unspec'd but unusable -- IREE lowers these to `scf.if`/`scf.while`, whose
# yielded type must match the region's declared type, so it refuses the
# program outright even though StableHLO's own verifier accepts it. A program
# that needs the widening has to do it explicitly, inside the branch.

# A shape vector that may carry dynamic axes. `assert_shapevec()` stays strict
# at every one of its call sites: where StableHLO lets a size vary it does so
# with a separate op that takes the sizes as an *operand*, so `NA` reaches an
# inference function only as a result-type hint, never as a rendered attribute.
assert_shapevec_dyn <- function(x) {
  assert_integerish(x, lower = 0)
}

# The size operand of the dynamic-op family: `output_shape`, `slice_sizes`,
# `edge_padding_low`, ... -- a rank-1 tensor carrying one element per axis of
# the shape it describes. Shared rather than written out at each site, so the
# three things that are easy to drop when a size moves from an attribute to an
# operand are checked in one place.
#
# The dtype is one of them. The static twins of these ops get it free from
# `assert_const(x, dtype = as_dtype("i64"))`; taking the sizes as an operand
# loses that, and SPEC types every one of them "1-dimensional tensor of
# integer type".
#
# `static_extent` follows StableHLO's ODS. Most of these operands are
# `HLO_StaticDimensionTensor`, so a `tensor<?xi64>` there can never be valid
# at any run-time size and is refused outright. `real_dynamic_slice` takes
# `HLO_DimensionTensor`, which permits a dynamic extent, so there the
# element-count check is the one that defers.
assert_size_operand <- function(
  x,
  naxes,
  describes = "the result",
  arg = rlang::caller_arg(x),
  static_extent = TRUE,
  call = rlang::caller_env()
) {
  assert_vt_has_ttype(x, "int", "uint", arg = arg, call = call)
  declared <- shape(x)
  if (length(declared) != 1L) {
    cli_abort(
      c(
        "{.arg {arg}} must be a rank-1 tensor.",
        x = "Got shape {shapevec_repr(declared)}."
      ),
      call = call
    )
  }
  if (static_extent && is.na(declared[[1L]])) {
    cli_abort(
      c(
        "{.arg {arg}} must have a statically known number of elements.",
        i = "StableHLO types it a statically shaped tensor, so its own extent
             cannot be the thing that is only known at run time -- only the
             sizes it carries can.",
        x = "Got shape {shapevec_repr(declared)}."
      ),
      call = call
    )
  }
  if (provably_ne(declared, naxes)) {
    cli_abort(
      c(
        "{.arg {arg}} must have one element per axis of {describes}.",
        x = "Got {vec_repr(declared)} elements for a rank-{naxes} {describes}."
      ),
      call = call
    )
  }
  invisible(NULL)
}

# The dynamic-op family's index operands must share one identical type
# (`AllTypesMatch` in the ODS; SPEC (C2) for dynamic_pad), which no per-operand
# check can see. `xs` is a named list, so the message can say which ones.
assert_size_operands_same_type <- function(xs, call = rlang::caller_env()) {
  strs <- vapply(xs, function(x) x$type$str, character(1))
  if (length(unique(strs)) > 1L) {
    cli_abort(
      c(
        "{.arg {names(xs)}} must all have the same type.",
        x = "Got {.val {strs}}."
      ),
      call = call
    )
  }
  invisible(NULL)
}
