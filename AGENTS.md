@../claude-config/CLAUDE.md

## Package Overview

`stablehlo` is an R package that allows to create StableHLO programs, a portable computation representation used in machine learning. It allows creating, manipulating, and transforming StableHLO operations in R.
The Func object uses reference semantic, while other objects use value semantics.

## Design

The builder is optimized for low per-op overhead (lowering a graph should stay
in the same order of magnitude as `pjrt_compile()`):

* **Lazy rendering**: each op is stored on its func as a deferred
  `list(render, ctx)` pair via `func_emit()`, appended to the func's `buf`
  (a `fastmap::fastqueue`). The queue gives amortised-O(1) append that never
  copies earlier ops — note that growing an ordinary R list held in a field
  (`buf$ops[[n]] <- item`) is *not* O(1): the list is copy-on-modify and gets
  duplicated on every append, so it degrades to O(n^2). The MLIR text line is
  produced by `render(ctx)` at `repr()` time, in `func_lines()`, front-to-back
  (`buf$as_list()` yields ops in emission order). There is no op-record tree to
  walk; a single `repr()` per program means deferring the render costs nothing
  over rendering eagerly.
* **Repr-time value ids**: an auto SSA id (`ValueId()`) carries a mutable cell
  and gets its number when first rendered — in appearance order (`%0`, `%1`,
  ...), sharing one counter per program installed by `repr.Func`. Numbering
  skips integers already claimed by named ids (`collect_named_numeric()`), so
  an input named `"2"` does not collide with `%2` and a large name like
  `%1000000` does not inflate the counter. Region funcs rendered within the
  program share the scope, so ids stay unique across embedded regions.
* **Ops are descriptors, not objects**: `new_Op()` returns a lightweight
  descriptor (mnemonic, dialect, optional `render` function). `hlo_fn()` runs
  type inference, draws output ids, precomputes the id-independent strings
  (type strings, `sig_str`, `attrs_str`) and stores the `ValueId` objects and
  region funcs in a `ctx` list. At repr, `finalize_render()` fills the
  id-dependent strings (`outputs_str`, `values_str`, `funcs_str`) and calls
  the op's `render` function (default: `render_op_default()`, assembly or
  generic format). Ops with a custom MLIR syntax (e.g. `dot_general`,
  `custom_call`) define their own render function next to their `new_Op()`
  call.
* **Cached type strings**: `TensorType` renders `tensor<...>` once at creation
  and stores it in `$str`; equality and all rendering reuse it.
* **Known output types**: the `hlo_*` builders of common ops accept
  `output_types` (a list of `ValueType`). When provided, `hlo_fn()` skips
  type inference and its input validation entirely — used by lowerings
  (e.g. anvl) that already ran inference at trace time.
* Constructors on the hot path (`FuncValue`, `FuncInput`, `TensorType`, ...)
  do not validate their inputs; validation happens in `hlo_fn()` and the
  `infer_types_*` functions. Prefer `inherits()` over `checkmate` helpers in
  per-op code paths.

### Dynamic axis sizes

A `Shape` *is* an integer vector of axis sizes with a class attached (not a
list wrapping one), and `NA_integer_` in it means *dynamic*: a size the program
only learns at run time, rendered as `?` in the MLIR type. `length(shape)` is
the rank, which is never dynamic -- unranked tensors are not supported, so a
rank mismatch stays a hard error everywhere.

`R/shape-algebra.R` holds the reasoning that makes this work, and the point of
it is that "are these sizes equal" splits into three questions that must not be
spelled the same way:

* `possibly_*()` / `provably_*()` -- could this hold at run time, and does it
  provably hold. Constraint checks use `provably_*()`: refuse only what is
  *certainly* wrong, and leave anything a `?` could satisfy to the runtime,
  which can see the sizes. `provably_ne()` is the workhorse. The two are duals,
  not complements -- `possibly(x)` is `!provably(!x)` -- so "possibly equal" is
  the negation of *provably unequal*, never of "possibly unequal"; for `?`
  against `3` both of those hold at once.
* `identical()` -- type identity, for buffer aliasing and `output_types`, where
  `?` must *not* match a known size.
* `unify_shapes()` / `unify_all_shapes()` / `unify_vt()` -- the most specific
  shape or type consistent with every input, or an error when there is none.
  These build result shapes: a definite clash and a rank mismatch both abort,
  and otherwise a `?` unified with a known size gives the known size, so
  `add(tensor<?xf32>, tensor<3xf32>)` has type `tensor<3xf32>`. Unify a "these
  must all agree" set with `unify_all_shapes()` rather than comparing each
  against the first -- "possibly equal" is not transitive, so folding it would
  accept `(3, ?, 4)`, whereas unification is associative and so folds
  correctly.

Two consequences for inference code. `==` and `!=` on a `Shape` raise rather
than answer, so a check has to name which of the three it means. And an `if`
must never branch on a possibly-`NA` comparison (`if (NA == 0L)` is an error,
not a `FALSE`) -- write the guard as `provably_*()` so an unknown operand falls
through to arithmetic, which propagates `NA` and gives the honest `?`.

Where StableHLO lets a size come from *data* rather than from a shape it does
so with a separate op that takes the sizes as an operand
(`hlo_dynamic_reshape()`, `hlo_dynamic_iota()`, `hlo_dynamic_pad()`,
`hlo_real_dynamic_slice()`, ...). So `NA` reaches an inference function only as
a result-type hint, never as a rendered attribute: `assert_shapevec()` stays
strict at every one of its call sites, and only those ops use
`assert_shapevec_dyn()`. `assert_size_operand()` is the shared boundary check
for those operands -- rank-1, integer dtype, one element per axis, and (for
the ops StableHLO types statically shaped) a static extent of its own.

## Testing

You can compare PJRTBuffers using `expect_equal()`, so you don't need to use `as_array()`.

### The dynamism tests that need more than the package

Each op's dynamism tests live in its own `test-op-<name>.R`, under a
`# ---- dynamic axis sizes` banner. Most are pure inference and always run, but
two kinds need something extra and **skip silently** without it -- so a green
run does not mean they passed. Check the skip count.

* `expect_refines_and_runs()` and `expect_dynamic_op_runs()` need
  `pjrt::pjrt_refine_shapes()` and the `stablehlo-opt` binary behind it. Point
  `PJRT_STABLEHLO_OPT_PATH` at a local binary to skip the download (the build
  is at `r-xla/pjrt-builds`, release tag `stablehlo`, and unpacks to ~3 GB), or
  set `PJRT_INSTALL=1` and let pjrt fetch it into its cache. These are the
  tests that check our inferred type against what stablehlo's own refinement
  pass derives.
* `iree_compiles()` and `iree_run()` need `iree-compile` and `iree-run-module`
  on `PATH`. They cover the one thing refinement structurally cannot: an
  extent that comes from the *data*, which XLA refuses outright ("can't be
  translated to XLA HLO").

`iree_compiles()` deliberately does not pass `--iree-llvmcpu-link-embedded=false`.
That flag emits a system ELF, which only a runtime built with the system
library loader can execute; a stock `iree-run-module` has just the embedded
one, and every `iree_run()` then fails with "HAL device `__device_0` not found
or unavailable" while `iree_compiles()` keeps passing.

## Adding New Operations

When implementing a new operation, closely follow the specification described in SPEC.md.
Also, annotate each check in the inference function with the corresponding requirement from the specification (C1, C2, C3, etc.).

Each such check also has to decide what it does with a dynamic axis size -- see
"Dynamic axis sizes" above. A constraint the spec states as an equality becomes
`provably_ne()` plus a `unify_shapes()` that records the refinement; one the spec
states as an inequality (scatter's window sizes, say) becomes `provably_gt()` and
refines nothing, because an inequality cannot pin a size.

## Error Messages

Error messages use `cli_abort()` and should clearly state what was expected and what was received.

### Structure

Use a two-part message: a header stating the constraint, and an `x =` bullet showing the actual values.

```r
cli_abort(c(
  "{.arg window_dimensions} must have length equal to input rank.",
  x = "Expected length {rank}, got {length(window_dims)}."
))
```

Always use `x = "..."` (not `i = "..."`) for the bullet showing what went wrong.

### Referencing arguments

Use `{.arg name}` for argument names, never bare text or `{.var ...}`.

```r
# Good
"{.arg operand} must have rank >= 2"

# Bad
"operand must have rank >= 2"
"{.var operand} must have rank >= 2"
```

### Formatting values

- **Objects with `cli_format` methods** (types like `DataType`, `Shape`, `ValueType`):
  pass the object directly via `{.val {obj}}`, do not wrap in `repr()` or `as.character()`.

  ```r
  # Good
  "Got {.val {tensor_type$dtype}}."
  # Bad
  "Got {.cls {repr(tensor_type$dtype)}}."
  ```

- **Shape vectors** (dimension sizes): use `shapevec_repr()` which formats as `(2x3x4)`.

  ```r
  "Got shapes {shapevec_repr(shape(a))} and {shapevec_repr(shape(b))}."
  ```

- **Integer vectors** (sizes, counts, non-index vectors): use `vec_repr()` which formats as `c(1, 2, 3)` for length > 1 or a plain number for length 1. Do **not** pass bare integer vectors to `{.val {x}}` (cli would format them as "1, 2 and 3") or manually use `paste()`.

  ```r
  # Good
  "Got slice_sizes = {vec_repr(slice_sizes_vec)}."
  # Bad
  "Got slice_sizes = {.val {slice_sizes_vec}}."
  "Got slice_sizes = [{paste(slice_sizes_vec, collapse = ', ')}]."
  ```

- **Character option vectors** (e.g. valid choices): `{.val {options}}` is fine since cli's "a, b and c" formatting suits option lists.

  ```r

  "{.arg rng_algorithm} must be one of {.val {valid_options}}."
  ```

### 0-based indices

For errors referencing 0-based index values, wrap them with `index_vec()` and in custom conditions.
Also implement `to_one_based()` for the condition class.
If a fitting condition class is available, use it, otherwise create a new one.

### Propagating call context

When writing wrapper/assert functions, accept and forward `call` so the error points at the user's call site, not the internal helper:

```r
assert_vts_are_tensors <- function(..., call = rlang::caller_env()) {
  # ... pass call = call to cli_abort or inner asserts
}
```
