@../claude-config/CLAUDE.md

## Package Overview

`stablehlo` is an R package that allows to create StableHLO programs, a portable computation representation used in machine learning. It allows creating, manipulating, and transforming StableHLO operations in R.
The Func object uses reference semantic, while other objects use value semantics.

## Design

The builder is optimized for low per-op overhead (lowering a graph should stay
in the same order of magnitude as `pjrt_compile()`):

* **Lazy rendering**: each op is stored on its func as a deferred
  `list(render, ctx)` pair via `func_emit()`, appended to a growable `ops`
  list held in the func's `buf` environment. Because `ops` is not aliased
  between emissions, `buf$ops[[n]] <- item` extends it in place with
  amortised-O(1) doubling, so appending stays cheap and never rebuilds earlier
  entries. The MLIR text line is produced by `render(ctx)` at `repr()` time, in
  `func_lines()`, front-to-back (the buffer is already in emission order). There
  is no op-record tree to walk; a single `repr()` per program means deferring
  the render costs nothing over rendering eagerly.
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
* Constructors on the hot path (`FuncValue`, `FuncInput`, `TensorType`, ...)
  do not validate their inputs; validation happens in `hlo_fn()` and the
  `infer_types_*` functions. Prefer `inherits()` over `checkmate` helpers in
  per-op code paths.

## Testing

You can compare PJRTBuffers using `expect_equal()`, so you don't need to use `as_array()`.

## Adding New Operations

When implementing a new operation, closely follow the specification described in SPEC.md.
Also, annotate each check in the inference function with the corresponding requirement from the specification (C1, C2, C3, etc.).

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
