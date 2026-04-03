@../claude-config/CLAUDE.md

## Package Overview

`stablehlo` is an R package that allows to create StableHLO programs, a portable computation representation used in machine learning. It allows creating, manipulating, and transforming StableHLO operations in R.
The Func object uses reference semantic, while other objects use value semantics.

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
