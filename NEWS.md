# stablehlo (development version)

## Breaking changes

* A `Shape` is now represented as an integer.
* `shape.Shape` was removed.

## Bug fixes

* `hlo_triangular_solve()` now rejects operands that are not of floating-point
  type, as required by the StableHLO spec.
* `infer_types_slice()` rejects a stride of `0`. The spec's (C4) is
  `0 < strides`, but the check read `0 <= strides`.
* `infer_types_dynamic_slice()`, `infer_types_dynamic_update_slice()` and
  `infer_types_gather()` reject `start_indices` that are not of integer type,
  as the spec requires. A float one used to reach MLIR and come back as a raw
  parse error.
* `infer_types_scatter()` rejects `scatter_indices` that are not of integer
  type (I2), as `gather` already did.
* `infer_types_transpose()` rejects a `permutation` with duplicates. (C2) asks
  for a permutation of the operand's axes, but the check compared sets, so
  `c(0, 1, 1)` on a rank-2 operand passed and produced a rank-3 result type.
* `hlo_abs()` rejects unsigned operands. (I1) is signed-only, but the op used
  the shared `infer_types_numeric_uni()`, leaving its own `infer_types_abs()`
  unused.
* `infer_types_after_all()` requires its inputs to be tokens (I1), and accepts
  none -- which is how a token is produced.
* `infer_types_concatenate()` rejects a negative `dimension`. (C4) is
  `0 <= dimension < rank`, but only the upper bound was checked, and a
  negative one silently produced a wrong result type.
* `infer_types_reduce_window()` rejects a `window_dilations` of `0`, which
  (C11) forbids; it collapsed every window to width 1.
* `infer_types_dynamic_update_slice()` requires its `start_indices` to share
  one type (C5), as `dynamic_slice` already did.
* `infer_types_reverse()` accepts an empty `dimensions`, which (C2) and (C3)
  satisfy vacuously and StableHLO accepts.
* An op with no value operands reports its own error instead of failing with
  `subscript out of bounds`.

# stablehlo 0.4.0

## Features

* Added `hlo_convolution()`
* `hlo_dot_general()` gained a `precision_config` argument, which is
  either `NULL` or one or two of `"DEFAULT"`, `"HIGH"` and `"HIGHEST"`.

## Breaking changes

* Adopted tengen's enum-style `DataType`. The `BooleanType()`, `FloatType()`,
  `IntegerType()` and `UIntegerType()` constructors were removed, use
  `dtype()` / `as_dtype()` instead.
* The package now requires R >= 4.4.0.

## Performance

* The package was optimized w.r.t. runtime performance.
  This was achieved by reducing the number of classes
  that are used internally when creating `Func`s.
  The `hlo_<*>` user API remains unaffected.
* The `hlo_*` builders of common ops gained an `output_types` argument.
  When the output types are known ahead of time (e.g. from a lowering that
  ran type inference at trace time), passing them skips redundant inference
  and its input validation.

## Bug fixes

* emit width-correct hex for f64 NaN/Inf constants
* `hlo_reduce_window()` now validates the shape of `padding` against its
  declared type instead of its R representation.


# stablehlo 0.3.0

## Features

* Added support for CHLO ops, a higher-level companion dialect to stableHLO
  that is lowered to stableHLO during compilation. New ops:
  * Inverse trig: `hlo_acos()`, `hlo_asin()`, `hlo_atan()`.
  * Hyperbolic: `hlo_cosh()`, `hlo_sinh()`, `hlo_acosh()`, `hlo_asinh()`,
    `hlo_atanh()`.
  * Gamma family: `hlo_lgamma()`, `hlo_digamma()`, `hlo_polygamma()`.
  * Error / Bessel / misc: `hlo_erf()`, `hlo_erfc()`, `hlo_erf_inv()`,
    `hlo_bessel_i1e()`, `hlo_square()`.
  * Float predicates: `hlo_is_inf()`, `hlo_is_pos_inf()`, `hlo_is_neg_inf()`.
  * Selection: `hlo_top_k()` returning the top-k values and their indices
    along the last dimension.
* `OpName()` and `new_Op()` gain a `dialect` argument (default `"stablehlo"`)
  to support ops from other MLIR dialects.

## Bug fixes

* `hlo_reduce_window()` now works on rank-1 inputs.

# stablehlo 0.2.0

## Features

* Added support for Modules

## Bug fixes

* Constants +-Inf/NaN of dtype f64 are now correctly created.
* Fixed assembly format for `select` op with all boolean (i1) types.

## Miscellaneous

* Use a simpler StableHLO string format for improved readability

# stablehlo 0.1.0

* Initial release
