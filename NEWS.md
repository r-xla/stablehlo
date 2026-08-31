# stablehlo (development version)

## Features

* Shape inference understands dimensions that are only known at run time. A
  constraint over an `NA` axis size is refused only when it is *certainly*
  violated and left to the runtime otherwise, and where one operand knows a
  size the other does not, the known size wins: `add(tensor<?xf32>,
  tensor<3xf32>)` used to be an error and now infers `tensor<3xf32>`. Applied
  to the elementwise and comparison ops, `select`, `reduce` and `concatenate`.
  Type *identity* is deliberately unchanged -- `tensor<?xf32>` still does not
  equal `tensor<3xf32>` -- so buffer aliasing stays sound.
* Added `hlo_get_dimension_size()` and `hlo_dynamic_broadcast_in_dim()`, the
  two ops a program needs to broadcast to a shape it only learns at run time.
  `Shape()` and `TensorType()` already accepted `NA` for a dynamic axis size;
  these make one usable. XLA does not compile a dynamic shape, so they are for
  backends that do (anvl's experimental IREE backend).

## Breaking changes

* A `Shape` *is* its integer vector now, with a class attached, rather than a
  list wrapping one. `length(shape)` is the rank, `shape[i]` is an axis size,
  and `shape$dims` is gone -- read the sizes with `unclass()`.

* `shape()` has no `Shape` method any more, for the same reason: a `Shape`
  already *is* its integer vector, so there is nothing to extract. `shape()`
  keeps working on a `ValueType`, `TensorType` or `Constant`.

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
