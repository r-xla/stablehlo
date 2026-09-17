# stablehlo (development version)

## Breaking changes

* A `Shape` *is* its integer vector now, with a class attached, rather than a
  list wrapping one. `length(shape)` is the rank and `shape[i]` an axis size;
  `shape$dims` is gone -- read the sizes with `unclass()`.

* Comparison operators on a `Shape` (`==`, `!=`, `<`, `>`, `<=`, `>=`) now
  raise. Once an axis size can be `NA`, "equal" is three different questions
  and an operator cannot say which was meant.

* `shape()` has no `Shape` method any more; a `Shape` already is its integer
  vector. It keeps working on a `ValueType`, `TensorType` or `Constant`.

## Features

* Shape inference understands axis sizes known only at run time, written `NA`
  in a `Shape` and `?` in MLIR. A constraint is refused only when *certainly*
  violated, and a known size wins over a dynamic one, so
  `add(tensor<?xf32>, tensor<3xf32>)` infers `tensor<3xf32>`.

* Added the ops that take their sizes as operands rather than attributes:
  `hlo_dynamic_broadcast_in_dim()`, `hlo_dynamic_iota()`,
  `hlo_dynamic_reshape()`, `hlo_dynamic_pad()`, `hlo_dynamic_gather()` and
  `hlo_dynamic_conv()`. Each takes a `shape` argument giving the result's
  static shape with `NA` where a size is only known at run time; it is a
  claim, not a check.

* Added `hlo_get_dimension_size()`, which reads an axis size out as a value.

* Added `hlo_real_dynamic_slice()`, in the StableHLO dialect but not in
  SPEC.md. It is the only op whose result extent comes from the data, so it
  needs a backend that compiles dynamic shapes -- XLA refuses it and shape
  refinement cannot remove it.

* `hlo_dynamic_broadcast_in_dim()` gained the optional
  `known_expanding_dimensions` and `known_nonexpanding_dimensions` attributes.

## Bug fixes

* The short assembly form (`%0 = stablehlo.<op> %a : <type>`) is emitted only
  for ops that actually allow it.
* `hlo_triangular_solve()` now rejects operands that are not of floating-point
  type, as required by the StableHLO spec.
* `infer_types_slice()` rejects a stride of `0`. The spec's (C4) is
  `0 < strides`, but the check read `0 <= strides`.
* `infer_types_dynamic_slice()`, `infer_types_dynamic_update_slice()` and
  `infer_types_gather()` reject `start_indices` that are not of integer type,
  as the spec requires. A float one used to reach MLIR and come back as a raw
  parse error.
* Corrected some checks in the inference functions.
* Added some missing checks in the inference functions.
* `infer_types_reduce()`, `infer_types_reduce_window()`,
  `infer_types_scatter()` and `infer_types_sort()` check their region's
  arguments. Only the region's outputs were read, so a body with the wrong
  arity, a non-scalar argument, or the wrong element type passed inference --
  a one-argument `sort` comparator returning an `f32` was accepted and
  rendered.
* `infer_types_reduce()` accepts a body that accumulates into a wider element
  type. (C6) is `is_promotable(element_type(inputs[i]), Ei)`, not an equality,
  so summing an `i8` into an `i32` is legal; it used to be refused.

* `hlo_pad()`'s negative-padding check compared each axis's trimming against
  the operand's *rank* rather than that axis's size, so it refused legal
  programs and accepted illegal ones. It is now (C4) applied per axis.

* The dynamic ops' size operands (`output_shape`, `slice_sizes`,
  `edge_padding_low`, ...) must be integer tensors, and those StableHLO types
  statically shaped must have a statically known number of elements.

* `hlo_scatter()` read an axis of `scatter_indices` at `index_vector_dim`
  before bounding it, so an out-of-range value failed with an internal R error.

* `hlo_if()` now rejects branches that declare inputs, as `hlo_case()` already
  did, and `hlo_while()` checks its `body`'s inputs and not only its outputs.

* `Shape()` refuses an `NA` that `as.integer()` invented -- a value out of
  integer range, an `Inf`/`NaN`, or a non-numeric -- since `NA` means dynamic.

* A shape holding a known `0` beside a `?` now has a known element count of 0,
  so `hlo_reshape()` and `hlo_dynamic_reshape()` still refuse an operand that
  provably holds no elements.

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
