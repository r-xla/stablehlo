# Changelog

## stablehlo (development version)

### Breaking changes

- A `Shape` is now represented as an integer.
- `shape.Shape` was removed.

### Features

- Inference functions now refuse a dimension attribute that contains a
  missing value, instead of letting it surface as R’s
  `missing value where TRUE/FALSE needed`.
  [`GatherDimensionNumbers()`](https://r-xla.github.io/stablehlo/dev/reference/GatherDimensionNumbers.md)
  and
  [`ScatterDimensionNumbers()`](https://r-xla.github.io/stablehlo/dev/reference/ScatterDimensionNumbers.md)
  check their dimension vectors the same way.

- [`infer_types_convolution()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_convolution.md)
  refuses a `padding` that takes away more than a spatial dimension
  holds. Such a shape made XLA’s own inference abort the process;
  negative padding that only empties a dimension stays legal. It also
  refuses a zero-sized kernel spatial dimension, which would otherwise
  infer a non-empty result from an empty window.

- Dimension attributes are checked before they are coerced, so a whole
  number outside the integer range (`3e9`, `Inf`) is reported as the
  value the caller passed instead of the `NA`
  [`as.integer()`](https://rdrr.io/r/base/integer.html) made of it.
  Affects
  [`hlo_convolution()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_convolution.md),
  [`hlo_broadcast_in_dim()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_broadcast_in_dim.md),
  [`hlo_dynamic_slice()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_dynamic_slice.md)
  and
  [`hlo_empty()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_constant.md).

- [`CustomOpBackendConfig()`](https://r-xla.github.io/stablehlo/dev/reference/CustomOpBackendConfig.md)
  now accepts `ConstantAttr` items, so a custom call can carry
  array-valued attributes (what an XLA FFI handler decodes as
  `Span<const T>`) and not just scalars, booleans and strings.

- [`hlo_custom_call()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_custom_call.md)
  gained an `output_operand_aliases` argument, built with the new
  [`OutputOperandAlias()`](https://r-xla.github.io/stablehlo/dev/reference/OutputOperandAlias.md).
  XLA then hands the handler the same buffer for the aliased operand and
  result, which is what lets an in-place kernel avoid a copy.

### Bug fixes

- [`hlo_pad()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_pad.md)
  refused negative edge padding whose magnitude exceeded `rank(operand)`
  rather than the size of the dimension it applied to, so
  `pad(tensor<10xf32>, low = -5)` was rejected although its result is a
  `tensor<5xf32>`. Padding that genuinely empties a dimension is still
  refused, now naming the arguments involved.
- [`infer_types_concatenate()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_concatenate.md)
  rejects inputs of different rank. Its (C2) check compared the shapes
  without the concatenation dimension, which a shorter shape passed, so
  `concatenate(tensor<2x3x4>, tensor<2x3>, dimension = 2)` inferred a
  result with an unknown size along that dimension.
- [`infer_types_if()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_if.md)
  rejects a branch that declares inputs.
- [`infer_types_while()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_while.md)
  checks its `body`’s inputs and not only its outputs.
- The short assembly form (`%0 = stablehlo.<op> %a : <type>`) is emitted
  only for ops that actually allow it.
- [`hlo_triangular_solve()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_triangular_solve.md)
  now rejects operands that are not of floating-point type, as required
  by the StableHLO spec.
- [`infer_types_slice()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_slice.md)
  rejects a stride of `0`. The spec’s (C4) is `0 < strides`, but the
  check read `0 <= strides`.
- [`infer_types_dynamic_slice()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_dynamic_slice.md),
  [`infer_types_dynamic_update_slice()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_dynamic_update_slice.md)
  and
  [`infer_types_gather()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_gather.md)
  reject `start_indices` that are not of integer type, as the spec
  requires. A float one used to reach MLIR and come back as a raw parse
  error.
- Corrected some checks in the inference functions.
- Added some missing checks in the inference functions.
- [`infer_types_reduce()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce.md),
  [`infer_types_reduce_window()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce_window.md),
  [`infer_types_scatter()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_scatter.md)
  and
  [`infer_types_sort()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_sort.md)
  check their region’s arguments. Only the region’s outputs were read,
  so a body with the wrong arity, a non-scalar argument, or the wrong
  element type passed inference – a one-argument `sort` comparator
  returning an `f32` was accepted and rendered.
- [`infer_types_reduce()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce.md)
  accepts a body that accumulates into a wider element type. (C6) is
  `is_promotable(element_type(inputs[i]), Ei)`, not an equality, so
  summing an `i8` into an `i32` is legal; it used to be refused.

## stablehlo 0.4.0

### Features

- Added
  [`hlo_convolution()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_convolution.md)
- [`hlo_dot_general()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_dot_general.md)
  gained a `precision_config` argument, which is either `NULL` or one or
  two of `"DEFAULT"`, `"HIGH"` and `"HIGHEST"`.

### Breaking changes

- Adopted tengen’s enum-style `DataType`. The `BooleanType()`,
  `FloatType()`, `IntegerType()` and `UIntegerType()` constructors were
  removed, use
  [`dtype()`](https://r-xla.github.io/tengen/reference/dtype.html) /
  [`as_dtype()`](https://r-xla.github.io/tengen/reference/as_dtype.html)
  instead.
- The package now requires R \>= 4.4.0.

### Performance

- The package was optimized w.r.t. runtime performance. This was
  achieved by reducing the number of classes that are used internally
  when creating `Func`s. The `hlo_<*>` user API remains unaffected.
- The `hlo_*` builders of common ops gained an `output_types` argument.
  When the output types are known ahead of time (e.g. from a lowering
  that ran type inference at trace time), passing them skips redundant
  inference and its input validation.

### Bug fixes

- emit width-correct hex for f64 NaN/Inf constants
- [`hlo_reduce_window()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce_window.md)
  now validates the shape of `padding` against its declared type instead
  of its R representation.

## stablehlo 0.3.0

### Features

- Added support for CHLO ops, a higher-level companion dialect to
  stableHLO that is lowered to stableHLO during compilation. New ops:
  - Inverse trig:
    [`hlo_acos()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_acos.md),
    [`hlo_asin()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_asin.md),
    [`hlo_atan()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_atan.md).
  - Hyperbolic:
    [`hlo_cosh()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_cosh.md),
    [`hlo_sinh()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_sinh.md),
    [`hlo_acosh()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_acosh.md),
    [`hlo_asinh()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_asinh.md),
    [`hlo_atanh()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_atanh.md).
  - Gamma family:
    [`hlo_lgamma()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_lgamma.md),
    [`hlo_digamma()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_digamma.md),
    [`hlo_polygamma()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_polygamma.md).
  - Error / Bessel / misc:
    [`hlo_erf()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_erf.md),
    [`hlo_erfc()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_erfc.md),
    [`hlo_erf_inv()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_erf_inv.md),
    [`hlo_bessel_i1e()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_bessel_i1e.md),
    [`hlo_square()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_square.md).
  - Float predicates:
    [`hlo_is_inf()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_is_inf.md),
    [`hlo_is_pos_inf()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_is_pos_inf.md),
    [`hlo_is_neg_inf()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_is_neg_inf.md).
  - Selection:
    [`hlo_top_k()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_top_k.md)
    returning the top-k values and their indices along the last
    dimension.
- `OpName()` and
  [`new_Op()`](https://r-xla.github.io/stablehlo/dev/reference/new_Op.md)
  gain a `dialect` argument (default `"stablehlo"`) to support ops from
  other MLIR dialects.

### Bug fixes

- [`hlo_reduce_window()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce_window.md)
  now works on rank-1 inputs.

## stablehlo 0.2.0

### Features

- Added support for Modules

### Bug fixes

- Constants +-Inf/NaN of dtype f64 are now correctly created.
- Fixed assembly format for `select` op with all boolean (i1) types.

### Miscellaneous

- Use a simpler StableHLO string format for improved readability

## stablehlo 0.1.0

- Initial release
