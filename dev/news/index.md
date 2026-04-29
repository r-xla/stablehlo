# Changelog

## stablehlo (development version)

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
- [`OpName()`](https://r-xla.github.io/stablehlo/dev/reference/OpName.md)
  and
  [`new_Op()`](https://r-xla.github.io/stablehlo/dev/reference/new_Op.md)
  gain a `dialect` argument (default `"stablehlo"`) to support ops from
  other MLIR dialects.

### Bug fixes

[`hlo_reduce_window()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_reduce_window.md)
now works on rank-1 inputs. \# stablehlo 0.2.0

### Features

- Added support for Modules

### Bug fixes

- Constants +-Inf/NaN of dtype f64 are now correctly created.
- Fixed assembly format for `select` op with all boolean (i1) types.

### Miscellaneous

- Use a simpler StableHLO string format for improved readability

## stablehlo 0.1.0

- Initial release
