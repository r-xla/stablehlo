# stablehlo (development version)

## Features

* Added `hlo_fft()` for forward and inverse Fourier transforms (`FFT`, `IFFT`,
  `RFFT`, `IRFFT`).
* Re-export `ComplexType()` from tengen for representing `complex<f32>` /
  `complex<f64>` (a.k.a. `c64` / `c128`) tensors.
* Added support for CHLO ops, a higher-level companion dialect to stableHLO
  that is lowered to stableHLO during compilation. New ops:
  * Inverse trig: `hlo_acos()`, `hlo_asin()`, `hlo_atan()`.
  * Hyperbolic: `hlo_cosh()`, `hlo_sinh()`, `hlo_acosh()`, `hlo_asinh()`,
    `hlo_atanh()`.
  * Gamma family: `hlo_lgamma()`, `hlo_digamma()`, `hlo_polygamma()`.
  * Error / Bessel / misc: `hlo_erf()`, `hlo_erfc()`, `hlo_erf_inv()`,
    `hlo_bessel_i1e()`, `hlo_square()`.
  * Float predicates: `hlo_is_inf()`, `hlo_is_pos_inf()`, `hlo_is_neg_inf()`.
* `OpName()` and `new_Op()` gain a `dialect` argument (default `"stablehlo"`)
  to support ops from other MLIR dialects.

## Bug fixes

`hlo_reduce_window()` now works on rank-1 inputs.
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
