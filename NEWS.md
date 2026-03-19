# stablehlo (development version)

## Bug fixes

* Constants +-Inf/NaN of dtype f64 are now correctly created.

## Miscellaneous

* Simplify the generated stablehlo lines for ops without
  attributes where all input and output value types are identical.

# stablehlo 0.1.0

* Initial release
