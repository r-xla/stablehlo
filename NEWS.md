# stablehlo 0.2.0

## Features

* Added support for Modules

## Bug fixes

* Constants +-Inf/NaN of dtype f64 are now correctly created.
* Fixed assembly format for `select` op with all boolean (i1) types.

## Miscellaneous

* Simplify the generated stablehlo lines for ops without
  attributes where all input and output value types are identical.
* Use a simpler StableHLO string format for improved readability

# stablehlo 0.1.0

* Initial release
