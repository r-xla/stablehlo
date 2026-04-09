# Changelog

## stablehlo (development version)

### Features

- Added support for Modules

### Bug fixes

- Constants +-Inf/NaN of dtype f64 are now correctly created.

### Miscellaneous

- Simplify the generated stablehlo lines for ops without attributes
  where all input and output value types are identical.
- Use a simpler StableHLO string format for improved readability

## stablehlo 0.1.0

- Initial release
