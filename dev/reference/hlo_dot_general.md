# DotGeneral Operator

See <https://openxla.org/stablehlo/spec#dot_general> for details.

## Usage

``` r
infer_types_dot_general(
  lhs,
  rhs,
  dot_dimension_numbers,
  precision_config = NULL
)

hlo_dot_general(
  lhs,
  rhs,
  contracting_dims,
  batching_dims = NULL,
  precision_config = NULL,
  output_types = NULL
)
```

## Arguments

- lhs, rhs, contracting_dims, batching_dims:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- dot_dimension_numbers:

  (`DotDimensionNumbers`)  
  The dot dimension number.

- precision_config:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The precision configuration, a character vector of length 2 giving the
  precision used for `lhs` and `rhs`. Each entry must be one of
  `"DEFAULT"`, `"HIGH"` or `"HIGHEST"`. If `NULL` (default), no
  precision configuration is emitted, which is equivalent to `"DEFAULT"`
  for both operands.

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
