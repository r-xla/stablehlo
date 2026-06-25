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
  precision_config = NULL
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

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
