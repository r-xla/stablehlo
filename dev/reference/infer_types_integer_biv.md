# Infer types for integer binary operations

Infer the types for binary operations taking integers only. Unlike
[`infer_types_integerish_biv()`](https://r-xla.github.io/stablehlo/dev/reference/infer_types_integerish_biv.md),
booleans are not accepted: the bit-shift operations take a
`tensor of integer type`, which in the StableHLO spec does not include
`i1`.

## Usage

``` r
infer_types_integer_biv(lhs, rhs)
```

## Arguments

- lhs:

  (`ValueType`)  
  The left-hand side operand.

- rhs:

  (`ValueType`)  
  The right-hand side operand.

## Value

(`ValueType`)  
The inferred type.
