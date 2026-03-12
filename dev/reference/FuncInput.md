# FuncInput

This represents an input of a
[`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md).

## Usage

``` r
FuncInput(id, type, alias = NULL)
```

## Arguments

- id:

  ([`ValueId`](https://r-xla.github.io/stablehlo/dev/reference/ValueId.md))  
  The id of the input.

- type:

  ([`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md))  
  The type of the input.

- alias:

  (`integer(1)` \| `NULL`)  
  With which output buffer to alias this input.

## Value

(`FuncInput`)
