# ValueId

This represents the name of a
[`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md).

## Usage

``` r
ValueId(id = NULL)
```

## Arguments

- id:

  (`character(1)` or `NULL`)  
  Either a fixed name or `NULL` (default). A `NULL` id is assigned a
  numeric name lazily when the program is rendered
  ([`repr()`](https://r-xla.github.io/stablehlo/dev/reference/repr.md)),
  in the order ids first appear: `%0`, `%1`, ..., skipping any integer
  already claimed by a named id in the same program (e.g. an input
  called `"2"`).

## Value

(`ValueId`)
