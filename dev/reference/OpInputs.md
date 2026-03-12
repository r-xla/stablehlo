# OpInputs

This represents all the inputs to an operation, including values,
functions, and attributes.

## Usage

``` r
OpInputs(
  values,
  funcs = OpInputFuncs(),
  attrs = OpInputAttrs(),
  custom_attrs = list()
)
```

## Arguments

- values:

  ([`OpInputValues`](https://r-xla.github.io/stablehlo/dev/reference/OpInputValues.md))  
  The values used as inputs.

- funcs:

  ([`OpInputFuncs`](https://r-xla.github.io/stablehlo/dev/reference/OpInputFuncs.md))  
  The functions used as inputs.

- attrs:

  ([`OpInputAttrs`](https://r-xla.github.io/stablehlo/dev/reference/OpInputAttrs.md))  
  The attributes used as inputs.

- custom_attrs:

  (`list`)  
  Custom attributes. Use this attributes that require custom formatting.

## Value

(`OpInputs`)
