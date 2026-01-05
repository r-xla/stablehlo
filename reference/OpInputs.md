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

  ([`OpInputValues`](OpInputValues.md))  
  The values used as inputs.

- funcs:

  ([`OpInputFuncs`](OpInputFuncs.md))  
  The functions used as inputs.

- attrs:

  ([`OpInputAttrs`](OpInputAttrs.md))  
  The attributes used as inputs.

- custom_attrs:

  (`list`)  
  Custom attributes. Use this attributes that require custom formatting.

## Value

(`OpInputs`)
