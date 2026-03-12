# CustomOpBackendConfig

A backend configuration as a list of typed attributes for custom
operations. Each element must be a `BoolAttr`, `StringAttr`, or
`ScalarAttr` for now. All attribute names must be unique.

## Usage

``` r
CustomOpBackendConfig(items = list())
```

## Arguments

- items:

  (`list`)  
  A list of `BoolAttr`, `StringAttr`, or `ScalarAttr` objects.

## Value

`CustomOpBackendConfig`
