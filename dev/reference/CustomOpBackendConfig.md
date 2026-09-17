# CustomOpBackendConfig

A backend configuration as a list of typed attributes for custom
operations. Each element must be a
[`BoolAttr`](https://r-xla.github.io/stablehlo/dev/reference/BoolAttr.md),
[`StringAttr`](https://r-xla.github.io/stablehlo/dev/reference/StringAttr.md),
[`ScalarAttr`](https://r-xla.github.io/stablehlo/dev/reference/ScalarAttr.md)
or
[`ConstantAttr`](https://r-xla.github.io/stablehlo/dev/reference/ConstantAttr.md).
All attribute names must be unique.

A
[`ConstantAttr`](https://r-xla.github.io/stablehlo/dev/reference/ConstantAttr.md)
carries a vector rather than a scalar and is what an XLA FFI handler
decodes as `Span<const T>`. The dtype has to match the handler's element
type exactly – `Span<const int64_t>` needs `"i64"`, not the `"i32"` an R
integer vector would otherwise infer.
[`constant_attr()`](https://r-xla.github.io/stablehlo/dev/reference/constant_attr.md)
is a shorthand that builds the
[`Constant`](https://r-xla.github.io/stablehlo/dev/reference/Constant.md)
for you.

## Usage

``` r
CustomOpBackendConfig(items = list())
```

## Arguments

- items:

  (`list`)  
  A list of
  [`BoolAttr`](https://r-xla.github.io/stablehlo/dev/reference/BoolAttr.md),
  [`StringAttr`](https://r-xla.github.io/stablehlo/dev/reference/StringAttr.md),
  [`ScalarAttr`](https://r-xla.github.io/stablehlo/dev/reference/ScalarAttr.md)
  or
  [`ConstantAttr`](https://r-xla.github.io/stablehlo/dev/reference/ConstantAttr.md)
  objects.

## Value

`CustomOpBackendConfig`

## Examples

``` r
CustomOpBackendConfig(list(
  StringAttr(name = "mode", value = "fast"),
  ConstantAttr(
    name = "axes",
    value = r_to_constant(c(0L, 2L), dtype = "i64", shape = 2L)
  )
))
#> [[1]]
#> $name
#> [1] "mode"
#> 
#> $value
#> [1] "fast"
#> 
#> attr(,"class")
#> [1] "StringAttr"  "OpInputAttr"
#> 
#> [[2]]
#> $name
#> [1] "axes"
#> 
#> $value
#> $data
#> [1] 0 2
#> 
#> $type
#> tensor<2xi64> 
#> 
#> attr(,"class")
#> [1] "Constant"
#> 
#> $simplify_dense
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "ConstantAttr" "OpInputAttr" 
#> 
#> attr(,"class")
#> [1] "CustomOpBackendConfig"
```
