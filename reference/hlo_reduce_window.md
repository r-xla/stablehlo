# ReduceWindow Operator

See <https://openxla.org/stablehlo/spec#reduce_window> for details.

## Usage

``` r
infer_types_reduce_window(
  ...,
  body,
  window_dimensions,
  window_strides,
  base_dilations,
  window_dilations,
  padding
)

hlo_reduce_window(
  inputs,
  init_values,
  window_dimensions,
  window_strides,
  base_dilations,
  window_dilations,
  padding,
  body
)
```

## Arguments

- ...:

  (Inputs, Init values)

- body:

  (`Func`)  
  The reduction function to apply to each window.

- window_dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The size of the window in each dimension.

- window_strides:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The stride of the window in each dimension.

- base_dilations:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dilation factor for the input tensor.

- window_dilations:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dilation factor for the window.

- padding:

  (`matrix`)  
  A matrix with shape `[rank, 2]` specifying the padding before and
  after each dimension.

- inputs:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`FuncValue`](FuncValue.md))  
  The input tensor(s) to apply the reduction to.

- init_values:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`FuncValue`](FuncValue.md))  
  The initial value(s) for the reduction. Must be 0-D tensors.
