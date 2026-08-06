# Convolution Operator

Computes dot products between windows of `lhs` and slices of `rhs`. See
<https://openxla.org/stablehlo/spec#convolution> for details.

All dimension indices are 0-based.

## Usage

``` r
infer_types_convolution(
  lhs,
  rhs,
  dimension_numbers,
  precision_config,
  window_strides,
  padding,
  lhs_dilation,
  rhs_dilation,
  window_reversal,
  feature_group_count,
  batch_group_count
)

hlo_convolution(
  lhs,
  rhs,
  dimension_numbers,
  window_strides,
  padding,
  lhs_dilation = NULL,
  rhs_dilation = NULL,
  window_reversal = NULL,
  feature_group_count = 1L,
  batch_group_count = 1L,
  precision_config = c("DEFAULT", "DEFAULT"),
  output_types = NULL
)
```

## Arguments

- lhs:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md))  
  The input tensor (typically `[batch, spatial..., feature]`).

- rhs:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md))  
  The kernel tensor.

- dimension_numbers:

  ([`ConvDimensionNumbers`](https://r-xla.github.io/stablehlo/reference/ConvDimensionNumbers.md))  
  The convolution dimension numbers describing the layout of `lhs`,
  `rhs`, and the result.

- precision_config:

  (`character(2)`)  
  Two precision specifiers (one for each operand), each one of
  `"DEFAULT"`, `"HIGH"` or `"HIGHEST"`. Defaults to
  `c("DEFAULT", "DEFAULT")`.

- window_strides:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Stride of the kernel window in each spatial dimension. Length `N - 2`
  where `N = rank(lhs)`.

- padding:

  (`matrix`)  
  `[N - 2, 2]` integer matrix of `(low, high)` padding for each spatial
  dimension.

- lhs_dilation:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Dilation factor applied to `lhs` (a.k.a. transpose-conv stride).
  Length `N - 2`. Defaults to all 1's.

- rhs_dilation:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Dilation factor applied to `rhs` (a.k.a. atrous-conv rate). Length
  `N - 2`. Defaults to all 1's.

- window_reversal:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether to reverse the kernel along each spatial dimension. Length
  `N - 2`. Defaults to all `FALSE`.

- feature_group_count:

  (`integer(1)`)  
  Number of feature groups (for grouped / depthwise convolution).
  Defaults to 1.

- batch_group_count:

  (`integer(1)`)  
  Number of batch groups. Defaults to 1.

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md)
