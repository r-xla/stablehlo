# ConvDimensionNumbers

Represents the dimension numbers used by
[`hlo_convolution`](https://r-xla.github.io/stablehlo/dev/reference/hlo_convolution.md).
All indices are 0-based.

For an input/output tensor of rank `N`, the batch dimension, the feature
dimension and the `N - 2` spatial dimensions partition `[0, N)`.
Likewise for the kernel, the input feature, output feature and spatial
dimensions partition `[0, N)`.

## Usage

``` r
ConvDimensionNumbers(
  input_batch_dimension,
  input_feature_dimension,
  input_spatial_dimensions,
  kernel_input_feature_dimension,
  kernel_output_feature_dimension,
  kernel_spatial_dimensions,
  output_batch_dimension,
  output_feature_dimension,
  output_spatial_dimensions
)
```

## Arguments

- input_batch_dimension:

  (`integer(1)`)  
  Batch dimension of the input.

- input_feature_dimension:

  (`integer(1)`)  
  Feature dimension of the input.

- input_spatial_dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Spatial dimensions of the input.

- kernel_input_feature_dimension:

  (`integer(1)`)  
  Input-feature dimension of the kernel.

- kernel_output_feature_dimension:

  (`integer(1)`)  
  Output-feature dimension of the kernel.

- kernel_spatial_dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Spatial dimensions of the kernel.

- output_batch_dimension:

  (`integer(1)`)  
  Batch dimension of the output.

- output_feature_dimension:

  (`integer(1)`)  
  Feature dimension of the output.

- output_spatial_dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Spatial dimensions of the output.

## Value

(`ConvDimensionNumbers`)
