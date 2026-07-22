# Report arrays instead of tensors

Rewrites a condition's message to use array terminology: "tensor"
becomes "array" and "Tensor" becomes "Array". This is for downstream
packages such as `anvl`, whose users think in terms of arrays rather
than the StableHLO notion of tensors. stablehlo's own errors are
unaffected unless this function is applied to them.

The substitution is applied on word boundaries, so identifiers such as
`TensorType`,
[`hlo_tensor()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_constant.md)
or `assert_vts_are_tensors()` are left untouched. A rendered type such
as `tensor<2x3xf32>` *is* rewritten to `array<2x3xf32>`, which is the
intended behaviour for array-facing packages.

## Usage

``` r
to_array_terminology(x)
```

## Arguments

- x:

  (`condition`)  
  Condition whose message should be rewritten. Other objects are
  returned unchanged.

## Value

The condition, with array terminology applied to its message.
