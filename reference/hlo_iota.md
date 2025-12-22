# Iota Operator

See <https://openxla.org/stablehlo/spec#iota> for details.

## Usage

``` r
hlo_iota(iota_dimension, dtype, shape, func = NULL)

infer_types_iota(iota_dimension, dtype, shape)
```

## Arguments

- iota_dimension, dtype, shape, func:

  ([`FuncValue`](FuncValue.md))  

- dtype:

  (`character(1)`)  
  The element type of the output tensor. One of: pred, i8, i16, i32,
  i64, ui8, ui16, ui32, ui64, f32, f64 (excluding boolean).

- shape:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The shape of the output tensor.

- func:

  ([`Func`](Func.md))  
  The function to add the operation to. Per default, uses the last
  function created with [`hlo_func`](hlo_func.md) or
  [`local_func`](hlo_func.md).

- iota_dimension:

  (`integer(1)`)  
  The dimension along which to generate increasing values. Must be in
  range `[0, rank(output))`.

## Value

[`FuncValue`](FuncValue.md)  
