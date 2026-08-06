# RngBitGenerator Operator

See <https://openxla.org/stablehlo/spec#rng_bit_generator> for details.

## Usage

``` r
infer_types_rng_bit_generator(initial_state, rng_algorithm, dtype, shape)

hlo_rng_bit_generator(
  initial_state,
  rng_algorithm = c("DEFAULT", "THREE_FRY", "PHILOX"),
  dtype,
  shape,
  output_types = NULL
)
```

## Arguments

- initial_state, rng_algorithm, dtype, shape:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md))  

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md)  
