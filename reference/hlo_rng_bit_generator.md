# RngBitGenerator Operator

See <https://openxla.org/stablehlo/spec#rng_bit_generator> for details.

## Usage

``` r
infer_types_rng_bit_generator(initial_state, rng_algorithm, dtype, shape_out)

hlo_rng_bit_generator(
  initial_state,
  rng_algorithm = c("DEFAULT", "THREE_FRY", "PHILOX"),
  dtype,
  shape_out
)
```

## Arguments

- initial_state, rng_algorithm, dtype, shape_out:

  ([`FuncValue`](FuncValue.md))  

## Value

[`FuncValue`](FuncValue.md)  
