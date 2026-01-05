# ScalarAttr

An attribute holding a scalar value with an associated dtype.

## Usage

``` r
ScalarAttr(name, value, dtype)
```

## Arguments

- name:

  (`character(1)`)  
  The name of the attribute.

- value:

  (`numeric(1)` or `logical(1)`)  
  The scalar value.

- dtype:

  ([`TensorDataType`](TensorDataType.md))  
  The dtype of the scalar (e.g., `IntegerType(32)`, `FloatType(32)`,
  [`BooleanType()`](BooleanType.md)).

## Value

`ScalarAttr`
