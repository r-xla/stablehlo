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

  ([`DataType`](https://r-xla.github.io/tengen/reference/DataType.html))  
  The dtype of the scalar (e.g., `as_dtype("i32")`, `as_dtype("f32")`,
  `as_dtype("bool")`).

## Value

`ScalarAttr`
