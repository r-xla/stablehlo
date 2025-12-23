# Create a ConstantAttr from R values

Helper function to create a ConstantAttr from R values.

## Usage

``` r
constant_attr(name, value, dtype = NULL, shape = NULL, simplify_dense = TRUE)
```

## Arguments

- name:

  (`character(1)`)  
  The name of the attribute.

- value:

  (any)  
  The R value to convert to a constant.

- dtype:

  (`character(1)` \| `NULL`)  
  The dtype of the constant. If NULL, inferred from value.

- shape:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`)  
  The shape of the constant. If NULL, inferred from value.

- simplify_dense:

  (`logical(1)`)  
  Whether to simplify dense representation. Set to `FALSE` for
  multi-dimensional arrays.

## Value

(`ConstantAttr`)
