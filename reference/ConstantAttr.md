# ConstantAttr

An attribute holding a constant value.

## Usage

``` r
ConstantAttr(name, value, simplify_dense = TRUE)
```

## Arguments

- name:

  (`character(1)`)  
  The name of the attribute.

- value:

  (`Constant`)  
  The value of the attribute.

- simplify_dense:

  (`logical(1)`)  
  Whether to simplify dense representation. Set to `FALSE` for
  multi-dimensional arrays.

## Value

(`ConstantAttr`)
