# TriangularSolve Operator

See <https://openxla.org/stablehlo/spec#triangular_solve> for details.

## Usage

``` r
infer_types_triangular_solve(
  a,
  b,
  left_side,
  lower,
  unit_diagonal,
  transpose_a
)

hlo_triangular_solve(
  a,
  b,
  left_side,
  lower,
  unit_diagonal,
  transpose_a,
  output_types = NULL
)
```

## Arguments

- a, b:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- left_side:

  (`logical(1)`)  
  If `TRUE`, solve `op(a) * x = b`. If `FALSE`, solve `x * op(a) = b`.

- lower:

  (`logical(1)`)  
  If `TRUE`, use lower triangle of `a`. If `FALSE`, use upper triangle.

- unit_diagonal:

  (`logical(1)`)  
  If `TRUE`, assume diagonal elements of `a` are 1.

- transpose_a:

  (`character(1)`)  
  One of `"NO_TRANSPOSE"`, `"TRANSPOSE"`, or `"ADJOINT"`.

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
