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

hlo_triangular_solve(a, b, left_side, lower, unit_diagonal, transpose_a)
```

## Arguments

- a, b, left_side, lower, unit_diagonal, transpose_a:

  ([`FuncValue`](FuncValue.md))  

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

## Value

[`FuncValue`](FuncValue.md)  
