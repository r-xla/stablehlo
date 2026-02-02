# Convert 0-based indices to 1-based

Converts all index_vec fields in a condition object from 0-based to
1-based.

## Usage

``` r
to_one_based(x, ...)
```

## Arguments

- x:

  Condition object with index_vec fields.

- ...:

  Additional arguments (not used).

## Value

Condition object with index_vec fields incremented by 1.
