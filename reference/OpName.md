# OpName

This represents the name of an operation, containing a mnemonic and a
dialect.

## Usage

``` r
OpName(mnemonic, dialect = "stablehlo")
```

## Arguments

- mnemonic:

  (`character(1)`)  
  The mnemonic of the operation.

- dialect:

  (`character(1)`)  
  The MLIR dialect this op belongs to. Defaults to `"stablehlo"`. Use
  `"chlo"` for CHLO ops (which lower to StableHLO during compilation).

## Value

(`OpName`)
