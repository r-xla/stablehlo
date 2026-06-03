# Create a new Op subclass

Create a new Op subclass

## Usage

``` r
new_Op(classname, mnemonic, dialect = "stablehlo")
```

## Arguments

- classname:

  Name of the new Op class

- mnemonic:

  The operation mnemonic

- dialect:

  The MLIR dialect (`"stablehlo"` or `"chlo"`). Defaults to
  `"stablehlo"`.

## Value

Constructor function for the Op subclass
