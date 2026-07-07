# Define a new Op

Creates the descriptor of a StableHLO operation, consumed by `hlo_fn()`.

## Usage

``` r
new_Op(classname, mnemonic, dialect = "stablehlo", render = NULL)
```

## Arguments

- classname:

  Name of the op (kept for readability at the call sites).

- mnemonic:

  The operation mnemonic.

- dialect:

  The MLIR dialect (`"stablehlo"` or `"chlo"`). Defaults to
  `"stablehlo"`.

- render:

  (`function(ctx)` \| `NULL`)  
  Custom render function producing the op's MLIR line. `NULL` uses the
  default assembly/generic format. The `ctx` argument is a list with
  fields `mnemonic`, `dialect`, `outputs_str`, `values_str`,
  `in_type_strs`, `out_type_strs`, `sig_str`, `attrs`, `attrs_str`,
  `funcs_str` and `custom_attrs`.

## Value

A descriptor `list` for use with `hlo_fn()`.
