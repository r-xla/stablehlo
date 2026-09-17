# Define a new Op

Creates the descriptor of a StableHLO operation, consumed by `hlo_fn()`.

## Usage

``` r
new_Op(
  classname,
  mnemonic,
  dialect = "stablehlo",
  render = NULL,
  same_type_form = TRUE
)
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

- same_type_form:

  (`logical(1)`)  
  Whether the op's MLIR assembly has the short
  `%0 = stablehlo.<op> %a : <type>` form, which names one type for the
  operands and the result alike. In the ODS an op gets that form from
  the elementwise base classes' `custom<SameOperandsAndResultType>`
  assembly, and loses it by overriding `assemblyFormat` with
  `functional-type(operands, results)` – `is_finite` and
  `dynamic_update_slice` are the two such ops here whose operand and
  result types can still coincide, so the short form would be emitted
  for them and would not parse. Default `TRUE`; set it `FALSE` for an op
  whose ODS overrides the assembly.

## Value

A descriptor `list` for use with `hlo_fn()`.
