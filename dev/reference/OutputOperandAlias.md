# OutputOperandAlias

Declares that a
[`hlo_custom_call()`](https://r-xla.github.io/stablehlo/dev/reference/hlo_custom_call.md)
result shares its buffer with one of the call's operands. XLA then hands
the handler the same pointer for both, which is how an in-place kernel
avoids a copy – and why a handler that overwrites its input must be
written to tolerate it.

Indices are 0-based, as everywhere in StableHLO.

The StableHLO attribute also carries an `operand_tuple_indices` path,
for an operand that is itself a tuple. This package has no tuple types,
so that path can only ever be empty and is not exposed; it is rendered
as `[]`, which the attribute's syntax requires.

## Usage

``` r
OutputOperandAlias(operand_index, output_tuple_indices = integer())
```

## Arguments

- operand_index:

  (`integer(1)`)  
  Which operand of the custom call the result aliases.

- output_tuple_indices:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Which result is aliased, for a call that has several. This one is
  *not* about tuple types: StableHLO's verifier builds a tuple out of
  the result types whenever a custom call has more than one result, so a
  multi-result call must name the aliased result here or the alias is
  compared against the whole synthesised tuple and rejected. Empty (the
  default) is right for a single result.

## Value

`OutputOperandAlias`

## Examples

``` r
# the single result is written into the buffer of the first operand
OutputOperandAlias(operand_index = 0L)
#> #stablehlo.output_operand_alias<output_tuple_indices = [], operand_index = 0, operand_tuple_indices = []>
```
