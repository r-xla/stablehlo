# Infer types for custom call

Infer the output types for a custom call operation.

## Usage

``` r
infer_types_custom_call(
  ...,
  call_target_name,
  api_version,
  has_side_effect,
  backend_config = NULL,
  output_types = NULL,
  operand_layouts = NULL,
  result_layouts = NULL,
  output_operand_aliases = NULL
)
```

## Arguments

- ...:

  Input values.

- call_target_name:

  (`character(1)`)  
  The name of the custom function to call.

- api_version:

  (`integer(1)`)  
  The API version.

- has_side_effect:

  (`logical(1)`)  
  Whether the custom call has side effects.

- backend_config:

  (`list` \| `NULL`)  
  Optional backend configuration as a named list.

- output_types:

  (`list` of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  The output types of the custom call. Default is NULL (no outputs).

- operand_layouts, result_layouts:

  Layouts, checked but not used for type inference.

- output_operand_aliases:

  Aliases (not used for type inference; named only so that `hlo_fn()`
  does not pass it through `...` as an operand).

## Value

(`ValueTypes`)  
The output types (empty for side-effect only calls).
