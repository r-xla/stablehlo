# Infer types for custom call

Infer the output types for a custom call operation. The attributes
"called_computations" and "output_operand_aliases" are not implemented
yet.

## Usage

``` r
infer_types_custom_call(
  ...,
  call_target_name,
  api_version = 4L,
  has_side_effect,
  backend_config = NULL,
  output_types = NULL
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
  The API version. Default is 4 (FFI API).

- has_side_effect:

  (`logical(1)`)  
  Whether the custom call has side effects.

- backend_config:

  (`list` \| `NULL`)  
  Optional backend configuration as a named list.

- output_types:

  (`list` of [`ValueType`](ValueType.md) \| `NULL`)  
  The output types of the custom call. Default is NULL (no outputs).

## Value

(`ValueTypes`)  
The output types (empty for side-effect only calls).
