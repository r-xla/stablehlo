# CustomCallConfig

Configuration for a custom call operation.

## Usage

``` r
CustomCallConfig(
  call_target_name,
  api_version = 4L,
  has_side_effect = TRUE,
  backend_config = NULL
)
```

## Arguments

- call_target_name:

  (`character(1)`)  
  The name of the custom function to call.

- api_version:

  (`integer(1)`)  
  The API version. Default is 4 (FFI API).

- has_side_effect:

  (`logical(1)`)  
  Whether the custom call has side effects. Default is TRUE.

- backend_config:

  (`list` \| `NULL`)  
  Optional backend configuration as a named list.

## Value

`CustomCallConfig`
