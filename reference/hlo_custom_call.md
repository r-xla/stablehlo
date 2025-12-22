# Custom Call Operation

Create a custom call operation that invokes an external function via the
FFI (Foreign Function Interface) API.

## Usage

``` r
hlo_custom_call(
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

  ([`FuncValue`](FuncValue.md))  
  Input values to pass to the custom call.

- call_target_name:

  (`character(1)`)  
  The name of the registered custom function to call.

- api_version:

  (`integer(1)`)  
  The API version. Default is 4 (FFI API).

- has_side_effect:

  (`logical(1)`)  
  Whether the custom call has side effects. Default is TRUE.

- backend_config:

  ([`CustomOpBackendConfig`](CustomOpBackendConfig.md) \| `NULL`)  
  Optional backend configuration.

- output_types:

  (`list` of [`ValueType`](ValueType.md) \| `NULL`)  
  The output types of the custom call. Default is NULL (no outputs).

## Value

([`FuncValue`](FuncValue.md) \|
[`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
The output value(s), or NULL for side-effect only calls.
