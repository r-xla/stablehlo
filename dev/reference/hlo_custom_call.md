# Custom Call Operation

Create a custom call operation that invokes an external function via the
FFI (Foreign Function Interface) API.

Note that the attributes `called_computations` and
`output_operand_aliases` are not implemented yet.

## Usage

``` r
hlo_custom_call(
  ...,
  call_target_name,
  api_version = 4L,
  has_side_effect,
  backend_config = NULL,
  output_types = NULL,
  operand_layouts = NULL,
  result_layouts = NULL
)
```

## Arguments

- ...:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  
  Input values to pass to the custom call.

- call_target_name:

  (`character(1)`)  
  The name of the registered custom function to call.

- api_version:

  (`integer(1)`)  
  The API version. Default is 4.

- has_side_effect:

  (`logical(1)`)  
  Whether the custom call has side effects.

- backend_config:

  ([`CustomOpBackendConfig`](https://r-xla.github.io/stablehlo/dev/reference/CustomOpBackendConfig.md)
  \| `NULL`)  
  Optional backend configuration.

- output_types:

  (`list` of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  The output types of the custom call. Default is NULL (no outputs).

- operand_layouts:

  (`list` of [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Layouts for each operand in minor-to-major order. Each element is an
  integer vector specifying the dimension order. For example,
  `c(0L, 1L)` means column-major (dimension 0 varies fastest), while
  `c(1L, 0L)` means row-major. Default `NULL` means no layout
  constraint.

- result_layouts:

  (`list` of [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Layouts for each result in minor-to-major order. Same format as
  `operand_layouts`.

## Value

([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)
\| [`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
The output value(s), or NULL for side-effect only calls.
