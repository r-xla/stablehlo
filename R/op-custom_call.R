#' @include op.R hlo.R type_inference.R
NULL

OpCustomCall <- new_Op("OpCustomCall", "custom_call")

#' @title CustomCallConfig
#' @description
#' Configuration for a custom call operation.
#' @param call_target_name (`character(1)`)\cr
#'   The name of the custom function to call.
#' @param api_version (`integer(1)`)\cr
#'   The API version. Default is 4 (FFI API).
#' @param has_side_effect (`logical(1)`)\cr
#'   Whether the custom call has side effects. Default is TRUE.
#' @param backend_config (`list` | `NULL`)\cr
#'   Optional backend configuration as a named list.
#' @return `CustomCallConfig`
#' @export
CustomCallConfig <- new_class(
  "CustomCallConfig",
  properties = list(
    call_target_name = S7::class_character,
    api_version = S7::class_integer,
    has_side_effect = S7::class_logical,
    backend_config = S7::class_list | NULL
  ),
  constructor = function(
    call_target_name,
    api_version = 4L,
    has_side_effect = TRUE,
    backend_config = NULL
  ) {
    new_object(
      S7::S7_object(),
      call_target_name = call_target_name,
      api_version = as.integer(api_version),
      has_side_effect = has_side_effect,
      backend_config = backend_config
    )
  }
)

method(repr, CustomCallConfig) <- function(x) {
  lines <- c(
    sprintf("call_target_name = \"%s\"", x@call_target_name),
    sprintf("has_side_effect = %s", tolower(as.character(x@has_side_effect))),
    sprintf("api_version = %d : i32", x@api_version)
  )

  if (!is.null(x@backend_config) && length(x@backend_config) > 0) {
    config_items <- vapply(
      names(x@backend_config),
      function(name) {
        value <- x@backend_config[[name]]
        if (is.character(value)) {
          sprintf("%s = \"%s\"", name, value)
        } else if (is.logical(value)) {
          sprintf("%s = %s", name, tolower(as.character(value)))
        } else if (is.numeric(value)) {
          if (is.integer(value)) {
            sprintf("%s = %d : i32", name, value)
          } else {
            sprintf("%s = %s", name, format(value, scientific = FALSE))
          }
        } else {
          sprintf("%s = %s", name, as.character(value))
        }
      },
      character(1)
    )
    backend_str <- paste0(
      "backend_config = {\n    ",
      paste(config_items, collapse = ",\n    "),
      "\n  }"
    )
    lines <- c(lines, backend_str)
  }

  paste0("{\n  ", paste(lines, collapse = ",\n  "), "\n}")
}

#' @title Infer types for custom call
#' @description
#' Infer the output types for a custom call operation.
#' @param ... Input value types and custom call config.
#' @return (`ValueTypes`)\cr
#'   The output types (empty for side-effect only calls).
#' @export
infer_types_custom_call <- function(...) {
  args <- list(...)
  # The last argument is always CustomCallConfig
  config <- args[[length(args)]]
  output_types <- attr(config, "output_types")

  if (is.null(output_types)) {
    return(ValueTypes(list()))
  }

  output_types
}

custom_call_impl <- hlo_fn(OpCustomCall, infer_types_custom_call)

#' @title Custom Call Operation
#' @description
#' Create a custom call operation that invokes an external function via the
#' FFI (Foreign Function Interface) API.
#'
#' @param ... ([`FuncValue`])\cr
#'   Input values to pass to the custom call.
#' @param call_target_name (`character(1)`)\cr
#'   The name of the registered custom function to call.
#' @param api_version (`integer(1)`)\cr
#'   The API version. Default is 4 (FFI API).
#' @param has_side_effect (`logical(1)`)\cr
#'   Whether the custom call has side effects. Default is TRUE.
#' @param backend_config (`list` | `NULL`)\cr
#'   Optional backend configuration as a named list.
#' @param output_types (`list` of [`ValueType`] | `NULL`)\cr
#'   The output types of the custom call. Default is NULL (no outputs).
#' @return ([`FuncValue`] | `list()` | `NULL`)\cr
#'   The output value(s), or NULL for side-effect only calls.
#' @export
#' @examples
#' \dontrun{
#' func <- local_func()
#' x <- hlo_input("x", "f32", shape = c(4))
#' # Call print_tensor (registered in pjrt)
#' hlo_custom_call(x, call_target_name = "print_tensor")
#' hlo_return(x)
#' }
hlo_custom_call <- function(
  ...,
  call_target_name,
  api_version = 4L,
  has_side_effect = TRUE,
  backend_config = NULL,
  output_types = NULL
) {
  values <- list(...)

  config <- CustomCallConfig(
    call_target_name = call_target_name,
    api_version = as.integer(api_version),
    has_side_effect = has_side_effect,
    backend_config = backend_config
  )

  # Attach output types to config for type inference
  if (!is.null(output_types)) {
    if (!inherits(output_types, "ValueTypes")) {
      output_types <- ValueTypes(output_types)
    }
    attr(config, "output_types") <- output_types
  }

  custom_call_impl(
    values = values,
    custom_attrs = list(
      custom_call_config = config
    )
  )
}

method(repr, OpCustomCall) <- function(x, ...) {
  config <- x@inputs@custom_attrs$custom_call_config

  # Build the @target_name reference
  target_name <- config@call_target_name

  # Build input values repr
  input_values <- repr(x@inputs@values)

  # Build the config attributes
  config_repr <- repr(config)

  # Build signature
  signature_repr <- repr(x@signature)

  # Build output part
  outputs_repr <- if (length(x@outputs@items) == 0) {
    ""
  } else {
    paste0(repr(x@outputs), " = ")
  }

  paste0(
    outputs_repr,
    "stablehlo.custom_call @",
    target_name,
    "(",
    input_values,
    ") ",
    config_repr,
    " : ",
    signature_repr
  )
}
