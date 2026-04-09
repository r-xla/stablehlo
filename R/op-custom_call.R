#' @include op.R hlo.R type_inference.R
NULL

OpCustomCall <- new_Op("OpCustomCall", "custom_call")

#' @title CustomOpBackendConfig
#' @description
#' A backend configuration as a list of typed attributes for custom operations.
#' Each element must be a `BoolAttr`, `StringAttr`, or `ScalarAttr` for now.
#' All attribute names must be unique.
#' @param items (`list`)\cr
#'   A list of `BoolAttr`, `StringAttr`, or `ScalarAttr` objects.
#' @return `CustomOpBackendConfig`
#' @export
CustomOpBackendConfig <- function(items = list()) {
  checkmate::assert_list(
    items,
    types = c("BoolAttr", "StringAttr", "ScalarAttr")
  )

  # Check for unique names
  if (length(items) > 0) {
    nms <- vapply(items, function(x) x$name, character(1))
    if (anyDuplicated(nms)) {
      cli_abort("All attribute names must be unique")
    }
  }

  structure(
    items,
    class = c("CustomOpBackendConfig")
  )
}

#' @export
repr.CustomOpBackendConfig <- function(
  x,
  simplify_dense = TRUE,
  ...
) {
  if (length(x) == 0) {
    return("backend_config = {}")
  }

  config_items <- vapply(
    x,
    repr,
    character(1),
    simplify_dense = simplify_dense
  )
  paste0(
    "backend_config = {\n    ",
    paste(config_items, collapse = ",\n    "),
    "\n  }"
  )
}

#' @title Infer types for custom call
#' @description
#' Infer the output types for a custom call operation.
#' @param ... Input values.
#' @param call_target_name (`character(1)`)\cr
#'   The name of the custom function to call.
#' @param api_version (`integer(1)`)\cr
#'   The API version.
#' @param has_side_effect (`logical(1)`)\cr
#'   Whether the custom call has side effects.
#' @param backend_config (`list` | `NULL`)\cr
#'   Optional backend configuration as a named list.
#' @param output_types (`list` of [`ValueType`] | `NULL`)\cr
#'   The output types of the custom call. Default is NULL (no outputs).
#' @param operand_layouts,result_layouts Layouts (not used for type inference).
#' @return (`ValueTypes`)\cr
#'   The output types (empty for side-effect only calls).
#' @export
infer_types_custom_call <- function(
  ...,
  call_target_name,
  api_version,
  has_side_effect,
  backend_config,
  output_types,
  operand_layouts,
  result_layouts
) {
  if (is.null(output_types)) {
    return(ValueTypes(list()))
  }

  if (!test_class(output_types, "ValueTypes")) {
    output_types <- ValueTypes(output_types)
  }

  output_types
}

custom_call_impl <- hlo_fn(OpCustomCall, infer_types_custom_call)

#' @title Custom Call Operation
#' @description
#' Create a custom call operation that invokes an external function via the
#' FFI (Foreign Function Interface) API.
#'
#' Note that the attributes `called_computations` and `output_operand_aliases` are not
#' implemented yet.
#'
#' @param ... ([`FuncValue`])\cr
#'   Input values to pass to the custom call.
#' @param call_target_name (`character(1)`)\cr
#'   The name of the registered custom function to call.
#' @param api_version (`integer(1)`)\cr
#'   The API version. Default is 4.
#' @param has_side_effect (`logical(1)`)\cr
#'   Whether the custom call has side effects.
#' @param backend_config ([`CustomOpBackendConfig`] | `NULL`)\cr
#'   Optional backend configuration.
#' @param output_types (`list` of [`ValueType`] | `NULL`)\cr
#'   The output types of the custom call. Default is NULL (no outputs).
#' @param operand_layouts (`list` of `integer()` | `NULL`)\cr
#'   Layouts for each operand in minor-to-major order. Each element is an
#'   integer vector specifying the dimension order. For example, `c(0L, 1L)`
#'   means column-major (dimension 0 varies fastest), while `c(1L, 0L)` means
#'   row-major. Default `NULL` means no layout constraint.
#' @param result_layouts (`list` of `integer()` | `NULL`)\cr
#'   Layouts for each result in minor-to-major order. Same format as
#'   `operand_layouts`.
#' @return ([`FuncValue`] | `list()` | `NULL`)\cr
#'   The output value(s), or NULL for side-effect only calls.
#' @export
hlo_custom_call <- function(
  ...,
  call_target_name,
  api_version = 4L,
  has_side_effect,
  backend_config = NULL,
  output_types = NULL,
  operand_layouts = NULL,
  result_layouts = NULL
) {
  values <- list(...)
  custom_call_impl(
    values = values,
    attrs = list(
      StringAttr(name = "call_target_name", value = call_target_name),
      ScalarAttr(
        name = "api_version",
        value = as.integer(api_version),
        dtype = IntegerType(32L)
      ),
      BoolAttr(name = "has_side_effect", value = has_side_effect)
    ),
    custom_attrs = if (!is.null(backend_config)) {
      list(
        backend_config = backend_config,
        output_types = output_types,
        operand_layouts = operand_layouts,
        result_layouts = result_layouts
      )
    } else {
      list(
        output_types = output_types,
        operand_layouts = operand_layouts,
        result_layouts = result_layouts
      )
    }
  )
}

# Format a single layout as `dense<[0, 1]> : tensor<2xindex>`
repr_layout <- function(layout) {
  layout <- as.integer(layout)
  paste0(
    "dense<[",
    paste(layout, collapse = ", "),
    "]> : tensor<",
    length(layout),
    "xindex>"
  )
}

# Format a list of layouts as `[dense<...>, dense<...>]`
repr_layouts <- function(name, layouts) {
  items <- vapply(layouts, repr_layout, character(1))
  paste0(name, " = [", paste(items, collapse = ", "), "]")
}

#' @export
repr.OpCustomCall <- function(
  x,
  toplevel = TRUE,
  simplify_dense = TRUE,
  ...
) {
  attrs <- x$inputs$attrs
  target_name <- NULL
  for (attr in attrs) {
    if (attr$name == "call_target_name") {
      target_name <- attr$value
      break
    }
  }

  attr_reprs <- vapply(
    attrs,
    repr,
    character(1),
    simplify_dense = simplify_dense
  )
  bec <- x$inputs$custom_attrs$backend_config
  if (!is.null(bec)) {
    attr_reprs <- c(attr_reprs, repr(bec))
  }
  ol <- x$inputs$custom_attrs$operand_layouts
  if (!is.null(ol)) {
    attr_reprs <- c(attr_reprs, repr_layouts("operand_layouts", ol))
  }
  rl <- x$inputs$custom_attrs$result_layouts
  if (!is.null(rl)) {
    attr_reprs <- c(attr_reprs, repr_layouts("result_layouts", rl))
  }
  attrs_str <- paste0("{\n  ", paste(attr_reprs, collapse = ",\n  "), "\n}")

  # Build output part
  outputs_repr <- if (!length(x$outputs)) {
    ""
  } else {
    paste0(repr(x$outputs), " = ")
  }

  paste0(
    outputs_repr,
    "stablehlo.custom_call @",
    target_name,
    "(",
    repr(x$inputs$values),
    ") ",
    attrs_str,
    " : ",
    repr(x$signature)
  )
}
