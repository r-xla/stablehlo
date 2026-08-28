#' @include op.R hlo.R type_inference.R
NULL

render_custom_call <- function(ctx) {
  target_name <- NULL
  for (attr in ctx$attrs) {
    if (attr$name == "call_target_name") {
      target_name <- attr$value
      break
    }
  }

  attr_reprs <- vapply(ctx$attrs, repr, character(1))
  bec <- ctx$custom_attrs$backend_config
  if (!is.null(bec)) {
    attr_reprs <- c(attr_reprs, repr(bec))
  }
  ol <- ctx$custom_attrs$operand_layouts
  if (!is.null(ol)) {
    attr_reprs <- c(attr_reprs, repr_layouts("operand_layouts", ol))
  }
  rl <- ctx$custom_attrs$result_layouts
  if (!is.null(rl)) {
    attr_reprs <- c(attr_reprs, repr_layouts("result_layouts", rl))
  }
  ooa <- ctx$custom_attrs$output_operand_aliases
  if (!is.null(ooa)) {
    attr_reprs <- c(attr_reprs, repr_output_operand_aliases(ooa))
  }
  attrs_str <- paste0("{\n  ", paste(attr_reprs, collapse = ",\n  "), "\n}")

  # Build output part
  outputs_repr <- if (!nzchar(ctx$outputs_str)) {
    ""
  } else {
    paste0(ctx$outputs_str, " = ")
  }

  paste0(
    outputs_repr,
    "stablehlo.custom_call @",
    target_name,
    "(",
    ctx$values_str,
    ") ",
    attrs_str,
    " : ",
    ctx$sig_str
  )
}

OpCustomCall <- new_Op(
  "OpCustomCall",
  "custom_call",
  render = render_custom_call
)

#' @title CustomOpBackendConfig
#' @description
#' A backend configuration as a list of typed attributes for custom operations.
#' Each element must be a [`BoolAttr`], [`StringAttr`], [`ScalarAttr`] or
#' [`ConstantAttr`]. All attribute names must be unique.
#'
#' A [`ConstantAttr`] built with [`constant_attr()`] carries a vector rather
#' than a scalar and is what an XLA FFI handler decodes as
#' `Span<const T>`. The dtype has to match the handler's element type
#' exactly -- `Span<const int64_t>` needs `"i64"`, not the `"i32"` that
#' [`constant_attr()`] would infer from an R integer vector.
#' @param items (`list`)\cr
#'   A list of [`BoolAttr`], [`StringAttr`], [`ScalarAttr`] or
#'   [`ConstantAttr`] objects.
#' @return `CustomOpBackendConfig`
#' @examples
#' CustomOpBackendConfig(list(
#'   StringAttr(name = "mode", value = "fast"),
#'   constant_attr(name = "axes", value = c(0L, 2L), dtype = "i64")
#' ))
#' @export
CustomOpBackendConfig <- function(items = list()) {
  checkmate::assert_list(
    items,
    types = c("BoolAttr", "StringAttr", "ScalarAttr", "ConstantAttr")
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

  if (!inherits(output_types, "ValueTypes")) {
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
#' Note that the attribute `called_computations` is not implemented yet.
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
#'   Optional backend configuration. Its attributes are what the FFI
#'   handler receives through `.Attr<T>("name")` or `.Attrs<Dictionary>()`.
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
#' @param output_operand_aliases (`list` of [`OutputOperandAlias`] | `NULL`)\cr
#'   Buffer aliases between operands and results. XLA then hands the handler
#'   the *same* pointer for the aliased operand and result, so a handler that
#'   works in place does not force a copy. `NULL` (the default) means no
#'   aliasing.
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
  result_layouts = NULL,
  output_operand_aliases = NULL
) {
  if (!is.null(output_operand_aliases)) {
    checkmate::assert_list(
      output_operand_aliases,
      types = "OutputOperandAlias",
      min.len = 1L
    )
  }
  values <- list(...)
  custom_call_impl(
    values = values,
    attrs = list(
      StringAttr(name = "call_target_name", value = call_target_name),
      ScalarAttr(
        name = "api_version",
        value = as.integer(api_version),
        dtype = as_dtype("i32")
      ),
      BoolAttr(name = "has_side_effect", value = has_side_effect)
    ),
    custom_attrs = if (!is.null(backend_config)) {
      list(
        backend_config = backend_config,
        output_types = output_types,
        operand_layouts = operand_layouts,
        result_layouts = result_layouts,
        output_operand_aliases = output_operand_aliases
      )
    } else {
      list(
        output_types = output_types,
        operand_layouts = operand_layouts,
        result_layouts = result_layouts,
        output_operand_aliases = output_operand_aliases
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

#' @title OutputOperandAlias
#' @description
#' Declares that a [`hlo_custom_call()`] result shares its buffer with one of
#' the call's operands. XLA then hands the handler the same pointer for both,
#' which is how an in-place kernel avoids a copy -- and why a handler that
#' overwrites its input must be written to tolerate it.
#'
#' Indices are 0-based, as everywhere in StableHLO. The `*_tuple_indices` are
#' the path into a tuple-typed result or operand and stay empty for the
#' ordinary case of a call with plain tensor results.
#' @param operand_index (`integer(1)`)\cr
#'   Which operand of the custom call the result aliases.
#' @param output_tuple_indices (`integer()`)\cr
#'   Path into the result tuple. Empty (the default) for a single result;
#'   for a call with several results, the index of the aliased one.
#' @param operand_tuple_indices (`integer()`)\cr
#'   Path into the operand, if that operand is a tuple. Empty by default.
#' @return `OutputOperandAlias`
#' @examples
#' # the single result is written into the buffer of the first operand
#' OutputOperandAlias(operand_index = 0L)
#' @export
OutputOperandAlias <- function(
  operand_index,
  output_tuple_indices = integer(),
  operand_tuple_indices = integer()
) {
  checkmate::assert_int(operand_index, lower = 0L)
  checkmate::assert_integerish(
    output_tuple_indices,
    lower = 0L,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    operand_tuple_indices,
    lower = 0L,
    any.missing = FALSE
  )

  structure(
    list(
      operand_index = as.integer(operand_index),
      output_tuple_indices = as.integer(output_tuple_indices),
      operand_tuple_indices = as.integer(operand_tuple_indices)
    ),
    class = "OutputOperandAlias"
  )
}

#' @export
repr.OutputOperandAlias <- function(x, ...) {
  paste0(
    "#stablehlo.output_operand_alias<output_tuple_indices = [",
    paste(x$output_tuple_indices, collapse = ", "),
    "], operand_index = ",
    x$operand_index,
    ", operand_tuple_indices = [",
    paste(x$operand_tuple_indices, collapse = ", "),
    "]>"
  )
}

#' @export
print.OutputOperandAlias <- function(x, ...) {
  cat(repr(x), "\n", sep = "")
  invisible(x)
}

# Format a list of aliases as `output_operand_aliases = [#stablehlo...., ...]`
repr_output_operand_aliases <- function(aliases) {
  items <- vapply(aliases, repr, character(1))
  paste0("output_operand_aliases = [", paste(items, collapse = ", "), "]")
}
