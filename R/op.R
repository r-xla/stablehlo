#' @include value_id.R
#' @include types.R
#' @include constant.R
#' @include func.R
#'
NULL

# Attribute Types ----------------------------------------------------------

#' @title OpInputAttr
#' @description
#' Base class for operation input attributes.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (any)\cr
#'   The value of the attribute.
#' @param dtype ([`DataType`])\cr
#'   The dtype of the attribute.
#' @return (`OpInputAttr`)
#' @export
OpInputAttr <- function(name, value, dtype) {
  checkmate::assert_string(name)

  structure(
    list(name = name, value = value, dtype = dtype),
    class = "OpInputAttr"
  )
}

#' @title ScalarAttr
#' @description
#' An attribute holding a scalar value with an associated dtype.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`numeric(1)` or `logical(1)`)\cr
#'   The scalar value.
#' @param dtype ([`DataType`])\cr
#'   The dtype of the scalar (e.g., `as_dtype("i32")`, `as_dtype("f32")`, `as_dtype("bool")`).
#' @return `ScalarAttr`
#' @export
ScalarAttr <- function(name, value, dtype) {
  checkmate::assert_string(name)
  assert_dtype(dtype)

  constant <- r_to_constant(value, dtype = repr(dtype), shape = integer())

  structure(
    list(name = name, value = constant, dtype = dtype),
    class = c("ScalarAttr", "OpInputAttr")
  )
}

#' @export
repr.ScalarAttr <- function(x, simplify_dense = TRUE, ...) {
  type_repr <- repr(x$dtype)
  data <- x$value$data
  value_repr <- if (is_dtype_bool(x$dtype)) {
    sprintf("%s : %s", tolower(as.character(data)), type_repr)
  } else if (
    is_dtype_int(x$dtype) ||
      is_dtype_uint(x$dtype)
  ) {
    sprintf("%d : %s", as.integer(data), type_repr)
  } else {
    precision <- dtype_bits(x$dtype)
    sprintf(
      "%s : %s",
      format_double(as.double(data), precision = precision),
      type_repr
    )
  }
  paste0(x$name, " = ", value_repr)
}

#' @title BoolAttr
#' @description
#' An attribute holding a boolean value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`logical(1)`)\cr
#'   The boolean value.
#' @return `BoolAttr`
#' @export
BoolAttr <- function(name, value) {
  checkmate::assert_string(name)
  checkmate::assert_flag(value)

  constant <- r_to_constant(value, dtype = "i1", shape = integer())

  structure(
    list(name = name, value = constant, dtype = as_dtype("bool")),
    class = c("BoolAttr", "OpInputAttr")
  )
}

#' @export
repr.BoolAttr <- function(x, simplify_dense = TRUE, ...) {
  paste0(x$name, " = ", tolower(as.character(x$value$data)))
}

#' @title StringAttr
#' @description
#' An attribute holding a string value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`character(1)`)\cr
#'   The string value.
#' @return `StringAttr`
#' @export
StringAttr <- function(name, value) {
  checkmate::assert_string(name)
  checkmate::assert_string(value)

  structure(
    list(name = name, value = value),
    class = c("StringAttr", "OpInputAttr")
  )
}

#' @export
repr.StringAttr <- function(x, simplify_dense = TRUE, ...) {
  sprintf("%s = \"%s\"", x$name, x$value)
}

#' @title ConstantAttr
#' @description
#' An attribute holding a constant value.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (`Constant`)\cr
#'   The value of the attribute.
#' @param simplify_dense (`logical(1)`)\cr
#'   Whether to simplify dense representation. Set to `FALSE` for multi-dimensional arrays.
#' @return (`ConstantAttr`)
#' @export
ConstantAttr <- function(name, value, simplify_dense = TRUE) {
  checkmate::assert_string(name)
  checkmate::assert_class(value, "Constant")
  checkmate::assert_flag(simplify_dense)

  structure(
    list(name = name, value = value, simplify_dense = simplify_dense),
    class = c("ConstantAttr", "OpInputAttr")
  )
}

#' @export
repr.ConstantAttr <- function(x, simplify_dense = TRUE, ...) {
  # TODO: This should be handled nicer
  use_simplify <- x$simplify_dense && simplify_dense
  paste0(
    x$name,
    " = ",
    repr(x$value, simplify_dense = use_simplify)
  )
}

#' @title Create a ConstantAttr from R values
#' @description
#' Helper function to create a ConstantAttr from R values.
#' @param name (`character(1)`)\cr
#'   The name of the attribute.
#' @param value (any)\cr
#'   The R value to convert to a constant.
#' @param dtype (`character(1)` | `NULL`)\cr
#'   The dtype of the constant. If NULL, inferred from value.
#' @param shape (`integer()` | `NULL`)\cr
#'   The shape of the constant. If NULL, inferred from value.
#' @param simplify_dense (`logical(1)`)\cr
#'   Whether to simplify dense representation. Set to `FALSE` for multi-dimensional arrays.
#' @return (`ConstantAttr`)
#' @export
constant_attr <- function(
  name,
  value,
  dtype = NULL,
  shape = NULL,
  simplify_dense = TRUE
) {
  if (is.null(shape)) {
    shape <- if (length(value) == 1L) integer() else length(value)
  }
  constant <- r_to_constant(value, dtype = dtype, shape = shape)
  ConstantAttr(name = name, value = constant, simplify_dense = simplify_dense)
}

# Op descriptors and rendering ---------------------------------------------

#' Define a new Op
#'
#' Creates the descriptor of a StableHLO operation, consumed by `hlo_fn()`.
#'
#' @param classname Name of the op (kept for readability at the call sites).
#' @param mnemonic The operation mnemonic.
#' @param dialect The MLIR dialect (`"stablehlo"` or `"chlo"`). Defaults to `"stablehlo"`.
#' @param render (`function(ctx)` | `NULL`)\cr
#'   Custom render function producing the op's MLIR line. `NULL` uses the
#'   default assembly/generic format. The `ctx` argument is a list with
#'   fields `mnemonic`, `dialect`, `outputs_str`, `values_str`,
#'   `in_type_strs`, `out_type_strs`, `sig_str`, `attrs`, `attrs_str`,
#'   `funcs_str` and `custom_attrs`.
#' @return A descriptor `list` for use with `hlo_fn()`.
#' @keywords internal
new_Op <- function(classname, mnemonic, dialect = "stablehlo", render = NULL) {
  list(mnemonic = mnemonic, dialect = dialect, render = render)
}

# Renders the ` {\n<attr>,\n<attr>\n}` block of an op line ("" if no attrs).
render_attrs <- function(attrs, simplify_dense = TRUE) {
  if (length(attrs) == 0L) {
    return("")
  }
  a <- paste0(
    vapply(
      attrs,
      function(item) repr(item, simplify_dense = simplify_dense),
      character(1)
    ),
    collapse = ",\n"
  )
  paste0(" {\n", a, "\n}")
}

# Renders the `({...}, {...})` region block of an op line ("" if no funcs).
render_funcs <- function(funcs) {
  if (length(funcs) == 0L) {
    return("")
  }
  paste0(
    "(",
    paste0(
      vapply(
        funcs,
        function(x) {
          repr.OpInputFunc(OpInputFunc(x$inputs, func_lines(x)))
        },
        character(1)
      ),
      collapse = ", "
    ),
    ")"
  )
}

# Default op rendering. Assembly format (`%0 = stablehlo.op %x, %y : type`)
# is used when the op is a plain stablehlo op without attributes or regions
# and all input and output types are identical; otherwise the generic format
# (`%0 = "stablehlo.op" (%x, %y): (type1, type2) -> (output_type)`).
render_op_default <- function(ctx) {
  if (
    ctx$dialect == "stablehlo" &&
      length(ctx$in_type_strs) > 0L &&
      nchar(ctx$funcs_str) == 0L &&
      nchar(ctx$attrs_str) == 0L &&
      length(ctx$custom_attrs) == 0L
  ) {
    strs <- c(ctx$in_type_strs, ctx$out_type_strs)
    if (all(strs == strs[[1L]])) {
      return(paste0(
        ctx$outputs_str,
        " = ",
        "stablehlo.",
        ctx$mnemonic,
        " ",
        ctx$values_str,
        " : ",
        strs[[1L]]
      ))
    }
  }
  paste0(
    ctx$outputs_str,
    " = \"",
    ctx$dialect,
    ".",
    ctx$mnemonic,
    "\" (",
    ctx$values_str,
    ")",
    ctx$funcs_str,
    ctx$attrs_str,
    ": ",
    ctx$sig_str
  )
}
