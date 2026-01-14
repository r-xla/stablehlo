OpConstant <- function(value, output = NULL) {
  checkmate::assert_class(value, "Constant")

  base_op <- Op(
    name = OpName("constant"),
    inputs = OpInputs(
      values = OpInputValues(list()),
      funcs = OpInputFuncs(),
      attrs = OpInputAttrs(
        list(
          ConstantAttr(
            name = "value",
            value = value
          )
        )
      )
    ),
    outputs = output %||% OpOutputs(),
    signature = OpSignature(
      input_types = ValueTypes(list()),
      output_types = ValueTypes(list(ValueType(value$type)))
    )
  )
  class(base_op) <- c("OpConstant", "Op")
  base_op
}

#' @title Create a Constant
#' @name hlo_constant
#' @description
#' Create either a "scalar" ([`hlo_scalar`]) or tensor ([`hlo_tensor`]) constant.
#' Strictly speaking, stableHLO "scalars" are simply tensors with 0 dimensions.
#' To create an empty constant (at least one dimension is 0), use [`hlo_empty`].
#' @param value (any)\cr
#'   Value from which to create a constant.
#' @param func ([`Func`])\cr
#'   The function to add the constant to.
#'   Per default, uses the last function created with [`hlo_func`] or [`local_func`].
#' @param dtype (`character(1)`)\cr
#'   One of: `r roxy_dtypes()`.
#' @param shape (`integer()`)\cr
#'   The shape
#' @param ... (any)\cr
#'   Additional arguments.
#' @export
#' @examples
#' hlo_scalar(1L, dtype = "i32", func = Func())
#' hlo_scalar(1, dtype = "f32", func = Func())
#' hlo_scalar(TRUE, func = Func())
#' hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), dtype = "f32", func = Func())
#' hlo_empty(dtype = "f32", shape = c(0, 3), func = Func())
hlo_scalar <- function(value, ..., dtype = NULL, func = NULL) {
  func <- func %??% .current_func()
  UseMethod("hlo_scalar")
}

#' @export
hlo_scalar.logical <- function(value, ..., func = NULL) {
  func <- func %??% .current_func()
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  impl_hlo_constant(value, dtype = "pred", func = func, shape = integer())
}

#' @export
hlo_scalar.double <- function(value, ..., dtype = NULL, func = NULL) {
  func <- func %??% .current_func()
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  impl_hlo_constant(value, dtype = dtype, func = func, shape = integer())
}

#' @export
hlo_scalar.integer <- function(value, ..., dtype = NULL, func = NULL) {
  func <- func %??% .current_func()
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  if (!is.null(dtype) && grepl("^ui", dtype) && any(value < 0L)) {
    cli_abort("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func, shape = integer())
}

#' @export
hlo_scalar.PJRTBuffer <- function(value, ..., func = NULL) {
  func <- func %??% .current_func()
  if (!identical(shape(value), integer())) {
    cli_abort("hlo_scalar expects a scalar")
  }
  impl_hlo_constant(
    value,
    dtype = as.character(pjrt::elt_type(value)),
    func = func,
    shape = integer()
  )
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- function(value, ..., dtype = NULL, shape = NULL, func = NULL) {
  UseMethod("hlo_tensor")
}

#' @export
hlo_tensor.array <- function(value, ..., dtype = NULL, func = NULL) {
  func <- func %??% .current_func()
  if (
    is.integer(value) &&
      !is.null(dtype) &&
      grepl(dtype, "ui") &&
      any(value < 0)
  ) {
    cli_abort("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func, shape = dim(value))
}

#' @export
hlo_tensor.integer <- function(
  value,
  ...,
  dtype = NULL,
  shape = NULL,
  func = NULL
) {
  func <- func %??% .current_func()
  shape <- shape %||% get_dims(value)
  impl_hlo_constant(value, dtype = dtype, func = func, shape = shape)
}

#' @export
hlo_tensor.logical <- function(value, ..., shape = NULL, func = NULL) {
  func <- func %??% .current_func()
  shape <- shape %||% get_dims(value)
  impl_hlo_constant(value, dtype = "i1", func = func, shape = shape)
}

#' @export
hlo_tensor.double <- function(
  value,
  ...,
  dtype = NULL,
  shape = NULL,
  func = NULL
) {
  func <- func %??% .current_func()
  shape <- shape %||% get_dims(value)
  impl_hlo_constant(value, dtype = dtype, func = func, shape = shape)
}

#' @export
hlo_tensor.PJRTBuffer <- function(value, ..., func = NULL) {
  func <- func %??% .current_func()
  impl_hlo_constant(
    value,
    dtype = as.character(pjrt::elt_type(value)),
    func = func,
    shape = shape(value)
  )
}

#' @rdname hlo_constant
#' @export
hlo_empty <- function(dtype, shape, func = NULL) {
  func <- func %??% .current_func()
  data <- if (dtype == "pred") {
    logical()
  } else if (startsWith(dtype, "f")) {
    double()
  } else {
    integer()
  }

  if (!any(shape == 0L)) {
    cli_abort("Shape must contain at least one 0-dimension")
  }

  impl_hlo_constant(
    data,
    dtype = dtype,
    func = func,
    shape = shape
  )
}

impl_hlo_constant <- function(value, dtype, func, shape) {
  if (length(shape) && !test_class(value, "PJRTBuffer") && length(value) > 1) {
    # stablehlo allows e.g. dense<0.0> : tensor<2x2xf32>, so if length(value) == 1
    # we don't need to recycle to keep the program size smaller
    value <- array(value, dim = shape)
  }
  const_value <- r_to_constant(value, dtype = dtype, shape = shape)
  value_id <- ValueId()
  op <- OpConstant(
    const_value,
    OpOutputs(
      items = list(
        OpOutput(
          value_id
        )
      )
    )
  )
  func$body <- FuncBody(c(func$body, list(op)))

  FuncValue(
    value_id = value_id,
    value_type = ValueType(const_value$type),
    func = func
  )
}

#' @rdname hlo_constant
#' @export
infer_types_constant <- function(value) {
  ValueTypes(list(ValueType(value$type)))
}

#' @export
repr.OpConstant <- function(x, ...) {
  paste0(
    repr(x$outputs),
    " = ",
    repr(x$name),
    " ",
    repr(x$inputs, simplify_dense = FALSE),
    ": ",
    repr(x$signature)
  )
}

globals[["infer_fn"]][["constant"]] <- infer_types_constant
