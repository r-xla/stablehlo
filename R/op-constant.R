OpConstant <- S7::new_class(
  "OpConstant",
  parent = Op,
  constructor = function(value, output = NULL) {
    new_object(
      Op(
        name = OpName(OpMnemonic("constant")),
        inputs = OpInputs(
          values = OpInputValues(list()),
          funcs = OpInputFuncs(),
          attrs = OpInputAttrs(
            list(
              OpInputAttr(
                "value",
                value
              )
            )
          )
        ),
        outputs = output %||% OpOutputs(),
        signature = OpSignature(
          input_types = ValueTypes(list()),
          output_types = ValueTypes(list(value@value@type))
        )
      )
    )
  }
)

op_constant <- function(value, dtype = NULL) {
  const_value <- r_to_constant(value, dtype = dtype)
  OpConstant(const_value)
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
hlo_scalar <- S7::new_generic(
  "hlo_scalar",
  "value",
  function(value, ..., func = .current_func()) {
    S7::S7_dispatch()
  }
)

S7::method(hlo_scalar, S7::class_logical) <- function(
  value,
  ...,
  func = .current_func()
) {
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    cli_abort("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, dtype = "pred", func = func)
}

S7::method(hlo_scalar, S7::class_double) <- function(
  value,
  ...,
  dtype = NULL,
  func = .current_func()
) {
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    cli_abort("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

S7::method(hlo_scalar, S7::class_integer) <- function(
  value,
  ...,
  dtype = NULL,
  func = .current_func()
) {
  if (length(value) != 1L) {
    cli_abort("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    cli_abort("Data for constants must not contain NA values.")
  }
  if (!is.null(dtype) && grepl("^ui", dtype) && any(value < 0L)) {
    cli_abort("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

S7::method(hlo_scalar, S7::new_S3_class("PJRTBuffer")) <- function(
  value,
  ...,
  func = .current_func()
) {
  impl_hlo_constant(
    tengen::as_array(value),
    dtype = as.character(pjrt::elt_type(value)),
    func = func
  )
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- S7::new_generic(
  "hlo_tensor",
  "value",
  function(value, ..., func = .current_func()) {
    S7::S7_dispatch()
  }
)

S7::method(hlo_tensor, S7::new_S3_class("array")) <- function(
  value,
  ...,
  dtype = NULL,
  func = .current_func()
) {
  if (anyNA(value)) {
    cli_abort("Data for constants must not contain NA values.")
  }
  if (
    is.integer(value) &&
      !is.null(dtype) &&
      grepl(dtype, "ui") &&
      any(value < 0)
  ) {
    cli_abort("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

S7::method(hlo_tensor, S7::class_integer) <- function(
  value,
  ...,
  dtype = NULL,
  shape = NULL,
  func = .current_func()
) {
  shape <- shape %??% get_dims(value)
  impl_hlo_constant(array(value, dim = shape), dtype = dtype, func = func)
}

S7::method(hlo_tensor, S7::class_logical) <- function(
  value,
  ...,
  shape = NULL,
  func = .current_func()
) {
  shape <- shape %??% get_dims(value)
  impl_hlo_constant(array(value, dim = shape), dtype = "i1", func = func)
}

S7::method(hlo_tensor, S7::class_double) <- function(
  value,
  ...,
  dtype = NULL,
  shape = NULL,
  func = .current_func()
) {
  shape <- shape %??% get_dims(value)
  impl_hlo_constant(array(value, dim = shape), dtype = dtype, func = func)
}

S7::method(hlo_tensor, S7::new_S3_class("PJRTBuffer")) <- function(
  value,
  ...,
  func = .current_func()
) {
  impl_hlo_constant(
    tengen::as_array(value),
    dtype = as.character(pjrt::elt_type(value)),
    func = func
  )
}

#' @rdname hlo_constant
#' @export
hlo_empty <- function(dtype, shape, func = .current_func()) {
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
    array(data, dim = shape),
    dtype = dtype,
    func = func
  )
}

impl_hlo_constant <- function(value, dtype, func) {
  const_value <- r_to_constant(value, dtype = dtype)
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
  func@body <- FuncBody(c(func@body@items, list(op)))

  FuncVariable(
    value_id = value_id,
    value_type = ValueType(const_value@value@type),
    func = func
  )
}

infer_types_constant <- function(value) {
  ValueTypes(list(value@value@type))
}

method(repr, OpConstant) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = FALSE),
    ": ",
    repr(x@signature)
  )
}
