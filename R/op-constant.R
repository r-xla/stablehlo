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
#' @description
#' Create either a scalar or tensor constant.
#' Note that strictly speaking, stableHLO 'scalars' are simply tensors with 0 dimensions.
#' @param value (any)\cr
#'   Value from which to create a constant.
#' @param ... (any)\cr
#'   Additional arguments including:
#'   \itemize{
#'     \item \code{dtype} (`character(1)`): String for element type.
#'       Can be one of f64, f32, u8, u16, u32, u64, i8, i16, i32, i64, pred.
#'     \item \code{shape} (`integer()`): Shape of the tensor (for hlo_tensor only).
#'       If not specified, the shape is inferred from the data.
#'   }
#' @param func ([`Func`])\cr
#'   The function to add the constant to.
#'   Per default, uses the last function created with [`hlo_func`] or [`local_func`].
#' @name hlo_constant
#' @export
#' @examples
#' hlo_scalar(1L, dtype = "i32")
#' hlo_scalar(1, dtype = "f32")
#' hlo_scalar(TRUE)
#' hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), dtype = "f32")
hlo_scalar <- function(value, ..., func = .current_func()) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_scalar")
}

#' @rdname hlo_constant
#' @export
hlo_scalar.logical <- function(value, ..., func = .current_func()) {
  args <- list(...)
  dtype <- args$dtype
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

#' @rdname hlo_constant
#' @export
hlo_scalar.double <- hlo_scalar.logical

#' @rdname hlo_constant
#' @export
hlo_scalar.integer <- function(value, ..., func = .current_func()) {
  args <- list(...)
  dtype <- args$dtype
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (!is.null(dtype) && grepl("^ui", dtype) && any(value < 0L)) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

#' @export
hlo_scalar.PJRTBuffer <- function(value, ..., func = .current_func()) {
  impl_hlo_constant(
    as_array(value),
    dtype = as.character(dtype(value)),
    func = func
  )
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- function(value, ..., func = .current_func()) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_tensor")
}

#' @rdname hlo_constant
#' @export
hlo_tensor.array <- function(value, ..., func = .current_func()) {
  args <- list(...)
  dtype <- args$dtype
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (
    is.integer(value) &&
      !is.null(dtype) &&
      grepl(dtype, "ui") &&
      any(value < 0)
  ) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype, func = func)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.integer <- function(value, ..., func = .current_func()) {
  args <- list(...)
  dtype <- args$dtype
  shape <- args$shape %||% get_dims(value)
  impl_hlo_constant(array(value, dim = shape), dtype = dtype, func = func)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.logical <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.double <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.PJRTBuffer <- function(value, ..., func = .current_func()) {
  impl_hlo_constant(
    as_array(value),
    dtype = as.character(dtype(value)),
    func = func
  )
}

hlo_empty <- function(dtype, shape, func = .current_func()) {
  data <- if (dtype == "pred") {
    logical()
  } else {
    integer()
  }

  if (!any(shape == 0L)) {
    stop("Shape must contain at least one 0-dimension")
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
