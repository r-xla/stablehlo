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
#' @param dtype (`character(1)`)\cr
#'   String for element type.
#'   Can be one of f64, f32, u8, u16, u32, u64, i8, i16, i32, i64, pred.
#' @param ... (any)\cr
#'   Additional arguments.
#' @name hlo_constant
#' @export
#' @examples
#' hlo_scalar(1L, "i32")
#' hlo_scalar(1, "f32")
#' hlo_scalar(TRUE)
#' hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), "f32")
hlo_scalar <- function(value, dtype = NULL, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_scalar")
}

#' @rdname hlo_constant
#' @export
hlo_scalar.logical <- function(value, dtype = NULL, ...) {
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_scalar.double <- hlo_scalar.logical

#' @rdname hlo_constant
#' @export
hlo_scalar.integer <- function(value, dtype = NULL, ...) {
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (!is.null(dtype) && grepl("^ui", dtype) && any(value < 0L)) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- function(value, dtype = NULL, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_tensor")
}

#' @rdname hlo_constant
#' @export
hlo_tensor.array <- function(value, dtype = NULL, ...) {
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
  impl_hlo_constant(value, dtype = dtype)
}

#' @rdname hlo_constant
#' @param shape (`integer()`)\cr
#'   Shape of the tensor.
#'   If not specified, the shape is inferred from the data.
#' @export
hlo_tensor.integer <- function(
  value,
  dtype = NULL,
  shape = get_dims(value),
  ...
) {
  impl_hlo_constant(array(value, dim = shape), dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.logical <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.double <- hlo_tensor.integer


impl_hlo_constant <- function(value, dtype) {
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

  FuncVariable(
    value_id = value_id,
    value_type = ValueType(const_value@value@type),
    func = Func(
      id = FuncId("main"),
      body = FuncBody(
        items = list(
          op
        )
      ),
      outputs = FuncOutputs()
    )
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
