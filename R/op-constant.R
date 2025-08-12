OpConstant <- S7::new_class(
  "OpConstant",
  parent = Op,
  constructor = function(value, output = NULL) {
    new_object(
      Op,
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
  }
)

op_constant <- function(value, elt_type = NULL) {
  const_value <- r_to_constant(value, elt_type = elt_type)
  OpConstant(const_value)
}

#' @title Create a Constant
#' @description
#' Create either a scalar or tensor constant.
#' Note that strictly speaking, stableHLO 'scalars' are simply tensors with 0 dimensions.
#' @param value (any)\cr
#'   Value from which to create a constant.
#' @param elt_type (`character(1)`)\cr
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
hlo_scalar <- function(value, elt_type = NULL, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_scalar")
}

#' @rdname hlo_constant
#' @export
hlo_scalar.logical <- function(value, elt_type = NULL, ...) {
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

#' @rdname hlo_constant
#' @export
hlo_scalar.double <- hlo_scalar.logical

#' @rdname hlo_constant
#' @export
hlo_scalar.integer <- function(value, elt_type = NULL, ...) {
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (!is.null(elt_type) && grepl("^ui", elt_type) && any(value < 0L)) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- function(value, elt_type = NULL, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_tensor")
}

#' @rdname hlo_constant
#' @export
hlo_tensor.array <- function(value, elt_type = NULL, ...) {
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (
    is.integer(value) &&
      !is.null(elt_type) &&
      grepl(elt_type, "ui") &&
      any(value < 0)
  ) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

#' @rdname hlo_constant
#' @param shape (`integer()`)\cr
#'   Shape of the tensor.
#'   If not specified, the shape is inferred from the data.
#' @export
hlo_tensor.integer <- function(
  value,
  elt_type = NULL,
  shape = get_dims(value),
  ...
) {
  impl_hlo_constant(array(value, dim = shape), elt_type = elt_type)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.logical <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.double <- hlo_tensor.integer


impl_hlo_constant <- function(value, elt_type) {
  const_value <- r_to_constant(value, elt_type = elt_type)
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
      body = FuncBody(
        items = list(
          op
        )
      ),
      outputs = FuncOutputs(
        items = list(
          FuncOutput(
            type = ValueType(const_value@value@type)
          )
        )
      )
    )
  )
}

infer_types_constant <- function(value) {
  ValueTypes(list(value@value@type))
}
