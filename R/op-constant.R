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
              OpInputAttrName("value"),
              OpInputAttrValue(value)
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

method(repr, OpConstant) <- function(x, ...) {
  repr(S7::super(x, to = Op))
}

op_constant <- function(value, elt_type = NULL) {
  const_value <- r_to_constant(value, elt_type = elt_type)
  OpConstant(const_value)
}

#' @title Create a Constant
#' @description
#' Create either a scalar or tensor constant.
#' @name hlo_constant
#' @export
#' @examples
#' hlo_scalar(1L)
#' hlo_tensor(1L, "i16")
#' hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), "f64")
hlo_scalar <- function(value, elt_type = NULL, ...) {
  UseMethod("hlo_scalar")
}

#' @rdname hlo_constant
#' @export
hlo_scalar.logical <- function(value, elt_type = NULL, ...) {
  if (length(value) != 1L) {
    stop("hlo_scalar expects a single atomic value.")
  }
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

#' @export
hlo_scalar.double <- hlo_scalar.logical

#' @export
hlo_tensor <- function(value, elt_type = NULL, ...) {
  UseMethod("hlo_tensor")
}

#' @export
hlo_tensor.array <- function(value, elt_type = NULL, ...) {
  if (anyNA(value)) {
    stop("Data for constants must not contain NA values.")
  }
  if (is.integer(value) && grepl(elt_type, "ui") && any(value < 0)) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}


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
