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

hlo_scalar <- function(value, elt_type = NULL) {
  if (!is.atomic(value) || length(value) != 1L) {
    stop("hlo_scalar expects a single atomic value.")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

hlo_tensor <- function(value, elt_type = NULL) {
  if (!is.array(value)) {
    stop("hlo_tensor expects an R array.")
  }
  impl_hlo_constant(value, elt_type = elt_type)
}

impl_hlo_constant <- function(value, elt_type = NULL) {
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

  # Then create the constant operation
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
