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
        attrs = OpInputAttrs(list(
          OpInputAttr(
            OpInputAttrName("value"),
            OpInputAttrValue(value)
          )
        ))
      ),
      outputs = output %||% OpOutputs(),
      signature = OpSignature(
        input_types = ValueTypes(list()),
        output_types = ValueTypes(list(value@value@type))
      )
    )
  }
)

method(repr, OpConstant) <- function(x) {
  repr(S7::super(x, to = Op))
}

op_constant <- function(value) {
  const_value <- r_to_constant(value)
  OpConstant(const_value)
}

hlo_constant <- function(value) {
  const_value <- r_to_constant(value)
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
