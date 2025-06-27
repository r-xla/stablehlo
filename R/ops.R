#' @include op.R

Abs <- new_class(
  "Abs",
  parent = Op,
  constructor = function(inputs, outputs, signature) {
    new_object(
      Op,
      name = OpName(OpMnemonic("abs")),
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )
  }
)

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
  repr(super(x, to = Op))
}

stablehlo_constant <- function(value) {
  # First convert the R value to a Constant value
  const_value <- r_to_constant(value)
  
  # Then create the constant operation
  OpConstant(const_value)
}
