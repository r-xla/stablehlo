#' @include op.R
NULL

Return <- new_class(
  "Return",
  parent = Op,
  constructor = function(inputs, outputs = OpOutputs(), signature = NULL) {
    if (length(outputs@items)) {
      stop("Return op must not have outputs.")
    }
    if (length(signature@output_types@items)) {
      stop("Invalid signature for Return op.")
    }
    new_object(
      Op,
      name = OpName(OpMnemonic("return")),
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )
  }
)

# Technicall this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
Return <- new_Op("Return", "return")
Abs <- new_Op("Abs", "abs")
Add <- new_Op("Add", "add")
If <- new_Op("If", "if")
Case <- new_Op("Case", "case")


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

stablehlo_constant <- function(value) {
  # First convert the R value to a Constant value
  const_value <- r_to_constant(value)

  # Then create the constant operation
  OpConstant(const_value)
}


stablehlo_add <- stablehlo_fn(Add, infer_types_add)
stablehlo_return <- stablehlo_fn(Return, infer_types_return, TRUE)
