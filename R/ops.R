#' @include op.R
NULL

# Technicall this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
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

Abs <- new_Op("Abs", "abs")
Add <- new_Op("Add", "add")
AfterAll <- new_Op("AfterAll", "after_all")
And <- new_Op("And", "and")
Atan2 <- new_Op("Atan2", "atan2")
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
