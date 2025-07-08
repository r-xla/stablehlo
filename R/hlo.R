#' @include type_inference.R
NULL

hlo_fn <- function(op_class, type_inference, return_func = FALSE) {
  # values: list of FuncPointer
  # funcs: list of Func
  # attrs: list of Constant
  function(values, funcs = NULL, attrs = NULL) {
    lapply(values, function(x) {
      if (!inherits(x, FuncPointer)) {
        stop("All arguments must be FuncPointers")
      }
    })
    lapply(funcs, function(x) {
      if (!inherits(x, Func)) {
        stop("All functions must be Func objects")
      }
    })
    lapply(attrs, function(x) {
      if (!inherits(x, Constant)) {
        stop("All attributes must be Constant objects")
      }
    })

    op_input_funcs <- OpInputFuncs(lapply(funcs, function(x) {
      OpInputFunc(
        inputs = x@inputs,
        body = x@body
      )
    }))

    op_input_attrs <- OpInputAttrs(lapply(seq_along(attrs), function(i) {
      attr <- attrs[[i]]
      name <- names(attrs)[i]
      OpInputAttr(
        name = OpInputAttrName(name),
        value = OpInputAttrValue(attr)
      )
    }))

    func <- merge_funcs(lapply(values, function(x) x@func))
    inputs <- OpInputs(
      OpInputValues(lapply(values, function(x) OpInputValue(x@value_id))),
      funcs = op_input_funcs,
      attrs = op_input_attrs
    )

    infer_args <- lapply(values, function(x) x@value_type)

    if (length(funcs) > 0L) {
      infer_args <- c(infer_args, funcs)
    }
    if (length(attrs) > 0L) {
      infer_args <- c(infer_args, attrs)
    }

    output_types <- rlang::exec(type_inference, !!!infer_args)
    nout <- length(output_types@items)

    output_value_ids = replicate(nout, ValueId(), simplify = FALSE)
    outputs = OpOutputs(lapply(output_value_ids, OpOutput))

    signature <- OpSignature(
      input_types = ValueTypes(lapply(values, function(x) x@value_type)),
      output_types = output_types
    )

    op <- op_class(
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )

    func@body <- FuncBody(c(func@body@items, list(op)))

    if (return_func) {
      func@outputs <- FuncOutputs(lapply(values, function(x) {
        FuncOutput(type = x@value_type)
      }))
      return(func)
    }

    if (nout == 1L) {
      return(FuncPointer(
        value_id = output_value_ids[[1L]],
        value_type = output_types@items[[1L]],
        func = func
      ))
    }
    lapply(seq_len(nout), function(i) {
      FuncPointer(
        value_id = output_value_ids[[i]],
        value_type = output_types@items[[i]],
        func = func
      )
    })
  }
}

#' @title Create a input to a function
#' @param argname (`character()`)\cr
#' @param type ([`ValueType`])\cr
#' @param shape (`integer()`)\cr
#' @param func_id ([`FuncId`])\cr
#' @export
hlo_input <- function(argname, type, shape = integer(), func_id = FuncId()) {
  value_id <- ValueId(argname)
  value_type <- ValueType(type, shape = shape)

  if (is.character(func_id)) {
    func_id <- FuncId(func_id)
  }

  func <- Func(
    inputs = FuncInputs(list(FuncInput(id = value_id, type = value_type))),
    id = func_id
  )
  FuncPointer(
    value_id = value_id,
    value_type = value_type,
    func = func
  )
}

