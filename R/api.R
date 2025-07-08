#' @include type_inference.R
NULL

hlo_fn <- function(op_class, type_inference, return_func = FALSE) {
  function(..., .funcs = NULL, .attrs = NULL) {
    pointers = list(...)
    lapply(pointers, function(x) {
      if (!inherits(x, FuncPointer)) {
        stop("All arguments must be FuncPointers")
      }
    })

    .op_input_funcs <- OpInputFuncs(lapply(.funcs, function(x) {
      OpInputFunc(
        inputs = x@inputs,
        body = x@body
      )
    }))

    func <- merge_funcs(lapply(pointers, function(x) x@func))
    inputs <- OpInputs(
      OpInputValues(lapply(pointers, function(x) OpInputValue(x@value_id))),
      funcs = .op_input_funcs,
      attrs = .attrs %??% OpInputAttrs()
    )

    args <- lapply(pointers, function(x) x@value_type)

    if (!is.null(.funcs)) {
      args <- c(args, list(.funcs = .funcs))
    }
    if (!is.null(.attrs)) {
      args <- c(args, list(.attrs = .attrs))
    }

    output_types <- rlang::exec(type_inference, !!!args)
    nout <- length(output_types@items)

    output_value_ids = replicate(nout, ValueId(), simplify = FALSE)
    outputs = OpOutputs(lapply(output_value_ids, OpOutput))

    signature <- OpSignature(
      input_types = ValueTypes(lapply(pointers, function(x) x@value_type)),
      output_types = output_types
    )

    op <- op_class(
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )

    func@body <- FuncBody(c(func@body@items, list(op)))

    if (return_func) {
      func@outputs <- FuncOutputs(lapply(list(...), function(x) {
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


.hlo_after_all <- hlo_fn(AfterAll, infer_types_after_all)
hlo_after_all <- function(..., .update_pointer = TRUE) {
  out <- .hlo_after_all(...)
  if (.update_pointer) {
    return(out)
  }
  in_pointers <- list(...)
  lapply(seq_along(in_pointers), function(i) {
    FuncPointer(
      value_id = in_pointers[[i]]@value_id,
      value_type = in_pointers[[i]]@value_type,
      func = out@func
    )
  })
}

hlo_atan2 <- hlo_fn(Atan2, infer_types_atan2)
