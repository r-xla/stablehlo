#' @importFrom S7 class_environment
#' @include types.R
#' @include func.R
NULL

#' @include value_id.R
FuncPointer <- new_class("FuncPointer",
  properties = list(
    value_id = ValueId,
    value_type = ValueType,
    func = Func
  )
)

merge_funcs <- function(funcs) {
  funcs = funcs[!duplicated(funcs)]
  if (!length(funcs)) {
    stop("Zero partial funcs provided")
  }
  if (length(funcs) == 1L) {
    return(funcs[[1]])
  }

  Func(
    inputs = merge_func_inputs(funcs),
    outputs = merge_func_outputs(funcs),
    body = merge_func_bodies(funcs),
    id = merge_func_ids(funcs)
  )
}

merge_func_ids <- function(funcs) {
  ids <- unlist(lapply(funcs, function(x) {
    id <- x@id@id
    if (identical(id, "")) {
      return(NULL)
    }
    id
  }))

  uids <- unique(ids)
  if (length(uids) > 1L) {
    stop("Cannot merge partial funcs with different ids")
  } else if (length(uids) == 1L) {
    FuncId(uids)
  } else {
    FuncId()
  }
}

merge_func_inputs <- function(funcs) {
  if (length(funcs) == 1L) {
    return(funcs[[1L]]@inputs)
  }
  .merge <- function(x, y) {
    x <- x@inputs
    y <- y@inputs
    # TODO: This assumes that inputs are unique within x and y, verify this
    if (any(duplicated(c(x, y))) && !identical(x, y)) {
      stop("Cannot merge partial funcs with duplicate inputs")
    }
    FuncInputs(unique(c(x@items, y@items)))
  }
  Reduce(.merge, funcs)
}

merge_func_outputs <- function(funcs) {
  lapply(funcs, function(func) {
    output <- func@outputs
    if (length(output@items)) {
      stop("Cannot merge partial funcs with outputs for now")
    }
  })
  FuncOutputs()
}

merge_func_bodies <- function(funcs) {
  # TODO: This assumes that all the variables that are from different input programs
  # are unique. This is the case when they are generated via ValueId(), but there can be
  # collisions if the name is specified. We might want to add a check for this
  bodies = lapply(unique(funcs), function(func) {
    func@body@items
  })
  # Merge the bodies.
  # Because each individual body is ordered (variables appearing on line n can only access
  # variables from lines <n), we can just merge them and m
  body = Reduce(c, bodies)
  # It is possible, however, that the same line appears in more than one body
  # This can happen if we have one function
  # f1(x): a <- x^2
  # and another function f2(y):
  # If we were to add f1's variable a and f2's variable y, we would get:
  # f12(x, y): a <- x^2; b <- y + a
  # However, we could again add f1's a variable to f12's b variable,
  # If we would not remove duplicates, and just merge the bodies, we would get the 'a <- x^2' line twice.

  # When we remove duplicates when merging two bodies, there is also no issue w.r.t. the order,
  # because we remove the second appearance of the creation of the variable, i.e. it's creation still
  # precedes the usage of the variable.

  body = body[!duplicated(body)]

  FuncBody(body)
}

stablehlo_fn <- function(op_class, type_inference, return_func = FALSE) {
  function(..., .funcs = OpInputFuncs(), .attrs = OpInputAttrs()) {
    pointers = list(...)
    lapply(pointers, function(x) {
      if (!inherits(x, FuncPointer)) {
        stop("All arguments must be FuncPointers")
      }
    })

    func <- merge_funcs(lapply(pointers, function(x) x@func))
    inputs <- OpInputs(
      OpInputValues(lapply(pointers, function(x) OpInputValue(x@value_id))),
      funcs = .funcs,
      attrs = .attrs
    )

    output_types <- rlang::exec(type_inference, !!!lapply(pointers, function(x) x@value_type))
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
      func@outputs <- FuncOutputs(lapply(list(...), function(x) FuncOutput(type = x@value_type)))
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

stablehlo_add <- stablehlo_fn(Add, infer_types_add)
stablehlo_return <- stablehlo_fn(Return, infer_types_return, TRUE)
