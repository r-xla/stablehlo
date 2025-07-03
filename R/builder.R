#' @importFrom S7 class_environment
#' @include types.R
NULL

PartialFunc <- R6::R6Class("PartialFunc",
  public = list(
    inputs = NULL,
    outputs = NULL,
    body = NULL,

    initialize = function(inputs, outputs, body) {
      self$inputs <- inputs
      self$outputs <- outputs
      self$body <- body
    }
  )
)

#' @include value_id.R
PartialFuncPointer <- new_class("PartialFuncPointer",
  properties = list(
    value_id = ValueId,
    value_type = ValueType,
    partial_func = class_environment
  )
)

merge_partial_funcs <- function(partial_funcs) {
  partial_funcs = partial_funcs[duplicated(partial_funcs)]
  if (!length(partial_funcs)) {
    stop("Zero partial funcs provided")
  }
  if (length(partial_funcs) == 1L) {
    return(partial_funcs[[1]])
  }

  PartialFunc$new(
    inputs = merge_partial_func_inputs(partial_funcs),
    outputs = merge_partial_func_outputs(partial_funcs),
    body = merge_partial_func_inputs(partial_funcs)
  )
}

merge_partial_func_inputs <- function(partial_funcs) {
  if (length(partial_funcs) == 1L) {
    return(partial_funcs[[1L]]$inputs)
  }
  .merge <- function(x, y) {
    # TODO: This assumes that inputs are unique within x and y, verify this
    if (any(duplicated(c(x, y))) && !identical(x, y)) {
      stop("Cannot merge partial funcs with duplicate inputs")
    }
    unique(c(x, y))
  }
  Reduce(.merge, partial_funcs)
}

merge_partial_func_outputs <- function(partial_funcs) {
  lapply(partial_funcs, function(partial_func) {
    output <- partial_func$outputs
    if (!is.null(output)) {
      stop("Cannot merge partial funcs with outputs for now")
    }
  })
  NULL
}

merge_partial_func_bodies <- function(partial_funcs) {
  # TODO: This assumes that all the variables that are from different input programs
  # are unique. This is the case when they are generated via ValueId(), but there can be
  # collisions if the name is specified. We might want to add a check for this
  bodies = lapply(partial_funcs, function(partial_func) {
    partial_func$body
  })
  do.call(c, unique(bodies))
}

# TODO What about branching and multiple returns?
stablehlo_return <- function(...) {


}

stablehlo_add <- function(x, y) {
  stopifnot(inherits(x, PartialFuncPointer))
  stopifnot(inherits(y, PartialFuncPointer))

  # x and y are a PartialFuncPointer

  partial_func <- merge_partial_funcs(list(x@partial_func, y@partial_func))

  inputs <- OpInputs(
    OpInputValues(list(
      OpInputValue(x@value_id),
      OpInputValue(y@value_id)
    ))
  )

  value_id <- ValueId()
  outputs <- OpOutputs(list(OpOutput(value_id)))

  output_type <- infer_types_add(x@value_type, y@value_type)

  signature <- OpSignature(
    input_types = ValueTypes(list(x@value_type, y@value_type)),
    output_types = ValueTypes(list(output_type))
  )

  op <- Add(
    inputs,
    outputs,
    signature
  )

  partial_func@body <- FuncBody(c(
    partial_func@body@items,
    list(op)
  ))

  PartialFuncPointer(
    value_id = value_id,
    value_type = output_type,
    partial_func = partial_func
  )
}
