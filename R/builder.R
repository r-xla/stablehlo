#' @importFrom S7 class_environment
#' @include types.R
NULL

PartialFunc <- R6::R6Class("PartialFunc",
  public = list(
    inputs = NULL,
    outputs = NULL,
    body = NULL,
    id = NULL,

    initialize = function(inputs, outputs = NULL, body = FuncBody(), id = NULL) {
      self$inputs <- inputs
      self$outputs <- outputs
      self$body <- body
      self$id <- id
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
  partial_funcs = partial_funcs[!duplicated(partial_funcs)]
  if (!length(partial_funcs)) {
    stop("Zero partial funcs provided")
  }
  if (length(partial_funcs) == 1L) {
    return(partial_funcs[[1]])
  }

  PartialFunc$new(
    inputs = merge_partial_func_inputs(partial_funcs),
    outputs = merge_partial_func_outputs(partial_funcs),
    body = merge_partial_func_bodies(partial_funcs),
    id = merge_partial_func_ids(partial_funcs)
  )
}

merge_partial_func_ids <- function(partial_funcs) {
  ids = unlist(lapply(partial_funcs, function(x) x$id))
  if (any(duplicated(ids))) {
    stop("Cannot merge partial funcs with duplicate ids")
  }
  unique(ids)
}

merge_partial_func_inputs <- function(partial_funcs) {
  if (length(partial_funcs) == 1L) {
    return(partial_funcs[[1L]]$inputs)
  }
  .merge <- function(x, y) {
    x <- x$inputs
    y <- y$inputs
    # TODO: This assumes that inputs are unique within x and y, verify this
    if (any(duplicated(c(x, y))) && !identical(x, y)) {
      stop("Cannot merge partial funcs with duplicate inputs")
    }
    FuncInputs(unique(c(x@items, y@items)))
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
  bodies = lapply(unique(partial_funcs), function(partial_func) {
    partial_func$body@items
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
      if (!inherits(x, PartialFuncPointer)) {
        stop("All arguments must be PartialFuncPointers")
      }
    })

    partial_func <- merge_partial_funcs(lapply(pointers, function(x) x@partial_func))
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

    partial_func$body <- FuncBody(c(partial_func$body@items, list(op)))

    if (return_func) {
      f <- Func(
        inputs = partial_func$inputs,
        outputs = FuncOutputs(lapply(list(...), function(x) FuncOutput(type = x@value_type))),
        body = partial_func$body,
        id = FuncId(partial_func$id)
      )
      return(f)
    }

    if (nout == 1L) {
      return(PartialFuncPointer(
        value_id = output_value_ids[[1L]],
        value_type = output_types@items[[1L]],
        partial_func = partial_func
      ))
    }
    lapply(seq_len(nout), function(i) {
      PartialFuncPointer(
        value_id = output_value_ids[[i]],
        value_type = output_types@items[[i]],
        partial_func = partial_func
      )
    })
  }
}

stablehlo_add <- stablehlo_fn(Add, infer_types_add)
stablehlo_return <- stablehlo_fn(Return, infer_types_return, TRUE)
