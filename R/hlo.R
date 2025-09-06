#' @include type_inference.R
NULL

# return_func is special and only used for hlo_return
hlo_fn <- function(op_class, type_inference, return_func = FALSE) {
  # custom_attrs are attributes that are formatted in a special way, see e.g.
  # hlo_dot_general for an example.
  # In principle this can be any type
  # You then need to implement repr for the Op class
  function(values, funcs = NULL, attrs = NULL, custom_attrs = NULL) {
    lapply(values, function(x) {
      if (!inherits(x, FuncVariable)) {
        stop("All arguments must be FuncVariables")
      }
    })
    lapply(funcs, function(x) {
      if (!inherits(x, Func)) {
        stop("All functions must be Func objects")
      }
    })

    attrs <- lapply(attrs, function(x) {
      err <- "All attributes must be FuncVariables with a single constant Op."
      if (!inherits(x, FuncVariable)) {
        stop(err)
      }
      items <- x@func@body@items
      if (length(items) != 1L) {
        stop(err)
      }

      if (!inherits(items[[1L]], OpConstant)) {
        stop(err)
      }
      items[[1]]@inputs@attrs@items[[1]]@value
    })

    op_input_funcs <- OpInputFuncs(
      lapply(funcs, function(x) {
        OpInputFunc(
          inputs = x@inputs,
          body = x@body
        )
      })
    )

    op_input_attrs <- OpInputAttrs(
      lapply(seq_along(attrs), function(i) {
        attr <- attrs[[i]]
        name <- names(attrs)[i]
        OpInputAttr(
          name = name,
          value = attr
        )
      })
    )

    func <- merge_funcs(lapply(values, function(x) x@func))

    inputs <- OpInputs(
      OpInputValues(lapply(values, function(x) OpInputValue(x@value_id))),
      funcs = op_input_funcs,
      attrs = op_input_attrs,
      custom_attrs = custom_attrs %??% list()
    )

    infer_args <- lapply(values, function(x) x@value_type)

    if (length(funcs) > 0L) {
      infer_args <- c(infer_args, funcs)
    }
    if (length(attrs) > 0L) {
      infer_args <- c(infer_args, attrs)
    }
    if (length(custom_attrs) > 0L) {
      infer_args <- c(infer_args, custom_attrs)
    }

    output_types <- rlang::exec(type_inference, !!!infer_args)
    nout <- length(output_types@items)

    output_value_ids <- replicate(nout, ValueId(), simplify = FALSE)
    outputs <- OpOutputs(lapply(output_value_ids, OpOutput))

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
      func@outputs <- FuncOutputs(
        lapply(values, function(x) {
          FuncOutput(type = x@value_type)
        })
      )
      return(func)
    }

    if (nout == 1L) {
      return(
        FuncVariable(
          value_id = output_value_ids[[1L]],
          value_type = output_types@items[[1L]],
          func = func
        )
      )
    }
    lapply(seq_len(nout), function(i) {
      FuncVariable(
        value_id = output_value_ids[[i]],
        value_type = output_types@items[[i]],
        func = func
      )
    })
  }
}

#' @title Create a input to a function
#' @param name (`character(1)`)\cr
#'   The name of the parameter.
#' @param dtype ([`ValueType`])\cr
#'   The element type of the parameter.
#'   Can contain digits, letters and underscores.
#'   If it starts with a digit, it can only contain digits.
#'   Otherwise it must start with a letter.
#' @param shape (`integer()`)\cr
#'   The shape of the parameter.
#'   Use `integer()` for scalars.
#' @param func_id ([`FuncId`] | `character(1)`)\cr
#'   The function id of the parameter.
#' @export
#' @examples
#' x <- hlo_input("x", "f32", shape = c(2, 2))
#' print(x)
#'
#' # You can combine multiple inputs as follows:
#' c(
#'   hlo_input("x", "f32", shape = c(2, 2)),
#'   hlo_input("y", "f32", shape = c(2, 2))
#' )
hlo_input <- function(
  name,
  dtype,
  shape = integer(),
  func_id = FuncId("main")
) {
  assert_valid_name(name)

  value_id <- ValueId(name)
  value_type <- ValueType(dtype, shape = shape)

  if (is.character(func_id)) {
    func_id <- FuncId(func_id)
  }

  func <- Func(
    inputs = FuncInputs(list(FuncInput(id = value_id, type = value_type))),
    id = func_id
  )
  FuncVariable(
    value_id = value_id,
    value_type = value_type,
    func = func
  )
}

#' @title Create a Closure
#' @description
#' Creates a new function without any arguments that captures the provided variables.
#' @param ... ([`FuncVariable`])\cr
#'   The variables to capture.
#' @return (`list()` of [`FuncVariable`])
#' @export
#' @examples
#' x <- hlo_input("x", "f32", shape = c(2, 2))
#' y <- hlo_input("y", "f32", shape = c(2, 2))
#' f <- hlo_closure(x, y)
#' print(f)
hlo_closure <- function(...) {
  vars <- list(...)
  ids <- vapply(vars, function(v) v@value_id@id, character(1))
  if (any(duplicated(ids))) {
    stop(
      "Each variable can only be captured once in hlo_closure (duplicate value_id detected)"
    )
  }
  lapply(vars, function(variable) {
    FuncVariable(
      value_id = variable@value_id,
      value_type = variable@value_type,
      func = Func(
        id = FuncId(""),
        inputs = FuncInputs(list()),
        outputs = FuncOutputs(list()),
        body = FuncBody(list())
      )
    )
  })
}
