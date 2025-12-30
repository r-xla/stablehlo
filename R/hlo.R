#' @include type_inference.R
NULL

# return_func is special and only used for hlo_return
hlo_fn <- function(op_class, type_inference, return_func = FALSE) {
  # custom_attrs are attributes that are formatted in a special way, see e.g.
  # hlo_dot_general for an example.
  # In principle this can be any type
  # You then need to implement repr for the Op class
  function(
    values,
    funcs = NULL,
    attrs = NULL,
    custom_attrs = NULL,
    simplify = TRUE
  ) {
    lapply(values, function(x) {
      if (!inherits(x, FuncValue)) {
        cli_abort("All arguments must be FuncValues")
      }
    })
    lapply(funcs, function(x) {
      if (!inherits(x, Func)) {
        cli_abort("All functions must be Func objects")
      }
    })

    # Process attrs - expect a list of OpInputAttr subclasses
    attrs <- attrs %??% list()
    lapply(attrs, function(x) {
      if (!S7::S7_inherits(x, OpInputAttr)) {
        cli_abort(
          "All attributes must be OpInputAttr subclasses (e.g., ConstantAttr, StringAttr, BoolAttr, ScalarAttr)"
        )
      }
    })

    op_input_funcs <- OpInputFuncs(
      lapply(funcs, function(x) {
        OpInputFunc(
          inputs = x@inputs,
          body = x@body
        )
      })
    )

    op_input_attrs <- OpInputAttrs(attrs)

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
    # For type inference, extract values from OpInputAttr subclasses as named args
    if (length(attrs) > 0L) {
      attr_values <- lapply(attrs, function(x) x@value)
      names(attr_values) <- vapply(
        attrs,
        function(x) x@name,
        character(1)
      )
      infer_args <- c(infer_args, attr_values)
    }
    if (length(custom_attrs) > 0L) {
      infer_args <- c(infer_args, custom_attrs)
    }

    output_types <- tryCatch(
      rlang::exec(type_inference, !!!infer_args),
      error = function(e) {
        if (!is.null(e$condition)) {
          throw_error(e$condition, call = sys.call(-5))
        }
        rlang::abort(e$message, call = sys.call(-5))
      }
    )
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

    if (nout == 1L && simplify) {
      return(
        FuncValue(
          value_id = output_value_ids[[1L]],
          value_type = output_types@items[[1L]],
          func = func
        )
      )
    }
    lapply(seq_len(nout), function(i) {
      FuncValue(
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
#' @param func ([`Func`])\cr
#'   The function id of the parameter.
#'   Per default, uses the last function created with [`hlo_func`].
#' @param alias (`integer(1)` or `NULL`)\cr
#'   If integer, marks this input as alias with the given output index (0-based).
#' @export
#' @examples
#' func <- hlo_func()
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
  func = .current_func(),
  alias = NULL
) {
  assert_valid_id(name)
  value_id <- ValueId(name)
  value_type <- ValueType(dtype, shape = shape)
  alias <- assert_int(alias, coerce = TRUE, null.ok = TRUE)

  inp <- FuncInput(
    id = ValueId(name),
    type = ValueType(dtype, shape = shape),
    alias = alias
  )
  func@inputs <- FuncInputs(c(func@inputs@items, inp))

  FuncValue(
    value_id = value_id,
    value_type = value_type,
    func = func
  )
}

#' @title Create a Closure
#' @description
#' Creates a new function without any arguments that captures the provided variables.
#' @param ... ([`FuncValue`])\cr
#'   The variables to capture.
#' @return (`list()` of [`FuncValue`])
#' @export
#' @examples
#' func <- local_func()
#' x <- hlo_input("x", "f32", shape = c(2, 2))
#' y <- hlo_input("y", "f32", shape = c(2, 2))
#' f <- hlo_closure(x, y)
#' print(f)
hlo_closure <- function(...) {
  vars <- list(...)
  ids <- vapply(vars, function(v) v@value_id@id, character(1))
  if (anyDuplicated(ids)) {
    cli_abort(
      "Each variable can only be captured once in hlo_closure (duplicate value_id detected)"
    )
  }
  envir <- parent.frame()
  lapply(vars, function(variable) {
    FuncValue(
      value_id = variable@value_id,
      value_type = variable@value_type,
      func = local_func("", envir)
    )
  })
}
