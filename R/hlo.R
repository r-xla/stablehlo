#' @include type_inference.R
NULL

# return_func is special and only used for hlo_return
# value_list_names specifies which value arguments are lists of FuncValues
# (e.g., c("inputs", "updates") for scatter)
hlo_fn <- function(
  op_class,
  type_inference,
  return_func = FALSE,
  value_list_names = character()
) {
  # custom_attrs are attributes that are formatted in a special way, see e.g.
  # hlo_dot_general for an example.
  # In principle this can be any type.
  # You then need to pass a custom render function to new_Op().
  op_render <- op_class$render %??% render_op_default
  mnemonic <- op_class$mnemonic
  dialect <- op_class$dialect

  # Fill in the id-dependent parts of the ctx (only knowable once ids are
  # numbered at repr time) and render the op line.
  finalize_render <- function(ctx) {
    ctx$outputs_str <- ids_str(ctx$output_ids)
    ctx$values_str <- ids_str(ctx$value_ids)
    ctx$funcs_str <- if (is.null(ctx$funcs)) "" else render_funcs(ctx$funcs)
    op_render(ctx)
  }

  function(
    values,
    funcs = NULL,
    attrs = NULL,
    custom_attrs = NULL,
    simplify = TRUE,
    output_types = NULL
  ) {
    if (length(value_list_names) == 0L) {
      for (x in values) {
        if (!inherits(x, "FuncValue")) {
          cli_abort("All arguments must be FuncValues")
        }
      }
      flat_values <- values
    } else {
      flat_values <- list()
      for (nm in names(values)) {
        v <- values[[nm]]
        if (nm %in% value_list_names) {
          if (!test_list(v, types = "FuncValue")) {
            cli_abort("'{nm}' must be a list of FuncValues")
          }
          flat_values <- c(flat_values, v)
        } else {
          if (!test_class(v, "FuncValue")) {
            cli_abort("'{nm}' must be a FuncValue")
          }
          flat_values <- c(flat_values, list(v))
        }
      }
    }

    for (x in funcs) {
      if (!inherits(x, "Func")) {
        cli_abort("All functions must be Func objects")
      }
    }

    # Process attrs - expect a list of OpInputAttr subclasses
    for (x in attrs) {
      if (!inherits(x, "OpInputAttr")) {
        cli_abort(
          "All attributes must be OpInputAttr subclasses (e.g., ConstantAttr, StringAttr, BoolAttr, ScalarAttr)"
        )
      }
    }

    func <- merge_funcs(lapply(flat_values, function(x) x$func))

    # When the caller already knows the output types (e.g. a lowering that
    # ran type inference at trace time), it can pass them via `output_types`
    # and inference (including its validation of the inputs) is skipped.
    if (is.null(output_types)) {
      # For infer_args, keep structure: extract value_type from each FuncValue
      infer_args <- if (length(value_list_names) == 0L) {
        lapply(values, function(v) v$value_type)
      } else {
        lapply(values, function(v) {
          if (inherits(v, "FuncValue")) {
            v$value_type
          } else {
            # List of FuncValues
            lapply(v, function(x) x$value_type)
          }
        })
      }

      if (length(funcs) > 0L) {
        infer_args <- c(infer_args, funcs)
      }
      # For type inference, extract values from OpInputAttr subclasses as named args
      if (length(attrs) > 0L) {
        attr_values <- lapply(attrs, function(x) x$value)
        names(attr_values) <- vapply(
          attrs,
          function(x) x$name,
          character(1)
        )
        infer_args <- c(infer_args, attr_values)
      }
      if (length(custom_attrs) > 0L) {
        infer_args <- c(infer_args, custom_attrs)
      }

      output_types <- do.call(type_inference, infer_args)
    }
    nout <- length(output_types)

    output_value_ids <- lapply(seq_len(nout), function(i) ValueId())

    in_type_strs <- vapply(
      flat_values,
      function(x) x$value_type$type$str,
      character(1)
    )
    out_type_strs <- vapply(output_types, type_str, character(1))
    custom_attrs <- custom_attrs %??% list()

    # Value ids are numbered at repr time, so store the ValueId objects (and
    # region funcs) and defer the id-dependent strings to finalize_render().
    ctx <- list(
      mnemonic = mnemonic,
      dialect = dialect,
      output_ids = output_value_ids,
      value_ids = lapply(flat_values, function(x) x$value_id),
      funcs = funcs,
      in_type_strs = in_type_strs,
      out_type_strs = out_type_strs,
      sig_str = paste0(
        "(",
        paste0(in_type_strs, collapse = ", "),
        ") -> (",
        paste0(out_type_strs, collapse = ", "),
        ")"
      ),
      attrs = attrs,
      attrs_str = render_attrs(attrs),
      custom_attrs = custom_attrs
    )

    func_emit(func, list(finalize_render, ctx))

    if (return_func) {
      func$outputs <- FuncOutputs(
        lapply(flat_values, function(x) {
          FuncOutput(type = x$value_type)
        })
      )
      return(func)
    }

    if (nout == 1L && simplify) {
      return(
        FuncValue(
          value_id = output_value_ids[[1L]],
          value_type = output_types[[1L]],
          func = func
        )
      )
    }
    lapply(seq_len(nout), function(i) {
      FuncValue(
        value_id = output_value_ids[[i]],
        value_type = output_types[[i]],
        func = func
      )
    })
  }
}

#' @title Create a input to a function
#' @param name (`character(1)`)\cr
#'   The name of the parameter.
#' @param dtype ([`ValueType`])\cr
#'   The data type of the parameter.
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
  func$inputs <- FuncInputs(c(func$inputs, list(inp)))

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
  seen <- utils::hashtab()
  for (v in vars) {
    cell <- v$value_id$id
    if (!is.null(seen[[cell]])) {
      cli_abort(
        "Each variable can only be captured once in hlo_closure (duplicate value_id detected)"
      )
    }
    seen[[cell]] <- TRUE
  }
  envir <- parent.frame()
  lapply(vars, function(variable) {
    FuncValue(
      value_id = variable$value_id,
      value_type = variable$value_type,
      func = local_func("", envir)
    )
  })
}
