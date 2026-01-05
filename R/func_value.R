#' @include types.R
#' @include func.R
#' @include value_id.R
NULL

#' @title FuncValue
#' @description
#' This represents a variable within a function.
#' @param value_id The name of the variable.
#' @param value_type The type of the variable.
#' @param func The function the variable belongs to.
#' @export
FuncValue <- function(value_id, value_type, func) {
  checkmate::assert_class(value_id, "stablehlo_ValueId")
  checkmate::assert_class(value_type, "stablehlo_ValueType")
  checkmate::assert_class(func, "stablehlo_Func")

  structure(
    list(value_id = value_id, value_type = value_type, func = func),
    class = "stablehlo_FuncValue"
  )
}

#' @export
print.stablehlo_FuncValue <- function(x, ...) {
  local_vars()
  str <- repr(x$func)
  cat(sprintf("Variable %s in:\n", cli::col_blue(repr(x$value_id))))
  cat(sub(repr(x$value_id), cli::col_blue(repr(x$value_id)), str, fixed = TRUE))
}

merge_funcs <- function(funcs) {
  if (length(funcs) == 1L) {
    return(funcs[[1L]])
  }
  all_same <- all(vapply(
    funcs[-1],
    function(f) {
      identical(f$.env, funcs[[1]]$.env)
    },
    logical(1)
  ))
  if (!all_same) {
    cli_abort("All functions must be identical")
  }
  funcs[[1L]]
}

#' @export
#' @method shape stablehlo_FuncValue
shape.stablehlo_FuncValue <- function(x, ...) {
  shape(x$value_type)
}

#' @export
c.stablehlo_FuncValue <- function(...) {
  list(...)
}
