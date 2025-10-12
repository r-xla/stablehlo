#' @importFrom S7 class_environment
#' @include types.R
#' @include func.R
NULL

#' @title FuncVariable
#' @description
#' This represents a variable within a function.
#' @param value_id The name of the variable.
#' @param value_type The type of the variable.
#' @param func The function the variable belongs to.
#' @export
#' @include value_id.R
#' @export
FuncVariable <- new_class(
  "FuncVariable",
  properties = list(
    value_id = ValueId,
    value_type = ValueType,
    func = Func
  )
)

method(print, FuncVariable) <- function(x, ...) {
  str <- repr(x@func)
  cat(sprintf("Variable %s in:\n", cli::col_blue(repr(x@value_id))))
  cat(sub(repr(x@value_id), cli::col_blue(repr(x@value_id)), str, fixed = TRUE))
}

merge_funcs <- function(funcs) {
  if (length(funcs) == 1L) {
    return(funcs[[1L]])
  }
  all_same <- all(sapply(funcs[-1], function(f) {
    identical(f@.env, funcs[[1]]@.env)
  }))
  if (!all_same) {
    cli_abort("All functions must be identical")
  }
  funcs[[1L]]
}

#' @export
#' @method shape stablehlo::FuncVariable
`shape.stablehlo::FuncVariable` <- function(x, ...) {
  shape(x@value_type)
}
