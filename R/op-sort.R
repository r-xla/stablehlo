#' @include op.R hlo.R
NULL

OpSort <- new_Op("OpSort", "sort")

infer_types_sort <- function(..., dimension, is_stable, comparator) {
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))
  dim <- dimension@value@data + 1

  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })

  # (C1) 0 < size(inputs).
  if (!length(dots)) {
    cli_abort("provide at least one input")
  }

  # (C3) same(shape(inputs...) + shape(results...)).
  if (
    !all(vapply(input_dims, \(x) identical(input_dims[[1]], x), logical(1)))
  ) {
    cli_abort("each input must have the same shape")
  }

  # (C4) -R <= dimension < R, where R = rank(inputs[0]).
  if (dim > length(input_dims[[1]])) {
    cli_abort("dimension must be present in inputs")
  }

  # (C2) type(inputs...) = type(results...).
  ValueTypes(lapply(
    dots,
    \(x) ValueType(TensorType(dtype = x@type@dtype, shape = Shape(shape(x))))
  ))
}

hlo_sort_impl <- hlo_fn(OpSort, infer_types_sort)

#' @title mnemonic sort
#' @param ... ([`FuncVariable`])\cr
#' @export
hlo_sort <- function(..., dimension, is_stable, comparator) {
  dots <- list(...)
  dim_attr <- hlo_tensor(
    as.integer(dimension),
    dtype = "i64",
    func = Func()
  )
  hlo_sort_impl(
    values = dots,
    funcs = list(comparator = comparator),
    attrs = list(dimension = dim_attr),
    custom_attrs = list(is_stable = as.logical(is_stable))
  )
}

method(repr, OpSort) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    sub(
      "(.*?}.*?)\n}",
      paste0(
        "\\1,\nis_stable = ",
        tolower(as.character(x@inputs@custom_attrs$is_stable)),
        "\n} "
      ),
      repr(x@inputs, simplify_dense = TRUE, simplify_attrs = TRUE)
    ),
    ": ",
    repr(x@signature)
  )
}
