OpConstant <- S7::new_class(
  "OpConstant",
  parent = Op,
  constructor = function(value, output = NULL) {
    new_object(
      Op(
        name = OpName(OpMnemonic("constant")),
        inputs = OpInputs(
          values = OpInputValues(list()),
          funcs = OpInputFuncs(),
          attrs = OpInputAttrs(
            list(
              OpInputAttr(
                "value",
                value
              )
            )
          )
        ),
        outputs = output %||% OpOutputs(),
        signature = OpSignature(
          input_types = ValueTypes(list()),
          output_types = ValueTypes(list(value@value@type))
        )
      )
    )
  }
)

op_constant <- function(value, dtype = NULL) {
  const_value <- r_to_constant(value, dtype = dtype)
  OpConstant(const_value)
}

#' @title Create a Constant
#' @description
#' Create either a scalar or tensor constant.
#' Note that strictly speaking, stableHLO 'scalars' are simply tensors with 0 dimensions.
#' @param x (any)\cr
#'   Value from which to create a constant.
#' @param ... (any)\cr
#'   Additional arguments including:
#'   \itemize{
#'     \item \code{dtype} (`character(1)`): String for element type.
#'       Can be one of f64, f32, u8, u16, u32, u64, i8, i16, i32, i64, pred.
#'     \item \code{shape} (`integer()`): Shape of the tensor (for hlo_tensor only).
#'       If not specified, the shape is inferred from the data.
#'   }
#' @name hlo_constant
#' @export
#' @examples
#' hlo_scalar(1L, dtype = "i32")
#' hlo_scalar(1, dtype = "f32")
#' hlo_scalar(TRUE)
#' hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), dtype = "f32")
hlo_scalar <- function(x, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_scalar")
}

#' @rdname hlo_constant
#' @export
hlo_scalar.logical <- function(x, ...) {
  args <- list(...)
  dtype <- args$dtype
  if (length(x) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(x)) {
    stop("Data for constants must not contain NA values.")
  }
  impl_hlo_constant(x, dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_scalar.double <- hlo_scalar.logical

#' @rdname hlo_constant
#' @export
hlo_scalar.integer <- function(x, ...) {
  args <- list(...)
  dtype <- args$dtype
  if (length(x) != 1L) {
    stop("hlo_scalar expects a single value.")
  }
  if (anyNA(x)) {
    stop("Data for constants must not contain NA values.")
  }
  if (!is.null(dtype) && grepl("^ui", dtype) && any(x < 0L)) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(x, dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_tensor <- function(x, ...) {
  # Can't use S7 for now, because there is no array class
  UseMethod("hlo_tensor")
}

#' @rdname hlo_constant
#' @export
hlo_tensor.array <- function(x, ...) {
  args <- list(...)
  dtype <- args$dtype
  if (anyNA(x)) {
    stop("Data for constants must not contain NA values.")
  }
  if (
    is.integer(x) &&
      !is.null(dtype) &&
      grepl(dtype, "ui") &&
      any(x < 0)
  ) {
    stop("Data for unsigned integer must be non-negative")
  }
  impl_hlo_constant(x, dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.integer <- function(x, ...) {
  args <- list(...)
  dtype <- args$dtype
  shape <- args$shape %||% get_dims(x)
  impl_hlo_constant(array(x, dim = shape), dtype = dtype)
}

#' @rdname hlo_constant
#' @export
hlo_tensor.logical <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.double <- hlo_tensor.integer

#' @rdname hlo_constant
#' @export
hlo_tensor.PJRTBuffer <- function(x, ...) {
  impl_hlo_constant(pjrt::as_array(x), dtype = as.character(dtype(x)))
}


impl_hlo_constant <- function(value, dtype) {
  const_value <- r_to_constant(value, dtype = dtype)
  value_id <- ValueId()
  op <- OpConstant(
    const_value,
    OpOutputs(
      items = list(
        OpOutput(
          value_id
        )
      )
    )
  )

  FuncVariable(
    value_id = value_id,
    value_type = ValueType(const_value@value@type),
    func = Func(
      id = FuncId("main"),
      body = FuncBody(
        items = list(
          op
        )
      ),
      outputs = FuncOutputs()
    )
  )
}

infer_types_constant <- function(value) {
  ValueTypes(list(value@value@type))
}

method(repr, OpConstant) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = FALSE),
    ": ",
    repr(x@signature)
  )
}
