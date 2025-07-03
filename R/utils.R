#' @include constant.R
#' @include types.R
NULL

#' Create a Constant from R value
r_to_constant <- function(value) {

  if (is.numeric(value)) {
    # For numeric values, create a FloatLiteral
    x <- formatC(abs(value), digits = 16, format = "e")

    parts <- strcapture(
      "([0-9]+)\\.([0-9]+)e([+-])([0-9]+)",
      x,
      proto = list(
        integral = character(),
        fractional = character(),
        exponent_sign = character(),
        exponent_digits = character()
      )
    )

    charpart_to_digit <- \(x) {
      x |> strsplit("") |> unlist() |> as.integer() |> lapply(decimalDigit)
    }

    integer_part <- charpart_to_digit(parts$integral) |> IntegerPart()
    fractional_part <- charpart_to_digit(parts$fractional) |> FractionalPart()
    exponent_sign <- SignPart(parts$exponent_sign)
    exponent_digits <- charpart_to_digit(parts$exponent_digits) |> IntegerPart()

    f <- FloatLiteral(
      sign_part = SignPart(if (value >= 0) "+" else "-"),
      integer_part = integer_part,
      fractional_part = fractional_part,
      scientific_part = ScientificPart(
        exponent_sign = exponent_sign,
        exponent_digits = exponent_digits
      )
    )

    element_literal <- ElementLiteral(f)
  }

  shape <- Shape(as.list(dim(value) %||% length(value)))

  element_type <- TensorElementType(type = FloatType("f32"))
  tensor_type <- TensorType(dtype = element_type, shape = shape)
  tensor_literal <- TensorLiteral(literal = element_literal)
  tensor_constant <- TensorConstant(literal = tensor_literal, type = tensor_type)

  Constant(value = tensor_constant)
}
