# from https://josiahparry.com/posts/2023-11-10-enums-in-r/

#' @include repr.R
#' @importFrom S7 class_character method<- new_object S7_object
NULL

Enum <- new_class(
  "Enum",
  properties = list(
    value = S7::class_any,
    variants = S7::class_any
  ),
  validator = function(self) {
    if (length(self@value) != 1L) {
      "enum value's are length 1"
    } else if (!(self@value %in% self@variants)) {
      "enum value must be one of possible variants"
    }
  },
  abstract = TRUE
)

method(repr, Enum) <- function(x) {
  x@value # nolint
}

new_enum <- function(class, variants) {
  new_class(
    class,
    parent = Enum,
    constructor = function(value) {
      new_object(S7_object(), value = value, variants = variants)
    }
  )
}
