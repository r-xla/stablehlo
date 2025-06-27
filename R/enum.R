# from https://josiahparry.com/posts/2023-11-10-enums-in-r/

#' @include repr.R
#' @importFrom S7 class_character method<- new_object S7_object
NULL

Enum <- new_class(
  "Enum",
  properties = list(
    Value = S7::class_any,
    Variants = S7::class_any
  ),
  validator = function(self) { 
    if (length(self@Value) != 1L) {
      "enum value's are length 1"
    } else if (!(self@Value %in% self@Variants)) {
      "enum value must be one of possible variants"
    }
  }, 
  abstract = TRUE
)

method(repr, Enum) <- function(x) {
  x@Value
}

new_enum <- function(class, variants) {
  new_class(
    class,
    parent = Enum,
    constructor = function(Value) {
      new_object(S7_object(), Value = Value, Variants = variants)
    }
  )
}

