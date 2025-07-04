ValueId <- new_class(
  "ValueId",
  properties = list(
    id = S7::class_character
  )
)

method(repr, ValueId) <- function(x) {
  paste0("%", x@id)
}