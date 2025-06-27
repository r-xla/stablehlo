#' @include repr.R
#' @include list_of.R
NULL

DimensionSize <- new_class(
  "DimensionSize",
  properties = list(
    value = S7::new_union(
      S7::class_integer,
      S7::class_character
    )
  ),
  validator = function(self) {
    if (length(self@value) != 1L) {
      return("must be a single value")
    }

    if (is.integer(self@value) && self@value < 0L) {
      return("must be non negative")
    }

    if (is.character(self@value) && self@value != "?") {
      return(sprintf("must be '?', not %s", self@value))
    }
  }
)

method(repr, DimensionSize) <- function(x) {
  as.character(x@value)
}

Shape <- new_list_of(
  "Shape",
  item_type = DimensionSize
)

method(repr, Shape) <- function(x) {
  paste0(
    paste0(
      sapply(x@items, repr),
      "x"
    ),
    collapse = ""
  )
}
