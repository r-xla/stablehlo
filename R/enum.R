#' @include repr.R
NULL

#' Create a new enum class
#'
#' @param class_name Name of the enum class
#' @param variants Character vector of valid variants
#' @return Constructor function for the enum
#' @keywords internal
new_enum <- function(class_name, variants) {
  # Return a constructor function
  function(value) {
    checkmate::assert_string(value)
    if (!(value %in% variants)) {
      cli_abort("enum value must be one of: {paste(variants, collapse = ', ')}")
    }
    structure(
      list(value = value, variants = variants),
      class = c(class_name, "Enum")
    )
  }
}

#' @export
repr.Enum <- function(x, ...) {
  x$value
}
