# TODO: Move into helper package (code is currently in stablehlo and this package)
list_of <- new_class("list_of")

method(`==`, list(list_of, list_of)) <- function(e1, e2) {
  length(e1@items) == length(e2@items) &&
    all(
      sapply(seq_along(e1@items), function(i) {
        e1@items[[i]] == e2@items[[i]]
      })
    )
}

method(`!=`, list(list_of, list_of)) <- function(e1, e2) {
  !(e1 == e2)
}

method(length, list_of) <- function(x) {
  length(x@items)
}

new_list_of <- function(class_name, item_type) {
  new_class(
    class_name,
    parent = list_of,
    properties = list(
      items = new_property(
        S7::class_list,
        validator = function(value) {
          if (!is.list(value)) {
            return("Not a list")
          }
          if (inherits(item_type, "S7_union")) {
            item_types <- item_type$classes
            for (i in seq_along(value)) {
              for (it in item_types) {
                ok <- FALSE
                if (S7::S7_inherits(value[[i]], it)) {
                  ok <- TRUE
                  break
                }
              }
              if (!ok) {
                classnames <- sapply(item_types, \(x) attr(x, "name"))
                return(sprintf(
                  "Expected item to be of type %s. Got %s.",
                  paste0(classnames, collapse = ", "),
                  class(value[[i]])[[1L]]
                ))
              }
            }
          } else {
            for (i in seq_along(value)) {
              if (!S7::S7_inherits(value[[i]], item_type)) {
                return(
                  sprintf(
                    "Expected item to be of type %s. Got %s.",
                    S7::S7_class(value[[i]]),
                    S7::S7_class(item_type)
                  )
                )
              }
            }
          }
        }
      )
    )
  )
}
