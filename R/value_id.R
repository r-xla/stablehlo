ValueId <- new_class(
  "ValueId",
  properties = list(
    id = class_character | class_environment
  ),
  constructor = function(id = NULL) {
    if (!is.null(id)) {
      assert_valid_name(id)
    } else {
      id <- new.env(size = 0L)
    }
    new_object(
      S7::S7_object(),
      id = id
    )
  }
)

method(`==`, list(ValueId, ValueId)) <- function(e1, e2) {
  e1@id == e2@id
}

method(`!=`, list(ValueId, ValueId)) <- function(e1, e2) {
  !(e1 == e2)
}

method(repr, ValueId) <- function(x) {
  name <- if (is.environment(x@id)) {
    repr_env2name(x@id)
  } else {
    x@id
  }
  paste0("%", name)
}

#' @title Convert environment to name
#' @description
#' This function converts an environment to a name.
#' Can only be called within a `repr` call.
#' @param x The environment to convert to a name.
#' @return `character(1)`
#' @export
repr_env2name <- function(x) {
  # fmt: skip
  FUNC_ENV$vars[[x]] %??% {
    str <- as.character(FUNC_ENV$counter)
    FUNC_ENV$counter <- FUNC_ENV$counter + 1L
    FUNC_ENV$vars[[x]] <- str
    str
  }
}
