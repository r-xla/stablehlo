

new_list_of <- function(class_name, item_type) {
  new_class(
    class_name,
    properties = list(
      items = new_property(
        S7::class_list,
        validator = function(value) {
          if (!is.list(value)) {
            return("Not a list")
          }
          for (i in seq_along(value)) {
            if (!S7::S7_inherits(value[[i]], item_type)) {
              return(
                sprintf("Expected item to be of type %s. Got %s.",
                        S7::S7_class(value[[i]]),
                        S7::S7_class(item_type))
              )
            }
          }
        }
      )
    )
  )
}