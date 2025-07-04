# During build-up of a Func, we don't care about the actual value id.
# What we care about though is uniqueness.
# We still allow to specify the id for the tests, but from a user-level
# perspective, one should just call ValueId()
ValueId <- new_class(
  "ValueId",
  properties = list(
    id = class_character,
    # We store the environment, so the address is unique as long as the object
    # exists
    env = class_environment | NULL
  ),
  constructor = function(id) {
    if (missing(id)) {
      env = new.env()
      new_object(
        ValueId,
        env = env,
        id = paste0("v", rlang::obj_address(env))
      )
    } else {
      new_object(
        ValueId,
        id = id,
        env = NULL
      )
    }
  }
)

method(repr, ValueId) <- function(x) {
  paste0("%", x@id)
}