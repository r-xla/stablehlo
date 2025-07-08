# During build-up of a Func, we don't care about the actual value id.
# What we care about though is uniqueness.
# We still allow to specify the id for the tests, but from a user-level
# perspective, one should just call ValueId()

.id_gen <- new.env()
.id_gen$i <- 0

next_id <- function() {
  .id_gen$i <- .id_gen$i + 1
  paste0("", .id_gen$i)
}

reset_id_gen <- function() {
  .id_gen$i <- 0
}

local_reset_id_gen <- function(i = 0, local_envir = parent.frame()) {
  old_i <- .id_gen$i
  withr::defer(envir = local_envir, {
    .id_gen$i <- old_i
  })
  .id_gen$i <- i
  invisible(i)
}

ValueId <- new_class(
  "ValueId",
  properties = list(
    id = class_character
  ),
  constructor = function(id) {
    if (missing(id)) {
      id = next_id()
    } else {
      if (startsWith(id, "xxvar")) {
        stop("ValueId cannot start with 'xxvar'")
      }
    }
    new_object(
      ValueId,
      id = id
    )
  }
)

method(repr, ValueId) <- function(x) {
  paste0("%", x@id)
}
