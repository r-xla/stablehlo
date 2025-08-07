#' @importFrom S7 class_environment
#' @include types.R
#' @include func.R
NULL

#' @title FuncVariable
#' @description
#' This represents a variable within a function.
#' @param value_id The name of the variable.
#' @param value_type The type of the variable.
#' @param func The function the variable belongs to.
#' @export
#' @include value_id.R
#' @export
FuncVariable <- new_class(
  "FuncVariable",
  properties = list(
    value_id = ValueId,
    value_type = ValueType,
    func = Func
  )
)

merge_funcs <- function(funcs) {
  funcs = funcs[!duplicated(funcs)]
  if (!length(funcs)) {
    stop("Zero partial funcs provided")
  }
  if (length(funcs) == 1L) {
    return(funcs[[1]])
  }

  Func(
    inputs = merge_func_inputs(funcs),
    outputs = merge_func_outputs(funcs),
    body = merge_func_bodies(funcs),
    id = merge_func_ids(funcs)
  )
}

merge_func_ids <- function(funcs) {
  ids <- unlist(
    lapply(funcs, function(x) {
      id <- x@id@id
      if (identical(id, "")) {
        return(NULL)
      }
      id
    })
  )

  uids <- unique(ids)
  if (length(uids) > 1L) {
    stop("Cannot merge partial funcs with different ids")
  } else if (length(uids) == 1L) {
    FuncId(uids)
  } else {
    FuncId()
  }
}

merge_func_inputs <- function(funcs) {
  if (length(funcs) == 1L) {
    return(funcs[[1L]]@inputs)
  }
  inputs <- lapply(funcs, function(func) {
    func@inputs
  })
  inputs <- inputs[!duplicated(inputs)]
  .merge <- function(x, y) {
    if (!any(duplicated(c(x, y)))) {
      return(FuncInputs(c(x@items, y@items)))
    }

    if (anyDuplicated(c(x, y))) {
      # A possible situation might be:
      # f(x, y):
      #   a <- x + y
      #   ---<branch>---
      #   b <- a^2
      # g(x, y):
      #   a <- x + y
      #   ---<branch>---
      #   c <- a * 4

      # This is okay, as long as all variable names that are created after the branch
      # have different names

      small_len <- min(length(x@body@items), length(y@body@items))
      different <- which(
        vapply(
          seq_len(small_len),
          function(i) {
            x@body@items[[i]] != y@body@items[[i]]
          },
          logical(1L)
        )
      )

      if (!length(different)) {
        # all are the same
        return(x)
      }
      first_diff <- different[1L]

      xlines <- x@body@items[seq(first_diff, length(x@body@items))]
      ylines <- y@body@items[seq(first_diff, length(y@body@items))]

      xvars <- unlist(
        lapply(xlines, function(line) {
          sapply(line@outputs@items, function(out) out@id@id)
        })
      )
      yvars <- unlist(
        lapply(ylines, function(line) {
          sapply(line@outputs@items, function(out) out@id@id)
        })
      )

      # This check is conservative.
      # There are programs that could be merged (e.g. permuting rows that does not change a program)
      # that we reject here.
      # But as long as we create functions with our builder API, this won't happen
      if (anyDuplicated(c(xvars, yvars))) {
        stop(
          "The two functions that are being merged define the same variable differently"
        )
      }
    }
    combined <- c(x@items, y@items)
    FuncInputs(combined[!duplicated(combined)])
  }
  Reduce(.merge, inputs)
}

merge_func_outputs <- function(funcs) {
  lapply(funcs, function(func) {
    output <- func@outputs
    if (length(output@items)) {
      # I think that this never throws, because whenever we have a return statement,
      # a function is complete, as there can be no early returns in branches.
      stop("Cannot merge partial funcs with outputs for now")
    }
  })
  FuncOutputs()
}

merge_func_bodies <- function(funcs) {
  # TODO: This assumes that all the variables that are from different input programs
  # are unique. This is the case when they are generated via ValueId(), which is the
  # case with the builder API.
  # Maybe we want a check for this?
  # But I think it's fine to only guarantee valid programs when using the builder API
  bodies = lapply(unique(funcs), function(func) {
    func@body@items
  })
  # When creating functions with the builder API, we guarantee that
  # each individual body is ordered (variables appearing on line <n> can only access
  # variables from lines <n> - 1 and below), we can just merge them and maintain order
  body = Reduce(c, bodies)
  # It is possible, however, that the same line appears in more than one body
  # This can happen if we have one function
  # f1(x): a <- x^2
  # and another function f2(y):
  # If we were to add f1's variable a and f2's variable y, we would get:
  # f12(x, y): a <- x^2; b <- y + a
  # However, we could again add f1's a variable to f12's b variable,
  # If we would not remove duplicates, and just merge the bodies, we would get the 'a <- x^2' line twice.

  # When we remove duplicates when merging two bodies, there is also no issue w.r.t. the order,
  # because we remove the second appearance of the creation of the variable, i.e. it's creation still
  # precedes the usage of the variable and the new function is still valid.

  body = body[!duplicated(body)]
  FuncBody(body)
}

method(c, FuncVariable) <- function(...) {
  variables <- list(...)
  func <- merge_funcs(lapply(variables, function(x) x@func))
  # Return all pointers of the inputs as outputs
  out <- lapply(variables, function(variable) {
    FuncVariable(
      value_id = variable@value_id,
      value_type = variable@value_type,
      func = func
    )
  })
  names(out) <- vapply(variables, function(x) x@value_id@id, character(1L))
  out
}
