write_bivariate_op <- function(op_name, dir = "R/wrapper/R") {
  op <- generate_op_wrapper(
    op_name = op_name,
    class_name = tools::toTitleCase(op_name),
    params = list(lhs = NULL, rhs = NULL),
    univariate = FALSE,
    export = TRUE
  )

  # throw error if directory doesn't exist
  if (!dir.exists(dir)) {
    stop("directory does not exist!")
  }

  filename <- file.path(dir, paste0("op-", op_name, ".R"))

  cat(paste(op, collapse = "\n"), file = filename)

  message(sprintf("Created: %s", filename))
}
