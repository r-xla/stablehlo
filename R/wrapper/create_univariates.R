write_univariate_op <- function(op_name, dir = "R/wrapper/R") {
  op <- generate_op_wrapper(
    op_name = op_name,
    class_name = NULL,
    params = list(operand = NULL),
    univariate = TRUE,
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
