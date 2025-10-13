generate_op_wrapper <- function(
  op_name,
  class_name = NULL,
  params = list(operand = NULL),
  attrs = list(),
  type_inference_fn,
  univariate = TRUE,
  export = TRUE
) {
  if (is.null(class_name)) {
    # snake to camel
    class_name <- paste(capitalize(strsplit(op_name, "_")[[1]]), collapse = "")
  }

  # Generate parameter names
  param_names <- names(params)
  attr_names <- names(attrs)
  all_param_names <- c(param_names, attr_names)

  # 0. header
  header <- sprintf("#' @include op.R hlo.R type_inference.R \nNULL \n")

  # 1. Generate the Op class definition
  op_class_line <- sprintf(
    "Op%s <- new_Op(\"Op%s\", \"%s\")\n",
    class_name,
    class_name,
    op_name
  )

  # 2. Generate type inference function
  # depreceated

  # 3. Generate hlo implementation
  hlo_impl_line <- sprintf(
    "hlo_%s_impl <- hlo_fn(Op%s, %s) \n",
    op_name,
    class_name,
    type_inference_fn
    # infer_fn_name
  )

  # 4. Generate roxygen comments for main function
  roxygen_lines <- character(0)
  if (export) {
    roxygen_lines <- c(
      sprintf("#' @templateVar mnemonic %s", op_name),
      "#' @template op",
      "#' @export"
    )
  }

  # 5. Generate the main function
  all_args <- paste(all_param_names, collapse = ", ")

  # Build values list (only if there are parameters)
  if (length(param_names) > 0) {
    param_assignments <- paste(
      sprintf("%s = %s", param_names, param_names),
      collapse = ", "
    )
    values_call <- sprintf("list(%s)", param_assignments)
  } else {
    values_call <- "list()"
  }

  # Build attrs list (only if there are attributes)
  if (length(attr_names) > 0) {
    attr_assignments <- paste(
      sprintf("%s = %s", attr_names, attr_names),
      collapse = ", "
    )
    attrs_call <- sprintf(", attrs = list(%s)", attr_assignments)
  } else {
    attrs_call <- ""
  }

  main_fn_lines <- c(
    roxygen_lines,
    sprintf("hlo_%s <- function(%s) {", op_name, all_args),
    sprintf("  hlo_%s_impl(values = %s%s)", op_name, values_call, attrs_call),
    "}"
  )

  # Combine all parts in correct order
  result <- c(
    header,
    op_class_line,
    hlo_impl_line,
    main_fn_lines,
    ""
  )

  return(result)
}
