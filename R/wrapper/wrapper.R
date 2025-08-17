#' Generate S7 Op wrapper functions
#'
#' @param op_name Character string for the operation name (e.g., "exponential", "absolute")
#' @param class_name Character string for the class name (defaults to titlecase of op_name)
#' @param params Named list of parameters and their default values (e.g., list(operand = NULL))
#' @param attrs Named list of attributes and their default values (e.g., list(lower = TRUE))
#' @param type_inference_fn Custom type inference function (optional)
#' @param univariate Logical, if TRUE uses standard univariate type inference
#' @param export Logical, whether to add @export tag
#' @return Character vector containing the generated code
generate_op_wrapper <- function(op_name,
                                class_name = tools::toTitleCase(op_name),
                                params = list(operand = NULL),
                                attrs = list(),
                                type_inference_fn = NULL,
                                univariate = TRUE,
                                export = TRUE) {

  # Generate parameter names
  param_names <- names(params)
  attr_names <- names(attrs)
  all_param_names <- c(param_names, attr_names)

  # 0. header
  header <- sprintf("#' @include op.R hlo.R \nNULL \n")

  # 1. Generate the Op class definition
  op_class_line <- sprintf('%s <- new_Op("%s", "%s")\n', class_name, class_name, op_name)

  # 2. Generate type inference function
  infer_fn_name <- paste0("infer_types_", op_name)
  if (is.null(type_inference_fn)) {
    if (univariate && length(param_names) == 1) {
      # Standard univariate type inference
      infer_fn_lines <- c(
        sprintf('infer_types_%s <- function(%s) {', op_name, paste(all_param_names, collapse = ", ")),
        sprintf('  stopifnot(inherits(%s@type, TensorType))', param_names[1]),
        sprintf('  ValueTypes(list(%s))', param_names[1]),
        '}'
      )
    } else if (all(c("lhs", "rhs") %in% param_names)) {
      # Two-parameter type inference
      infer_fn_lines <- c(
        sprintf('infer_types_%s <- function(%s) {', op_name, paste(all_param_names, collapse = ", ")),
        sprintf('  stopifnot(inherits(%s@type, TensorType))', param_names),
        "  stopifnot(lhs@type == rhs@type)",
        sprintf('  ValueTypes(list(%s))', param_names[1]),
        '}'
      )
    } else {
      # Multi-parameter type inference - TODO
      infer_fn_lines <- c(
        sprintf('infer_types_%s <- function(%s) {', op_name, paste(all_param_names, collapse = ", ")),
        sprintf('  stopifnot(inherits(%s@type, TensorType))', param_names),
        sprintf('  ValueTypes(list(%s))', param_names[1]),
        '}'
      )
    }
  } else {
    infer_fn_lines <- type_inference_fn
  }

  # 3. Generate hlo implementation
  hlo_impl_line <- sprintf('hlo_%s_impl <- hlo_fn(%s, %s) \n', op_name, class_name, infer_fn_name)

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
    param_assignments <- paste(sprintf("%s = %s", param_names, param_names), collapse = ", ")
    values_call <- sprintf("list(%s)", param_assignments)
  } else {
    values_call <- "list()"
  }

  # Build attrs list (only if there are attributes)
  if (length(attr_names) > 0) {
    attr_assignments <- paste(sprintf("%s = %s", attr_names, attr_names), collapse = ", ")
    attrs_call <- sprintf(", attrs = list(%s)", attr_assignments)
  } else {
    attrs_call <- ""
  }

  main_fn_lines <- c(
    roxygen_lines,
    sprintf('hlo_%s <- function(%s) {', op_name, all_args),
    sprintf('  hlo_%s_impl(values = %s%s)', op_name, values_call, attrs_call),
    '}'
  )

  # Combine all parts in correct order
  result <- c(
    header,
    op_class_line,
    infer_fn_lines,
    hlo_impl_line,
    main_fn_lines
  )

  return(result)
}
