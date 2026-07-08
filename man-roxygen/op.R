#' <% op_name <- paste0("Op", snake_to_camel(mnemonic)) %>
#' <% op <- get(op_name) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' <% not_func_variables <- if (is.null(get0("not_func_variables"))) character(0) else strsplit(get0("not_func_variables"), ",")[[1]] %>
#' <% has_output_types <- "output_types" %in% formalArgs(f) %>
#' @title <%= gsub("^Op", "", op_name) %> Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#<%= mnemonic %>} for details.
#' @param <%= paste(setdiff(formalArgs(f), c(not_func_variables, "output_types")), collapse = ",") %> ([`FuncValue`])\cr
#' <%= if (has_output_types) "@param output_types (`list()` of [`ValueType`] | `NULL`)\\cr Output types known ahead of time (e.g. from type inference at trace time). When provided, type inference and its input validation are skipped." else "" %>
#' @return [`FuncValue`]\cr
