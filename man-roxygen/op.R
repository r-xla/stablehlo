#' <% op_name <- paste0("Op", snake_to_camel(mnemonic)) %>
#' <% op <- get(op_name) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' <% not_func_variables <- if (is.null(get0("not_func_variables"))) character(0) else strsplit(get0("not_func_variables"), ",")[[1]] %>
#' @title <%= gsub("^Op", "", op_name) %> Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#<%= mnemonic %>} for details.
#' @param <%= paste(setdiff(formalArgs(f), not_func_variables), collapse = ",") %> ([`FuncValue`])\cr
#' @return [`FuncValue`]\cr
