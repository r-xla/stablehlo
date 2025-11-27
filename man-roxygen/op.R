#' <% op <- get(paste0("Op", snake_to_camel(mnemonic))) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' <% if (is.null(get0("not_func_variables"))) not_func_variables <- character(0) %>
#' @title <%= gsub("^Op", "", op@name) %> Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#<%= mnemonic %>} for details.
#' @param <%= paste(setdiff(formalArgs(f), not_func_variables), collapse = ",") %> ([`FuncVariable`])\cr
#' @return [`FuncVariable`]\cr
