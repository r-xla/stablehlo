#' <% op <- get(paste0("Op", snake_to_camel(mnemonic))) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' @title <%= op@name %> Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#<%= mnemonic %>} for details.
#' @param <%= paste(formalArgs(f), collapse = ",") %> ([`FuncVariable`])\cr
#' @return [`FuncVariable`]\cr
