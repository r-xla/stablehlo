#' <% op <- get(snake_to_camel(mnemonic)) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' @title StableHLO <%= op@name %>
#' @description
#' See \url{https://openxla.org/stablehlo/spec?hl=en#<%= mnemonic %>} for the semantics.
#' @param <%= paste(formalArgs(f), collapse = ",") %> ([`FuncVariable`])\cr
#' @return [`FuncVariable`]\cr
