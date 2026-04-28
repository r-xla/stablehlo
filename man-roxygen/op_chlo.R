#' <% op_name <- paste0("Op", snake_to_camel(mnemonic)) %>
#' <% op <- get(op_name) %>
#' <% f <- get(paste0("hlo_", mnemonic)) %>
#' <% not_func_variables <- if (is.null(get0("not_func_variables"))) character(0) else strsplit(get0("not_func_variables"), ",")[[1]] %>
#' @title <%= gsub("^Op", "", op_name) %> Operator (CHLO)
#' @description
#' This op is from the CHLO dialect, a higher-level companion to stableHLO
#' that is lowered to stableHLO during compilation. See
#' \url{https://openxla.org/stablehlo/generated/chlo#chlo<%= mnemonic %>_chlo<%= mnemonic %>op}
#' for details.
#' @param <%= paste(setdiff(formalArgs(f), not_func_variables), collapse = ",") %> ([`FuncValue`])\cr
#' @return [`FuncValue`]\cr
