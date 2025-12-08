.PHONY: format format-cpp format-r

format: format-r

format-r:
	@echo "Formatting R sources with air"
	@find R tests/testthat -maxdepth 1 -name '*.R' ! -name 'RcppExports.R' -print0 \
	  | xargs -0 sh -c 'set -- "$$@"; [ "$$#" -gt 0 ] && air format "$$@"' _
