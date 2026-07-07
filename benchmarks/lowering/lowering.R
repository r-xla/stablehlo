# Lowering micro-benchmarks for the stablehlo builder.
#
# Measures the two hot phases of producing an MLIR program:
#   * build -- constructing the graph (op emission + type inference)
#   * repr  -- rendering the graph to MLIR text
#
# Uses only the public `hlo_*` API so the identical script runs against any
# checkout (e.g. to A/B two branches). Load the package first (via
# `library(stablehlo)` or `devtools::load_all()`), then call
# `bench_lowering()`; it returns a tidy data.frame of median times.

# --- graph builders (public API only) --------------------------------------

# A deep chain of `n` elementwise ops: x <- x op x, alternating add/mul.
build_chain <- function(n) {
  local_func("main")
  x <- hlo_input("x", shape = c(64, 64), dtype = "f32")
  for (i in seq_len(n)) {
    x <- if (i %% 2L == 0L) hlo_add(x, x) else hlo_multiply(x, x)
  }
  hlo_return(x)
}

# A "fan" graph: `n` ops each reading the same two roots, then summed. This
# exercises value reuse (each root id is referenced many times).
build_fan <- function(n) {
  local_func("main")
  a <- hlo_input("a", shape = c(64, 64), dtype = "f32")
  b <- hlo_input("b", shape = c(64, 64), dtype = "f32")
  acc <- hlo_add(a, b)
  for (i in seq_len(n)) {
    acc <- hlo_add(acc, hlo_multiply(a, b))
  }
  hlo_return(acc)
}

BUILDERS <- list(chain = build_chain, fan = build_fan)
SIZES <- c(200L, 1000L, 5000L)

# --- driver -----------------------------------------------------------------

# Returns a data.frame with one row per (graph, size, phase), reporting the
# median time in milliseconds and allocations in MB.
bench_lowering <- function(builders = BUILDERS, sizes = SIZES, iterations = 20L) {
  rows <- list()
  for (bname in names(builders)) {
    builder <- builders[[bname]]
    for (n in sizes) {
      build_res <- bench::mark(
        builder(n),
        iterations = iterations,
        check = FALSE,
        filter_gc = FALSE
      )
      fun <- builder(n)
      repr_res <- bench::mark(
        repr(fun),
        iterations = iterations,
        check = FALSE,
        filter_gc = FALSE
      )
      rows[[length(rows) + 1L]] <- data.frame(
        graph = bname,
        size = n,
        phase = c("build", "repr"),
        median_ms = c(
          as.numeric(build_res$median) * 1000,
          as.numeric(repr_res$median) * 1000
        ),
        mem_mb = c(
          as.numeric(build_res$mem_alloc) / 1024^2,
          as.numeric(repr_res$mem_alloc) / 1024^2
        )
      )
    }
  }
  do.call(rbind, rows)
}

# When sourced interactively, print the table.
if (sys.nframe() == 0L) {
  print(bench_lowering(), row.names = FALSE)
}
