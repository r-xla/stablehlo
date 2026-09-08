N <- NA_integer_

dyn_input <- function(name, dtype, shape) {
  hlo_input(name, dtype, shape = as.integer(shape))
}

# The inferred result type of a program, as its MLIR string.
inferred <- function(build) {
  local_func()
  out <- build()
  vt <- if (inherits(out, "FuncValue")) out$value_type else out[[1L]]$value_type
  repr(vt$type)
}

# A binary-add region, built in its own scope so that `local_func()`'s
# deferred restore has run by the time the caller emits the enclosing op --
# otherwise the op lands inside the region instead of the parent func.
add_region <- function(dtype = "f32") {
  body <- local_func(id = "")
  l <- hlo_input("l", dtype, shape = integer())
  r <- hlo_input("r", dtype, shape = integer())
  hlo_return(hlo_add(l, r))
  body
}

# A less-than comparator region, scoped like add_region().
lt_region <- function(dtype = "f32") {
  f <- local_func(id = "")
  l <- hlo_input("l", dtype, shape = integer())
  r <- hlo_input("r", dtype, shape = integer())
  hlo_return(hlo_compare(
    l,
    r,
    comparison_direction = "LT",
    compare_type = "FLOAT"
  ))
  f
}

# A stand-in for a Func, for the control-flow inference functions. They only
# ever read `$inputs[[i]]$type` and `$outputs[[i]]$type` (see
# func_output_types()), so building a real Func -- which would mean tracing a
# body just to declare its result type -- buys nothing here.
fake_func <- function(out, inputs = list()) {
  structure(
    list(
      inputs = lapply(inputs, function(t) list(type = t)),
      outputs = lapply(out, function(t) list(type = t))
    ),
    class = "Func"
  )
}

# ---- conformance: does IREE accept and run what we infer? ------------------

skip_if_no_iree_compile <- function() {
  if (!nzchar(Sys.which("iree-compile"))) {
    testthat::skip("iree-compile not found")
  }
}

iree_compiles <- function(src) {
  dir <- tempfile("shlo-dyn-")
  dir.create(dir)
  mlir <- file.path(dir, "m.mlir")
  writeLines(src, mlir)
  out <- suppressWarnings(system2(
    "iree-compile",
    c(
      "--iree-hal-target-device=local",
      "--iree-hal-local-target-device-backends=llvm-cpu",
      "--iree-llvmcpu-link-embedded=false",
      "--iree-input-demote-f64-to-f32=false",
      shQuote(mlir),
      "-o",
      shQuote(file.path(dir, "m.vmfb"))
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  list(ok = is.null(status) || status == 0L, log = paste(out, collapse = "\n"))
}

# Executing a dynamic program needs a backend that compiles one, which XLA does
# not: it rejects a `?` entry point outright. So these tests run only against
# the IREE plugin, pointed at by PJRT_PLUGIN_PATH_CPU. Everything above this
# line checks *inference*; everything below checks that the types inference
# produces survive a real compile and give right answers at run time. The
# distinction matters -- an inferred type can be perfectly correct and the
# backend still return the wrong numbers.
skip_if_no_iree_runtime <- function() {
  skip_if_not_installed("pjrt")
  plugin <- Sys.getenv("PJRT_PLUGIN_PATH_CPU", "")
  if (!nzchar(plugin) || !file.exists(plugin)) {
    testthat::skip("PJRT_PLUGIN_PATH_CPU is not set to a plugin file")
  }
  if (!grepl("iree", basename(plugin), fixed = TRUE)) {
    testthat::skip("PJRT_PLUGIN_PATH_CPU is not the IREE plugin")
  }
  # Creating the client warns once per session for each custom call pjrt tries
  # to register, because the IREE plugin exposes no FFI extension. Expected on
  # this backend, and nothing to do with what these tests assert -- so absorb
  # it here, once, rather than let it surface as a warning on whichever test
  # happens to touch the client first.
  suppressWarnings(pjrt::pjrt_client())
  invisible(NULL)
}

skip_if_no_refine <- function() {
  testthat::skip_if_not_installed("pjrt")
  # `pjrt_refine_shapes()` and the `stablehlo-opt` plumbing behind it are newer
  # than the pjrt this package requires, so check for them rather than assume.
  if (!("stablehlo_opt_available" %in% getNamespaceExports("pjrt"))) {
    testthat::skip("this pjrt has no shape refinement")
  }
  if (!pjrt::stablehlo_opt_available()) {
    testthat::skip("the stablehlo-opt binary is not available")
  }
  if (!pjrt::plugins_downloaded("cpu")) {
    testthat::skip("no PJRT CPU plugin")
  }
}

# The end-to-end check that a dynamic program is a *real* program: build it
# once with dynamic axes, hand the concrete argument types to
# `pjrt_refine_shapes()`, and compile and run what comes back.
#
# This tests something the inferred type string alone cannot. Our inference and
# stablehlo's refinement pass derive the result shape independently, so if they
# disagree -- if we claimed `?` where the refiner proves `6`, or claimed a size
# the refiner contradicts -- the refined program either fails to compile or
# returns the wrong shape. Running it against the statically built equivalent
# closes the loop.
#
# `dtype` is recycled over the arguments, so a program that mixes an f32
# operand with an i32 index takes `c("f32", "i32")`.
#
# `build` takes one input-shape argument (a list of shapes, one per argument of
# `main` -- static arguments included, since the refiner wants a type for every
# operand) and returns the func's output FuncValue, so the same code path
# builds the dynamic and the static program.
expect_refines_and_runs <- function(
  build,
  dyn_shapes,
  runs,
  dtype = "f32",
  tolerance = 1e-6
) {
  dtypes <- rep_len(dtype, length(dyn_shapes))
  local_func(id = "main")
  src_dyn <- repr(hlo_return(build(dyn_shapes)))
  testthat::expect_match(src_dyn, "?", fixed = TRUE)

  for (run in runs) {
    shapes <- run$shapes
    types <- .mapply(
      function(s, dt) repr(TensorType(as_dtype(dt), Shape(as.integer(s)))),
      list(shapes, dtypes),
      NULL
    )
    types <- unlist(types)

    # What the statically built program infers, for comparison with what the
    # refiner derives from the dynamic one.
    local_func(id = "main")
    static_out <- build(shapes)
    static_type <- repr(static_out$value_type$type)
    src_static <- repr(hlo_return(static_out))

    refined <- pjrt::pjrt_refine_shapes(src_dyn, types)
    testthat::expect_match(
      format(refined, n = 200L),
      static_type,
      fixed = TRUE,
      info = paste("refined result type for", paste(types, collapse = ", "))
    )

    buffers <- .mapply(
      function(v, s, dt) {
        pjrt::pjrt_buffer(v, dtype = dt, shape = as.integer(s))
      },
      list(run$args, shapes, dtypes),
      NULL
    )
    out_dyn <- do.call(
      pjrt::pjrt_execute,
      c(list(pjrt::pjrt_compile(refined)), buffers)
    )
    out_static <- do.call(
      pjrt::pjrt_execute,
      c(list(pjrt::pjrt_compile(pjrt::pjrt_program(src_static))), buffers)
    )
    testthat::expect_equal(
      as_array(out_dyn),
      as_array(out_static),
      tolerance = tolerance,
      info = paste("result for", paste(types, collapse = ", "))
    )
  }
  invisible(NULL)
}
