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
      # Embedded linking (the default) rather than `link_embedded=false`: the
      # latter emits a system ELF, which the runtime can only load if it was
      # built with the system library loader. A stock `iree-run-module` has
      # only the embedded one, and then every `iree_run()` here dies with
      # "HAL device `__device_0` not found or unavailable" -- the module
      # compiles, so `iree_compiles()` still passes and only the tests that
      # execute fail.
      "--iree-input-demote-f64-to-f32=false",
      shQuote(mlir),
      "-o",
      shQuote(file.path(dir, "m.vmfb"))
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  list(
    ok = is.null(status) || status == 0L,
    log = paste(out, collapse = "\n"),
    vmfb = file.path(dir, "m.vmfb")
  )
}

# The two-operand comparator `unique()` needs: keep-flag descending first, then
# value ascending, so the kept values migrate to the front of the sort in the
# order they already had. A multi-operand sort's comparator takes the operands'
# left halves first, then their right halves.
keep_desc_value_asc_region <- function() {
  f <- local_func(id = "")
  kl <- hlo_input("kl", "i32", shape = integer())
  kr <- hlo_input("kr", "i32", shape = integer())
  vl <- hlo_input("vl", "f32", shape = integer())
  vr <- hlo_input("vr", "f32", shape = integer())
  gt <- hlo_compare(
    kl,
    kr,
    comparison_direction = "GT",
    compare_type = "SIGNED"
  )
  eq <- hlo_compare(
    kl,
    kr,
    comparison_direction = "EQ",
    compare_type = "SIGNED"
  )
  lt <- hlo_compare(vl, vr, comparison_direction = "LT", compare_type = "FLOAT")
  hlo_return(hlo_or(gt, hlo_and(eq, lt)))
  f
}

# Compile and *run* a program with IREE's command line tools, returning the
# result as a numeric vector.
#
# This exists for one case the refine-and-run route structurally cannot cover:
# an extent that comes from the data rather than from a shape.
# `pjrt_refine_shapes()` pins the argument types, but nothing in the program
# determines such an extent, so the `?` survives refinement -- and XLA then
# refuses the op outright ("can't be translated to XLA HLO"). A backend that
# compiles dynamic shapes natively is the only way to execute it.
#
# `inputs` are the `NxTxdtype=v` operand strings iree-run-module takes.
iree_run <- function(src, inputs, dtype_size = 4L, what = "double") {
  dir <- tempfile("shlo-run-")
  dir.create(dir)
  mlir <- file.path(dir, "m.mlir")
  vmfb <- file.path(dir, "m.vmfb")
  out_bin <- file.path(dir, "out.bin")
  writeLines(src, mlir)
  comp <- iree_compiles(src)
  if (!comp$ok) {
    testthat::fail(paste("iree-compile failed:", comp$log))
  }
  file.copy(comp$vmfb, vmfb)
  res <- suppressWarnings(system2(
    "iree-run-module",
    c(
      sprintf("--module=%s", shQuote(vmfb)),
      "--function=main",
      # shQuote: system2() pastes its arguments into a shell command without
      # quoting, so an operand written `8xf32=1 2 3` would be split into
      # `--input=8xf32=1` -- a splat of 1 -- plus stray positional arguments,
      # and the program would silently run on the wrong data.
      sprintf("--input=%s", shQuote(inputs)),
      sprintf("--output=@%s", out_bin)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(res, "status")
  if (!is.null(status) && status != 0L) {
    testthat::fail(paste(
      "iree-run-module failed:",
      paste(res, collapse = "\n")
    ))
  }
  n <- file.size(out_bin) / dtype_size
  readBin(out_bin, what, n = n, size = dtype_size, endian = "little")
}

# For the dynamic-op family: build the program once, refine its argument
# types, compile and run, and compare against an expected result.
#
# `expect_refines_and_runs()` cannot serve here. It builds a statically-shaped
# twin and requires the refiner to derive the same result type -- but these ops
# take their extents from *data*, so a static twin still has `?` in its result,
# and the refiner, which constant-folds the size operands, arrives at a better
# answer than inference can. The twin also does not compile on its own. So
# there is no twin: the assertion is that refinement produces a program XLA
# accepts, and that it computes the right thing.
expect_dynamic_op_runs <- function(
  build,
  types,
  args,
  expected,
  refined_type,
  inferred_type,
  tolerance = 1e-6
) {
  local_func(id = "main")
  out <- build()
  # `inferred_type` is what *our* inference makes of the dynamic build, and it
  # has to be asserted explicitly. A `?` anywhere in the module proves
  # nothing: `dyn_input()` always writes one into `main`'s signature, so an op
  # that ignored its `shape` hint entirely -- returning all-`?` -- would still
  # match. And `refined_type` below is read back off `pjrt_refine_shapes()`,
  # which derives result types itself, so it does not pin ours either.
  testthat::expect_equal(repr(out$value_type$type), inferred_type)
  src <- repr(hlo_return(out))

  refined <- pjrt::pjrt_refine_shapes(src, types)
  # `as_array()` below drops `dim`, so the result *shape* needs an assertion
  # of its own -- otherwise an op returning the right numbers in the wrong
  # shape would pass.
  testthat::expect_equal(refined_result_type(refined), refined_type)
  exec <- pjrt::pjrt_compile(refined)
  out <- do.call(pjrt::pjrt_execute, c(list(exec), args))
  testthat::expect_equal(
    as.vector(tengen::as_array(out)),
    expected,
    tolerance = tolerance
  )
}

# The result type of a refined program's `main`, read off its signature.
#
# Not a grep over the module text: the result type usually coincides with an
# argument's or a constant's, so searching for it would pass regardless of what
# `main` actually returns.
refined_result_type <- function(program) {
  # `format()` returns the module as one string with embedded newlines, not a
  # vector of lines.
  lines <- strsplit(paste(format(program, n = 500L), collapse = "\n"), "\n")[[
    1L
  ]]
  line <- grep("func.func @main", lines, fixed = TRUE, value = TRUE)[[1L]]
  trimws(sub("^.*\\)\\s*->\\s*(.*?)\\s*\\{?\\s*$", "\\1", line))
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
  # No `?`-anywhere assertion here: `dyn_input()` puts one in `main`'s
  # signature regardless, so it would hold even for an op that inferred a
  # fully static result. What pins our inference is the static twin below,
  # whose type is derived by the same code and compared against the refiner's.

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
    testthat::expect_equal(
      refined_result_type(refined),
      static_type,
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
