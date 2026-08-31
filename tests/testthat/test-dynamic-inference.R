N <- NA_integer_

dyn_input <- function(name, dtype, shape) {
  hlo_input(name, dtype, shape = as.integer(shape))
}

# The inferred result type of a one-op program, as its MLIR string.
inferred <- function(build) {
  local_func()
  repr(build()$value_type$type)
}

test_that("elementwise: a dynamic operand meets a static one", {
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", N))
    }),
    "tensor<?xf32>"
  )
  # Refinement: the known side wins, so everything downstream stays static.
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", 3L))
    }),
    "tensor<3xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_multiply(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(2L, N))
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("elementwise: a definite size clash is still an error", {
  local_func()
  expect_error(
    hlo_add(dyn_input("a", "f32", 3L), dyn_input("b", "f32", 4L)),
    "same tensor type"
  )
})

test_that("compare: result is bool over the met shape", {
  expect_equal(
    inferred(function() {
      hlo_compare(
        dyn_input("a", "f32", N),
        dyn_input("b", "f32", 3L),
        comparison_direction = "LT",
        compare_type = "FLOAT"
      )
    }),
    "tensor<3xi1>"
  )
})

test_that("select: meets its operands, scalar pred still exempt", {
  expect_equal(
    inferred(function() {
      hlo_select(
        dyn_input("p", "bool", N),
        dyn_input("t", "f32", N),
        dyn_input("f", "f32", 3L)
      )
    }),
    "tensor<3xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_select(
        hlo_input("p", "bool", shape = integer()),
        dyn_input("t", "f32", N),
        dyn_input("f", "f32", N)
      )
    }),
    "tensor<?xf32>"
  )
})

test_that("reduce: the fold refines and rejects an impossible set", {
  reduce_of <- function(shapes) {
    local_func()
    inputs <- Map(
      function(s, i) dyn_input(paste0("x", i), "f32", s),
      shapes,
      seq_along(shapes)
    )
    inits <- lapply(seq_along(shapes), function(i) {
      hlo_scalar(0, dtype = "f32")
    })
    body <- local_func(id = "")
    args <- lapply(seq_len(2L * length(shapes)), function(i) {
      hlo_input(paste0("b", i), "f32", shape = integer())
    })
    outs <- lapply(seq_along(shapes), function(i) {
      hlo_add(args[[i]], args[[i + length(shapes)]])
    })
    do.call(hlo_return, outs)
    hlo_reduce(inputs, inits, body = body, dimensions = 0L)
  }

  # One dynamic input, one static: the static size wins.
  out <- reduce_of(list(c(N, 3L), c(2L, N)))
  expect_equal(repr(out[[1L]]$value_type$type), "tensor<3xf32>")

  # Every shape may match the first, but the last two cannot match each other.
  # A pairwise check against input 1 would accept this; the fold does not.
  expect_error(reduce_of(list(N, 3L, 4L)), "dimension")
})

test_that("concatenate: off-axis meets, on-axis sums to unknown", {
  expect_equal(
    inferred(function() {
      hlo_concatenate(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(N, 3L)),
        dimension = 0L
      )
    }),
    "tensor<?x3xf32>"
  )
  # A known part does not make the sum known while another part is unknown.
  expect_equal(
    inferred(function() {
      hlo_concatenate(
        dyn_input("a", "f32", c(2L, N)),
        dyn_input("b", "f32", c(N, 4L)),
        dimension = 0L
      )
    }),
    "tensor<?x4xf32>"
  )
  # Off-axis sizes that cannot agree are still refused.
  local_func()
  expect_error(
    hlo_concatenate(
      dyn_input("a", "f32", c(N, 3L)),
      dyn_input("b", "f32", c(N, 4L)),
      dimension = 0L
    )
  )
})

test_that("transpose carries a dynamic axis through the permutation", {
  expect_equal(
    inferred(function() {
      hlo_transpose(dyn_input("a", "f32", c(N, 3L)), permutation = c(1L, 0L))
    }),
    "tensor<3x?xf32>"
  )
})

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

test_that("the inferred dynamic types compile with IREE", {
  skip_if_no_iree_compile()

  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  cat2 <- hlo_concatenate(sel, a, dimension = 0L)
  src <- repr(hlo_return(cat2))

  expect_match(src, "tensor<?xf32>", fixed = TRUE)
  res <- iree_compiles(src)
  expect_true(res$ok, info = res$log)
})
