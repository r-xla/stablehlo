library(stablehlo)

f <- function() {
  local_func("main")
  x <- hlo_input("x", shape = c(2, 2), dtype = "f32")
  for (i in 1:100) {
    x <- hlo_add(x, x)
  }
  hlo_return(x)
}

bench::mark(f())

fun <- f()
bench::mark(repr(fun))
