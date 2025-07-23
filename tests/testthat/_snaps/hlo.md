# hlo_input works

    Code
      repr(f)
    Output
      [1] "func.func @ (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n\"func.return\"(%x):(tensor<2x2xf32>) -> ()\n}\n"

# hlo_closure works

    Code
      repr(xcap[[1L]]@func)
    Output
      [1] "func.func @ () ->  {\n\n}\n"

---

    Code
      repr(outs[[1L]]@func)
    Output
      [1] "func.func @ () ->  {\n\n}\n"

---

    Code
      repr(outs[[2L]]@func)
    Output
      [1] "func.func @ () ->  {\n\n}\n"

