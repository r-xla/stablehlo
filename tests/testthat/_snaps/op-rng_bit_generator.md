# errors

    Code
      infer_types_rng_bit_generator(state, "INVALID", dtype = "f32", shape = c(3L, 2L))
    Condition
      Error in `infer_types_rng_bit_generator()`:
      ! `rng_algorithm` must be one of DEFAULT, THREE_FRY, PHILOX
      x Got "INVALID".

---

    Code
      infer_types_rng_bit_generator(vt("ui64", 4L), "THREE_FRY", dtype = "f32",
      shape = c(3L, 2L))
    Condition
      Error in `infer_types_rng_bit_generator()`:
      ! THREE_FRY requires length(initial_state) = 2
      x Got 4.

---

    Code
      infer_types_rng_bit_generator(vt("ui64", 4L), "PHILOX", dtype = "f32", shape = c(
        3L, 2L))
    Condition
      Error in `infer_types_rng_bit_generator()`:
      ! PHILOX requires length(initial_state) to be 2 or 3
      x Got 4.

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%initial_state: tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>) {\n%0, %1 = \"stablehlo.rng_bit_generator\" (%initial_state) {\nrng_algorithm = #stablehlo<rng_algorithm THREE_FRY>\n}: (tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>)\n\"func.return\"(%0, %1): (tensor<2xui64>, tensor<2x2xui64>) -> ()\n}\n"

---

    Code
      repr(f_d)
    Output
      [1] "func.func @main (%initial_state: tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>) {\n%0, %1 = \"stablehlo.rng_bit_generator\" (%initial_state) {\nrng_algorithm = #stablehlo<rng_algorithm DEFAULT>\n}: (tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>)\n\"func.return\"(%0, %1): (tensor<2xui64>, tensor<2x2xui64>) -> ()\n}\n"

---

    Code
      repr(f_p2)
    Output
      [1] "func.func @main (%initial_state: tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>) {\n%0, %1 = \"stablehlo.rng_bit_generator\" (%initial_state) {\nrng_algorithm = #stablehlo<rng_algorithm PHILOX>\n}: (tensor<2xui64>) -> (tensor<2xui64>, tensor<2x2xui64>)\n\"func.return\"(%0, %1): (tensor<2xui64>, tensor<2x2xui64>) -> ()\n}\n"

---

    Code
      repr(f_p3)
    Output
      [1] "func.func @main (%initial_state: tensor<3xui64>) -> (tensor<3xui64>, tensor<2x2xui64>) {\n%0, %1 = \"stablehlo.rng_bit_generator\" (%initial_state) {\nrng_algorithm = #stablehlo<rng_algorithm PHILOX>\n}: (tensor<3xui64>) -> (tensor<3xui64>, tensor<2x2xui64>)\n\"func.return\"(%0, %1): (tensor<3xui64>, tensor<2x2xui64>) -> ()\n}\n"

