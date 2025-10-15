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

