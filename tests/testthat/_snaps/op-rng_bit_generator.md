# rng_bit_generator three_fry

    Code
      repr(result[[1]]@func@body@items[[2]])
    Output
      [1] "%0, %1 = stablehlo.rng_bit_generator %2 { rng_algorithm = #stablehlo<rng_algorithm THREE_FRY> } : (tensor<2xui64>) -> (tensor<2xui64>, tensor<ui64>)"

# rng_bit_generator philox

    Code
      repr(result[[1]]@func@body@items[[2]])
    Output
      [1] "%0, %1 = stablehlo.rng_bit_generator %2 { rng_algorithm = #stablehlo<rng_algorithm PHILOX> } : (tensor<3xui64>) -> (tensor<3xui64>, tensor<ui64>)"

# rng_bit_generator default

    Code
      repr(result[[1]]@func@body@items[[2]])
    Output
      [1] "%0, %1 = stablehlo.rng_bit_generator %2 { rng_algorithm = #stablehlo<rng_algorithm DEFAULT> } : (tensor<2xui64>) -> (tensor<2xui64>, tensor<ui64>)"

