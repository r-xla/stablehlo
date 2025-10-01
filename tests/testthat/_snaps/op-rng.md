# rng uniform

    Code
      repr(result@func@body@items[[4]])
    Output
      [1] "%0 = stablehlo.rng %1, %2, %3 { rng_distribution = #stablehlo<rng_distribution UNIFORM> } : (tensor<f32>, tensor<f32>, tensor<2xi64>) -> (tensor<f32>)"

# rng normal

    Code
      repr(result@func@body@items[[4]])
    Output
      [1] "%0 = stablehlo.rng %1, %2, %3 { rng_distribution = #stablehlo<rng_distribution NORMAL> } : (tensor<f32>, tensor<f32>, tensor<2xi64>) -> (tensor<f32>)"

# rng with integer types

    Code
      repr(result@func@body@items[[4]])
    Output
      [1] "%0 = stablehlo.rng %1, %2, %3 { rng_distribution = #stablehlo<rng_distribution UNIFORM> } : (tensor<i32>, tensor<i32>, tensor<2xi64>) -> (tensor<i32>)"

