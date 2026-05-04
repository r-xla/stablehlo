# inputs with different shapes are rejected

    Code
      hlo_scan(inputs = list(a, b), init = z, body = function(carry, ai, bi) list(
        carry, carry), dim = 0L)
    Condition
      Error in `hlo_scan()`:
      ! All `inputs` must share a shape.
      x Input 1 has shape (4), input 2 has shape (5).

# rank-0 input is rejected

    Code
      hlo_scan(inputs = list(a), init = z, body = function(carry, elem) list(carry,
        carry), dim = 0L)
    Condition
      Error in `hlo_scan()`:
      ! `inputs` must have rank >= 1.

# body returning the wrong out_elem shape is rejected

    Code
      hlo_scan(inputs = list(a), init = z, body = function(carry, elem) list(carry, a),
      dim = 0L)
    Condition
      Error in `hlo_scan()`:
      ! `body`'s second return value (out_elem) must have inputs[[1]]'s shape minus `dim` and dtype.
      x Got out_elem: tensor<4xf32>, expected: tensor<f32>.

