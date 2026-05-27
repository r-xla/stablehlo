# FFT (complex -> complex) repr

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x4xcomplex<f32>>) -> tensor<2x4xcomplex<f32>> {\n%0 = \"stablehlo.fft\" (%x) {\nfft_type = #stablehlo<fft_type FFT>,\nfft_length = array<i64: 4>\n}: (tensor<2x4xcomplex<f32>>) -> (tensor<2x4xcomplex<f32>>)\nreturn %0 : tensor<2x4xcomplex<f32>>\n}\n"

# IFFT (complex -> complex) repr

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x4x8xcomplex<f64>>) -> tensor<3x4x8xcomplex<f64>> {\n%0 = \"stablehlo.fft\" (%x) {\nfft_type = #stablehlo<fft_type IFFT>,\nfft_length = array<i64: 4, 8>\n}: (tensor<3x4x8xcomplex<f64>>) -> (tensor<3x4x8xcomplex<f64>>)\nreturn %0 : tensor<3x4x8xcomplex<f64>>\n}\n"

# RFFT (real -> complex) repr and shape

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x4xf32>) -> tensor<2x3xcomplex<f32>> {\n%0 = \"stablehlo.fft\" (%x) {\nfft_type = #stablehlo<fft_type RFFT>,\nfft_length = array<i64: 4>\n}: (tensor<2x4xf32>) -> (tensor<2x3xcomplex<f32>>)\nreturn %0 : tensor<2x3xcomplex<f32>>\n}\n"

# IRFFT (complex -> real) repr and shape

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xcomplex<f32>>) -> tensor<2x4xf32> {\n%0 = \"stablehlo.fft\" (%x) {\nfft_type = #stablehlo<fft_type IRFFT>,\nfft_length = array<i64: 4>\n}: (tensor<2x3xcomplex<f32>>) -> (tensor<2x4xf32>)\nreturn %0 : tensor<2x4xf32>\n}\n"

# errors

    Code
      infer_types_fft(vt("c64", 4L), "INVALID", cnst(4L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `fft_type` must be one of "FFT", "IFFT", "RFFT", and "IRFFT".
      x Got "INVALID".

---

    Code
      infer_types_fft(vt("f32", 4L), "FFT", cnst(4L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `operand` must have a complex dtype for "FFT".
      x Got f32.

---

    Code
      infer_types_fft(vt("c64", 4L), "RFFT", cnst(4L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `operand` must have a floating-point dtype for "RFFT".
      x Got c64.

---

    Code
      infer_types_fft(vt("f32", 4L), "IRFFT", cnst(4L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `operand` must have a complex dtype for "IRFFT".
      x Got f32.

---

    Code
      infer_types_fft(vt("c64", c(2L, 2L, 2L, 4L)), "FFT", cnst(c(2L, 2L, 2L, 4L),
      "i64", 4L))
    Condition
      Error in `infer_types_fft()`:
      ! `fft_length` must have 1 to 3 elements.
      x Got 4.

---

    Code
      infer_types_fft(vt("c64", 4L), "FFT", cnst(integer(), "i64", 0L))
    Condition
      Error in `infer_types_fft()`:
      ! `fft_length` must have 1 to 3 elements.
      x Got 0.

---

    Code
      infer_types_fft(vt("c64", 4L), "FFT", cnst(c(2L, 4L), "i64", 2L))
    Condition
      Error in `infer_types_fft()`:
      ! `fft_length` length must not exceed rank of `operand`.
      x Got fft_length of length 2 and operand of rank 1.

---

    Code
      infer_types_fft(vt("f32", c(2L, 4L)), "RFFT", cnst(8L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `operand` trailing 1 dimension must equal `fft_length`.
      x Got operand shape (2x4) and fft_length = 8.

---

    Code
      infer_types_fft(vt("c64", c(2L, 4L)), "IRFFT", cnst(8L, "i64", 1L))
    Condition
      Error in `infer_types_fft()`:
      ! `operand` trailing dimensions must match `fft_length`.
      x Got operand shape (2x4) and fft_length = 8; expected last operand dim 5 (= fft_length[1] / 2 + 1).

