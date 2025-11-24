#include <Rcpp.h>
#include <cstring>
#include <iomanip>
#include <sstream>

using namespace Rcpp;

// Helper for formatting double values
std::string format_float_value(double value, int precision) {
  if (R_IsNaN(value)) {
    return "0x7FC00000";
  } else if (!R_finite(value)) {
    return (value > 0) ? "0x7F800000" : "0xFF800000";
  }
  std::ostringstream oss;
  oss << std::scientific << std::setprecision(precision == 32 ? 8 : 16);
  oss << value;
  return oss.str();
}

// [[Rcpp::export]]
CharacterVector format_double_cpp(NumericVector x, int precision) {
  // Validate precision parameter
  if (precision != 32 && precision != 64) {
    stop("precision must be either 32 or 64");
  }

  // Get the dimensions of the input array
  std::vector<int32_t> dims;
  SEXP dim_attr = Rf_getAttrib(x, Rf_install("dim"));
  if (dim_attr != R_NilValue) {
    IntegerVector r_dims(dim_attr);
    dims.insert(dims.end(), r_dims.begin(), r_dims.end());
  }
  int n = x.length();

  CharacterVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = format_float_value(x[i], precision);
  }

  // Preserve the dimensions of the original array
  if (!dims.empty()) {
    result.attr("dim") = wrap(dims);
  }

  return result;
}

// Helper to format a single element based on dtype
// Assuming data pointer is correctly offset
std::string format_element(const unsigned char *ptr, std::string dtype) {
  std::ostringstream oss;
  if (dtype == "f32") {
    float val;
    std::memcpy(&val, ptr, 4);
    return format_float_value((double)val, 32);
  } else if (dtype == "f64") {
    double val;
    std::memcpy(&val, ptr, 8);
    return format_float_value(val, 64);
  } else if (dtype == "i64") {
    int64_t val;
    std::memcpy(&val, ptr, 8);
    oss << val;
  } else if (dtype == "ui64") {
    uint64_t val;
    std::memcpy(&val, ptr, 8);
    oss << val;
  } else if (dtype == "i32") {
    int32_t val;
    std::memcpy(&val, ptr, 4);
    oss << val;
  } else if (dtype == "ui32") {
    uint32_t val;
    std::memcpy(&val, ptr, 4);
    oss << val;
  } else if (dtype == "i16") {
    int16_t val;
    std::memcpy(&val, ptr, 2);
    oss << val;
  } else if (dtype == "ui16") {
    uint16_t val;
    std::memcpy(&val, ptr, 2);
    oss << val;
  } else if (dtype == "i8") {
    //
    oss << (int)(*reinterpret_cast<const int8_t *>(ptr));
  } else if (dtype == "ui8") {
    oss << (unsigned int)(*ptr);
  } else if (dtype == "pred" || dtype == "i1") {
    return (*ptr) ? "true" : "false";
  } else {
    stop("Unsupported dtype: " + dtype);
  }
  return oss.str();
}

int get_element_size(std::string dtype) {
  if (dtype == "f64" || dtype == "i64" || dtype == "ui64")
    return 8;
  if (dtype == "f32" || dtype == "i32" || dtype == "ui32")
    return 4;
  if (dtype == "i16" || dtype == "ui16")
    return 2;
  return 1;
}

// Recursive function to print nested arrays ([[1, 2], [3, 4]])
// The inmput is a raw vector so we can also properly format PJRTBuffers with >
// 32 bit integers
void print_recursive(std::ostringstream &out, const unsigned char *data,
                     std::string dtype, const IntegerVector &shape,
                     const std::vector<int64_t> &strides, int element_size,
                     int dim_index, int64_t current_offset) {

  // scalar case
  if (dim_index == shape.length()) {
    // Scalar case (should not be reached via recursion if shape > 0)
    // Actually, for rank-0 tensor (scalar), shape.length() is 0.
    // In that case we just print the value.
    out << format_element(data + current_offset * element_size, dtype);
    return;
  }

  if (dim_index == shape.length() - 1) {
    // Last dimension - print list of elements
    out << "[";
    int n = shape[dim_index];
    int64_t stride = strides[dim_index];
    for (int i = 0; i < n; ++i) {
      out << format_element(data + (current_offset + i * stride) * element_size,
                            dtype);
      if (i < n - 1)
        out << ", ";
    }
    out << "]";
  } else {
    // Intermediate dimension - print list of sub-arrays
    out << "[";
    int n = shape[dim_index];
    int64_t stride = strides[dim_index];
    for (int i = 0; i < n; ++i) {
      print_recursive(out, data, dtype, shape, strides, element_size,
                      dim_index + 1, current_offset + i * stride);
      if (i < n - 1)
        out << ", ";
    }
    out << "]";
  }
}

// [[Rcpp::export]]
String format_raw_buffer_cpp(RawVector data, std::string dtype,
                             IntegerVector shape) {
  std::ostringstream out;
  int element_size = get_element_size(dtype);

  int rank = shape.length();

  // check that length of data is is what we expect
  int expected_length = 1;
  for (int i = 0; i < rank; ++i) {
    expected_length *= shape[i];
  }
  expected_length *= element_size;
  if (data.length() != expected_length) {
    stop("Data size mismatch");
  }

  if (rank == 0) {
    out << format_element(data.begin(), dtype);
  } else {
    // special handling of 0-dimensional tensors (e.g., dense<> instead of
    // dense<[[], []]>)
    for (int i = 0; i < rank; ++i) {
      if (shape[i] == 0) {
        return String("");
      }
    }

    // Compute strides for iterating over array:
    std::vector<int64_t> strides(rank);
    // row_major: last dimension has stride 1; all other dimensions have stride
    // of the product of the dimensions to the right.
    strides[rank - 1] = 1;
    for (int i = rank - 2; i >= 0; --i) {
      strides[i] = strides[i + 1] * shape[i + 1];
    }

    print_recursive(out, data.begin(), dtype, shape, strides, element_size, 0,
                    0);
  }

  return String(out.str());
}
