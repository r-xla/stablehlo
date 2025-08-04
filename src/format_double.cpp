#include <Rcpp.h>
#include <iomanip>
#include <sstream>

using namespace Rcpp;

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
    // Convert R dimensions to std::vector
    IntegerVector r_dims(dim_attr);
    dims.insert(dims.end(), r_dims.begin(), r_dims.end());
  }
  int n = x.length();

  // Create output character vector with same shape
  CharacterVector result(n);

  // Set precision for output
  std::ostringstream oss;
  // precision is 8 for 32 bit float and 16 otherwise
  oss << std::scientific << std::setprecision(precision == 32 ? 8 : 16);

  // Format each element
  for (int i = 0; i < n; i++) {
    oss.str(""); // Clear the stream
    oss.clear(); // Clear any error flags

    double value = x[i];

    // Handle special cases using R's built-in functions
    if (R_IsNaN(value)) {
      result[i] = "nan";
    } else if (!R_finite(value)) {
      if (value > 0) {
        result[i] = "inf";
      } else {
        result[i] = "-inf";
      }
    } else {
      oss << value;
      result[i] = oss.str();
    }
  }

  // Preserve the dimensions of the original array
  if (!dims.empty()) {
    result.attr("dim") = wrap(dims);
  }

  return result;
}
