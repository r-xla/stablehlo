source("tools/create_uni.R")
source("tools/create_biv.R")

# univariate functions save to write
univariates <- c(
  "abs",
  "cbrt",
  "ceil",
  "cosine",
  "exponential_minus_one",
  "exponential",
  "floor",
  "tan",
  "tanh",
  "log",
  "log_plus_one",
  "logistic",
  "negate",
  "round_nearest_even",
  "rsqrt",
  "sign",
  "sine",
  "sqrt"
)

# create univariates:
for (op in univariates) {
  write_univariate_op(op)
}

# bivariate function where lhs=rhs
bivariates <- c(
  "add",
  "atan2",
  "subtract",
  "divide",
  "maximum",
  "minimum",
  "multiply",
  "power",
  "remainder"
)

# create biivariates:
for (op in bivariates) {
  write_bivariate_op(op)
}

logicals <- c(
  "and",
  "or",
  "xor"
)

inference_logical <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@elt_type@type, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}

for (op in logicals) {
  write_bivariate_op(op,
                     type_inference_fn = inference_logical)
}



# r_files <- list.files("./tests/testthat/", pattern = "\\.R$", full.names = TRUE)
#
# # Source each file
# for (file in r_files) {
#   source(file)
# }
