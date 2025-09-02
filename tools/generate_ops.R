source("tools/create_uni.R")
source("tools/create_biv.R")
source("tools/generic_inference.R")

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

logic_ops <- c(
  "and",
  "or",
  "xor"
)

for (op in logic_ops) {
  write_bivariate_op(op, type_inference_fn = "infer_types_boolean_biv")
}
