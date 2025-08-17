# univariate functions
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
  "is_finite"
)

# create univariates: mind the right directory
for (op in univariates) {
  write_univariate_op(op)
}

# bivariate function where lhs=rhs
bivariates <- c(
  "add",
  "atan2",
  "subtract",
  "divide"
)

# create biivariates: mind the right directory
for (op in bivariates) {
  write_bivariate_op(op)
}
