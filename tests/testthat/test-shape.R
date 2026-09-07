describe("Shape", {
  it("is the integer vector of axis sizes, with a class attached", {
    shape <- Shape(c(2, 3, 4))
    expect_s3_class(shape, "Shape")
    expect_true(is.integer(shape))
    expect_identical(unclass(shape), c(2L, 3L, 4L))
  })

  it("reports the rank as its length and an axis size by position", {
    shape <- Shape(c(2, 3, 4))
    expect_identical(length(shape), 3L)
    expect_identical(shape[[2L]], 3L)
    expect_identical(shape[2:3], c(3L, 4L))
  })

  it("is not a tengen array: naxes() and nelts() do not apply to it", {
    # Both are defined as length(shape(x)) / prod(shape(x)), so removing the
    # shape() method removes them too. On a Shape you write length() and
    # prod(unclass()) -- it already *is* the vector they would have fetched.
    shape <- Shape(c(2, 3, 4))
    expect_error(naxes(shape), "no applicable method")
    expect_error(nelts(shape), "no applicable method")
    expect_identical(length(shape), 3L)
    expect_identical(prod(unclass(shape)), 24)
  })

  it("accepts a dynamic axis size", {
    shape <- Shape(c(1, 2, NA))
    expect_identical(unclass(shape), c(1L, 2L, NA_integer_))
    expect_equal(repr(shape), "1x2x?")
  })

  it("rejects negative axis sizes", {
    expect_error(Shape(c(2, -1)), "must be >= 0")
  })

  it("reprs the empty shape as the empty string", {
    expect_equal(repr(Shape(integer())), "")
    expect_equal(format(Shape(integer())), "()")
  })

  it("refuses to be compared with an operator", {
    # With an unknown axis size in play, "equal" is ambiguous between type
    # identity and run-time satisfiability, so the caller must say which.
    # The methods exist to *refuse*: without them `==` would fall through to
    # the elementwise integer default and return a vector.
    expect_error(Shape(c(2, 3)) == Shape(c(2, 3)), "not defined for a")
    expect_error(Shape(c(2, 3)) != Shape(c(2, 3)), "not defined for a")
    expect_error(Shape(c(2, 3)) == c(2L, 3L), "not defined for a")
    expect_error(c(2L, 3L) == Shape(c(2, 3)), "not defined for a")
  })
})
