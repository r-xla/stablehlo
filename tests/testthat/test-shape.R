describe("Shape", {
  it("is the integer vector of axis sizes, with a class attached", {
    shape <- Shape(c(2, 3, 4))
    expect_s3_class(shape, "Shape")
    expect_true(is.integer(shape))
    expect_identical(unclass(shape), c(2L, 3L, 4L))
    expect_identical(shape(shape), c(2L, 3L, 4L))
    expect_integer(shape(shape))
  })

  it("reports the rank as its length and an axis size by position", {
    shape <- Shape(c(2, 3, 4))
    expect_identical(length(shape), 3L)
    expect_identical(naxes(shape), 3L)
    expect_identical(shape[[2L]], 3L)
    expect_identical(shape[2:3], c(3L, 4L))
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

  it("compares by axis sizes", {
    expect_true(Shape(c(2, 3)) == Shape(c(2, 3)))
    expect_false(Shape(c(2, 3)) == Shape(c(2, 3, 1)))
    expect_true(Shape(c(2, 3)) != Shape(c(3, 2)))
    expect_false(Shape(c(2, 3)) != Shape(c(2, 3)))
  })
})
