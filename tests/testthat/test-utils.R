describe("seq_len0", {
  it("counts from 0", {
    expect_identical(seq_len0(3L), 0:2)
  })

  it("returns an empty integer vector for 0", {
    expect_identical(seq_len0(0L), integer())
  })
})
