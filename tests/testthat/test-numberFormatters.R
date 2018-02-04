context("test-numberFormatters.R")

test_that("check correct conversions", {
  expect_equal(num_to_word(0), "zero")
  expect_equal(num_to_word(11), '11')
})
