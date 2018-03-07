# test the functions in numberFormatters.R

context("numFormatters correct behaviour")

test_that("correct output", {
  expect_equal(round_dp(0.12345,0), "0")
  expect_equal(round_dp(0.123456,1), "0.1")
  expect_equal(three_dp(0.123456), "0.123")
  expect_equal(three_dp(0.12), "0.120")
  expect_equal(format_p_md(0.1234), "0.123")
  expect_equal(format_p_md(0.1234/1000), "1.234 x 10^-4^")
  expect_equal(format_p_md(0.1234, sci = FALSE), "0.123")
  expect_equal(format_p_md(0.1234/1000, sci = FALSE), "0.000")
})



context("numFormatters supply weird data")

test_that("must be numeric",{
  expect_error(round_dp("a",1), "num and dp must be numeric")
  expect_error(round_dp(1,"a"), "num and dp must be numeric")
})

test_that("non positive integers supplied",{
  expect_error(round_dp(0.123, 1.1), "dp must be a whole number")
  expect_error(round_dp(0.123, -1), "dp must be positive")
})


