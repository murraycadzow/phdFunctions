context("text converters correct behaviour")

test_that("correct output", {
  expect_equal(stat_to_short("Tajima.D"), "td")
  expect_equal(stat_to_short("td"), "td")
  expect_equal(stat_to_short("a"), "a")
  expect_equal(short_to_latex("td"), "\\gls{td}")
  expect_equal(short_to_latex('a'), 'a')
})
