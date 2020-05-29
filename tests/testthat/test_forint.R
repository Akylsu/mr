context("Function forint")
library(mr)

test_that("Test MR functions", {
  expect_equal(forint(42), '42Ft')
  expect_type(get_bitcoin_price(), "double")
  expect_type(convert_currency("USD", "JPY", 10), "list")

  })
