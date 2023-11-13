# tests for check_year()
test_that("check_year works", {
  expect_equal(check_year(2020), 2020)
  expect_equal(check_year("2020"), "2020")
  expect_error(check_year(2014), class = "invalid_year")
  expect_error(check_year(20200), class = "invalid_year")
})

# tests for check_day()
test_that("check_day works", {
  expect_equal(check_day(1), 1)
  expect_equal(check_day("1"), "1")
  expect_error(check_day("01"), class = "invalid_day")
  expect_error(check_day(0), class = "invalid_day")
  expect_error(check_day(26), class = "invalid_day")
})
