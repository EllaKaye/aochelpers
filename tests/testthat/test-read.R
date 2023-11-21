# tests for aoc_input_path
test_that("aoc_input_path works", {
  expect_equal(aoc_input_path(1, 2022, check = FALSE), here::here(2022, "day", 1, "input"))
  expect_equal(aoc_input_path("1", "2022", check = FALSE), here::here(2022, "day", 1, "input"))
  expect_error(aoc_input_path(26, 2022, class = "invalid_day"))
  expect_error(aoc_input_path("01", 2022, class = "invalid_day"))
  expect_error(aoc_input_path(1, 2014, class = "invalid_year"))
})

