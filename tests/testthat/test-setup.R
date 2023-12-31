# write tests for aoc_url
test_that("aoc_url works", {
  expect_equal(aoc_url(1, 2022), "https://adventofcode.com/2022/day/1")
	expect_equal(aoc_url("1", "2022"), "https://adventofcode.com/2022/day/1")
	expect_equal(aoc_url(1L, 2022), "https://adventofcode.com/2022/day/1")
})

# write tests for aoc_url_input
test_that("aoc_url_input works", {
  expect_equal(aoc_url_input(1, 2022), "https://adventofcode.com/2022/day/1/input")
	expect_equal(aoc_url_input("1", "2022"), "https://adventofcode.com/2022/day/1/input")
	expect_equal(aoc_url_input(1L, 2022), "https://adventofcode.com/2022/day/1/input")
})
