# tests for extract_numbers
test_that("extract_numbers works", {
	expect_equal(extract_numbers("abc123def456"), c(123, 456))
	expect_equal(extract_numbers("Game 1:"), c(1))
	expect_equal(extract_numbers("Cards: 1 3 16 136"), c(1, 3, 16, 136))
})
