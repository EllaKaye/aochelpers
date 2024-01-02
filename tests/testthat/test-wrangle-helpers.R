# write test for lines_to_matrix
test_that("lines_to_matrix works", {
	expect_equal(lines_to_matrix(c("#.#.", "..#.", "##..")),
							 matrix(c("#", ".", "#", ".",
							 				 ".", ".", "#", ".",
							 				 "#", "#", ".", "."
							 				 ), byrow = TRUE, nrow = 3))
})

# write test for split_at_blanks
test_that("split_at_blanks works", {
	x <- c("123", "1234", "", "56", "567", "", "89")
	expect_equal(split_at_blanks(x), list(`1` = c("123", "1234"), `2` = c("56", "567"), `3` = "89"))
	expect_equal(split_at_blanks(x, "numeric"), list(`1` = c(123, 1234), `2` = c(56, 567), `3` = 89))

})
