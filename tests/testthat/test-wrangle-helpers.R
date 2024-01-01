# write test for lines_to_matrix
test_that("lines_to_matrix works", {
	expect_equal(lines_to_matrix(c("#.#.", "..#.", "##..")),
							 matrix(c("#", ".", "#", ".",
							 				 ".", ".", "#", ".",
							 				 "#", "#", ".", "."
							 				 ), byrow = TRUE, nrow = 3))
})
