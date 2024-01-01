# write tests for GCD
test_that("GCD works", {
	expect_equal(GCD(12, 18), 6)
	expect_equal(GCD(12, 0), 12)
	expect_equal(GCD(13, 2), 1)
})

# write tests for LCM
test_that("LCM works", {
	expect_equal(LCM(12, 18), 36)
	expect_equal(LCM(2, 6), 6)
	expect_equal(LCM(3, 5), 15)
})
