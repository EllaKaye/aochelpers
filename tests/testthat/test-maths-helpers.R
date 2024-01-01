# write tests for gcd
test_that("gcd works", {
	expect_equal(gcd(12, 18), 6)
	expect_equal(gcd(12, 0), 12)
	expect_equal(gcd(13, 2), 1)
})

# write tests for lcm
test_that("lcm works", {
	expect_equal(lcm(12, 18), 36)
	expect_equal(lcm(2, 6), 6)
	expect_equal(lcm(3, 5), 15)
})
