# function for greatest common divisor
# applies Euclid's algorithm
#' Greatest Common Divisor (GCD) and Least Common Multiple (LCM)
#'
#' @param x A single integer
#' @param y A single integer
#'
#' @return The greatest common divisor of x and y
#' @export
#'
#' @examples gcd(12, 18)
#' @examples gcd(12, 0)
#' @examples gcd(13, 2)
gcd <- function(x, y) {
	while (y != 0) {
		t <- y
		y <- x %% y
		x <- t
	}
	x
}


#' @rdname gcd
#' @export
#' @examples lcm(12, 18)
#' @examples lcm(2, 6)
#' @examples lcm(3, 5)
lcm <- function(x, y) {
	x * y / gcd(x, y)
}
