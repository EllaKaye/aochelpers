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
#' @examples GCD(12, 18)
#' @examples GCD(12, 0)
#' @examples GCD(13, 2)
GCD <- function(x, y) {
	while (y != 0) {
		t <- y
		y <- x %% y
		x <- t
	}
	x
}


#' @rdname GCD
#' @export
#' @examples LCM(12, 18)
#' @examples LCM(2, 6)
#' @examples LCM(3, 5)
LCM <- function(x, y) {
	x * y / GCD(x, y)
}
