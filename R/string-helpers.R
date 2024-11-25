# helpers for working with strings in AoC puzzles

#' Extract all numbers from a string
#'
#' @param x character. The string to extract numbers from.
#'
#' @return A numeric vector containing all numbers in the string.
#' @export
#'
#' @examples extract_numbers("abc123def456")
#' @examples extract_numbers("Game 1:")
#' @examples extract_numbers("Cards: 1 3 16 136")
#' @examples extract_numbers("1, -3, 16, -136")
#'
extract_numbers <- function(x) {
  matches <- gregexpr(pattern = "-?\\d+", text = x)
  regmatches(x, matches) |>
    unlist() |>
    as.numeric()
}
