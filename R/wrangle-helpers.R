#' Convert vectors to a matrix
#'
#' For the default split of `"`, assumes that each element of `lines` has the same number of characters.
#'
#' @param lines a vector
#' @param split character. The string to split the input on. Default is `""`, i.e. one character per column
#'
#' @return A matrix where the number of rows is the length of `lines` and, for a split on `"`, the number of columns is the same as the number of characters in each element of `lines`.
#' @export
#'
#' @examples lines_to_matrix(c("#.#.", "..#.", "##.."))
lines_to_matrix <- function(lines, split = "") {
	strsplit(lines, split) |> do.call(rbind, args = _)
}
