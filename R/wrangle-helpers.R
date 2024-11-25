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

#' Split input into groups
#'
#' When the strings represent numbers, it's better to read them in as a character vector using [aoc_input_vector()], then change mode to numeric as part of call to `split_at_blanks()`.
#' @param lines A character vector
#' @param mode Character string. One of 'character' or 'numeric'. Default is 'character'.
#' @param split character. The string to split the input on. Default is `""`, i.e. split at blank lines
#'
#' @return A list, with one element per group originally separated by blank lines.
#' @export
#'
#' @examples x <- c("123", "1234", "", "56", "567", "", "89")
#' @examples split_at_blanks(x)
#' @examples split_at_blanks(x, "numeric")
#'
split_at_blanks <- function(lines, mode = c("character", "numeric"), split = "") {
  mode <- rlang::arg_match(mode)

  groups <- lines |>
    # the `+1` means groups will be named `1`, `2`, etc
    split(cumsum(lines == split) + 1) |>
    lapply(\(x) x[x != ""])

  if (mode == "numeric") {
    groups <- lapply(groups, as.numeric)
  }

  groups
}
