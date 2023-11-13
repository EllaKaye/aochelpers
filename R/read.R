# get the input path given day and year
aoc_input_path <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()
	check_year(year)
	check_day(day)
	here::here(year, "day", day, "input")
}

#' Read input as a vector
#'
#' @inheritParams aoc_url
#' @param mode Character string. One of 'character' or 'numeric'. Default is 'character'.
#'
#' @return A vector containing the puzzle input for the day and year
#' @export
#'
#' @examples \dontrun{aoc_input_vector(1, 2020, "numeric")}
aoc_input_vector <- function(day, year = NULL, mode = c("character", "numeric")) {
	if (is.null(year)) year <- current_year()

	mode <- rlang::arg_match(mode)

	path <- aoc_input_path(day, year)

	input <- readr::read_lines(aoc_input_path(day, year))

	if (mode == "numeric") {
		input <- as.numeric(input)
	}

	input
}

aoc_input_data_frame <- function(day, year = NULL, class = c("tbl_df", "data.frame"),
																 col_names = FALSE, show_col_types = FALSE,
																 view = FALSE) {
	if (is.null(year)) year <- current_year()

	path <- aoc_input_path(day, year)
	class <- rlang::arg_match(class)

	if (class == "tbl_df") {
		input <- readr::read_tsv(path, col_names = col_names, show_col_types = show_col_types)
	} else if (class == "data.frame") {
		input <- utils::read.table(path)
	}

	if (view) {utils::View(input)}

	input
}

aoc_input_matrix <- function(day, year = NULL, mode = c("character", "numeric"),
														 view = FALSE) {
	if (is.null(year)) year <- current_year()

	mode <- rlang::arg_match(mode)

	path <- aoc_input_path(day, year)

	input <- readr::read_lines(aoc_input_path(day, year))

	input <- matrix(unlist(strsplit(input, split = "")), nrow = length(input), byrow = TRUE)

	if (mode == "numeric") {
		input <- apply(input, 2, as.double)
	}

	if (view) {utils::View(input)}

	input
}

