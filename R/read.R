# get the input path given day and year
aoc_input_path <- function(day, year = NULL, file = "input", check = TRUE) {
	if (is.null(year)) year <- current_year()
	check_year(year)
	check_day(day)
	path <- here::here(year, "day", day, file)

	if (check) {
		if (!file.exists(path)) {
			cli::cli_abort("No file {.file {file}} found for Day {day} of {year}.")
		}
	}
	path
}

#' Read puzzle input as a vector
#'
#' Read in the puzzle input, or other file, as a vector, one element per line.
#' It assumes that the file is stored in the directory
#' `"./YYYY/day/DD"`, where `YYYY` and `DD` are the values of `year` and `day`.
#' By default, the file name is `"input"`.
#' This file will exist in this location if the post was created using [aoc_new_day()]. The
#' reading is done by [readLines()].
#'
#' @inheritParams aoc_url
#' @param file Character string. The name of a file that exists in the directory
#'   `"./YYYY/day/DD"`, where `YYYY` and `DD` are the values of `year` and
#'   `day`. Defaults to `"input"`.
#' @param mode Character string. One of 'character' or 'numeric'. Default is
#'   'character'.
#'
#' @return A vector containing the puzzle input for the day and year.
#' @export
#' @seealso [aoc_input_data_frame()] [aoc_input_matrix()]
#'
#' @examples \dontrun{aoc_input_vector(1, 2020, "numeric")}
aoc_input_vector <- function(day, year = NULL, file = "input", mode = c("character", "numeric")) {
	if (is.null(year)) year <- current_year()

	mode <- rlang::arg_match(mode)

	path <- aoc_input_path(day, year, file)

	input <- readLines(path)

	if (mode == "numeric") {
		input <- as.numeric(input)
	}

	input
}

#' Read puzzle input as a data frame
#'
#' Read in the puzzle input, or other file, as a data frame, one row per line.
#' It assumes that the file is stored in the directory
#' `"./YYYY/day/DD"`, where `YYYY` and `DD` are the values of `year` and `day`.
#' By default, the file name is `"input"`.
#' This file will exist in this location if the post was created using [aoc_new_day()].
#' class is `tbl_df`, the reading is done by [readr::read_table()]. If the class
#' is `data.frame`, the reading is done by [utils::read.table()].
#' The defaults have been chosen to suit typical Advent of Code puzzle input.
#'
#' @inheritParams aoc_input_vector
#' @param class character. One of 'tbl_df' or 'data.frame'. Default is 'tbl_df'.
#' @param col_names logical. Only relevant if class is `tbl_df`. If `TRUE`, the
#'   first row of the input file will be used as the column names. Default is
#'   `FALSE`
#' @param show_col_types logical. If `TRUE`, the column types will be shown.
#'   Default is `FALSE`
#' @param view logical. If `TRUE`, calls [utils::View()] to view the returned input.
#'   Default is `FALSE`
#'
#' @return A data frame containing the puzzle input for the day and year.
#' @export
#'
#' @seealso [aoc_input_vector()] [aoc_input_matrix()]
#'
#' @examples \dontrun{aoc_input_data_frame(8, 2020)}
aoc_input_data_frame <- function(day, year = NULL, file = "input", class = c("tbl_df", "data.frame"),
																 col_names = FALSE, show_col_types = FALSE,
																 view = FALSE) {
	if (is.null(year)) year <- current_year()

	path <- aoc_input_path(day, year, file)
	class <- rlang::arg_match(class)

	if (class == "tbl_df") {
		input <- readr::read_table(path, col_names = col_names, show_col_types = show_col_types)
	} else if (class == "data.frame") {
		input <- utils::read.table(path)
	}

	if (view) {utils::View(input)}

	input
}

#' Read puzzle input as a matrix
#'
#' Read in the puzzle input, or other file, as a matrix, one row per line, by
#' default one character per column. It assumes that the file is stored in the
#' directory `"./YYYY/day/DD"`, where `YYYY` and `DD` are the values of `year`
#' and `day`. By default, the file name is `"input"`. This file will exist in
#' this location if the post was created using [aoc_new_day()]. The initial
#' reading is done by [readLines()].
#'
#' @inheritParams aoc_input_vector
#' @inheritParams aoc_input_data_frame
#' @param split character. The string to split the input on. Default is `""`,
#'   i.e. one character per column
#'
#' @return A matrix containing the puzzle input for the day and year.
#' @export
#'
#' @seealso [aoc_input_vector()] [aoc_input_data_frame()]
#'
#' @examples \dontrun{aoc_input_matrix(3, 2020)}
aoc_input_matrix <- function(day, year = NULL, file = "input", mode = c("character", "numeric"), split = "",
														 view = FALSE) {
	if (is.null(year)) year <- current_year()

	mode <- rlang::arg_match(mode)

	input <- readLines(aoc_input_path(day, year, file))

	input <- strsplit(input, split) |> do.call(rbind, args = _)

	if (mode == "numeric") {
		input <- apply(input, 2, as.numeric)
	}

	if (view) {utils::View(input)}

	input
}

