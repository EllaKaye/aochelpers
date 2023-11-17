# Functions for working with website structure ----------------------------

#' Get URLs from the Advent of Code website
#'
#' Get the URL from the Advent of Code website for the puzzle and the puzzle
#' input for a given day and year.
#'
#' @param day An integer between 1 and 25
#' @param year An integer representing the year. Defaults to the current year.
#' @param browse logical - should the URL be opened in the browser?
#' @return A character string with the URL
#' @export
#' @examples
#' aoc_url(1, 2022)
aoc_url <- function(day, year = NULL, browse = FALSE) {
	if (is.null(year)) year <- current_year()
	check_day(day)
	check_year(year)
	url <- paste0("https://adventofcode.com/", year, "/day/", day)
	if (browse) utils::browseURL(url)
	url
}

#' @rdname aoc_url
#' @export
#' @examples aoc_input_url(1, 2022)
aoc_input_url <- function(day, year = NULL, browse = FALSE) {
	if (is.null(year)) year <- current_year()
	check_day(day)
	check_year(year)
	url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
	if (browse) utils::browseURL(url)
	url
}

#' Get and save the puzzle input
#'
#' Get the puzzle input from the Advent of Code website for a given day and year
#' and save it to a file. The file will be saved in the current working
#' directory with a relative path `"./YYYY/day/DD/input"`, where `YYYY` is the
#' value of the year parameter and `DD` is value of the day parameter. This path
#' echoes the URL structure of the Advent of Code website.
#'
#' This function assumes that you have an account on the [Advent of Code
#' website](https://adventofcode.com). For this function to work, you must set
#' the `ADVENT_SESSION` environment variable in your `.Renviron` file. For
#' guidance on how to find your session token, see
#' <https://github.com/dgrtwo/adventdrob/tree/main#installation>. Once you have
#' your session token, you can set the environment variable with
#' [usethis::edit_r_environ()]. This function is adapted from
#' <https://github.com/dgrtwo/adventdrob/blob/main/R/input.R>.
#'
#' @inheritParams aoc_url
#' @return Returns, invisibly, a character string with the absolute path to the
#'   input file.
#' @export
#' @examples \dontrun{aoc_get_input(1, 2022)}
aoc_get_input <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()
	check_day(day)
	check_year(year)
	check_published(day, year)

	session <- Sys.getenv("ADVENT_SESSION")
	if (session == "") {
		cli::cli_abort(c("You must set `ADVENT_SESSION` in your {.file ~/.Renviron} file.",
										 "!" = "See {.fun aochelpers::aoc_get_input} for more information."))
	}

	url <- aoc_input_url(day, year)

	year_path <- here::here(year)
	day_path <- here::here(year_path, "day", day)
	input_path <- here::here(day_path, "input")

	# check if there's a directory for the year and create one if not
	if (!dir.exists(year_path)) {
		dir.create(year_path)
	}

	# check if there's a directory for the day and create one if not
	if (!dir.exists(day_path)) {
		dir.create(day_path, recursive = TRUE)
	}

	# get and save the input
	req <- httr::GET(url,
									 httr::set_cookies(session = session),
									 httr::write_disk(input_path, overwrite = TRUE))

	invisible(input_path)
}

#' Set up up a new post
#'
#' Create the necessary directories for a new post, copy in template files and make them relevant to the day and year (see Details).
#'
#' `aoc_new_post()` and
#' `aoc_new_day()` both assume they being called from a project with a directory
#' `_templates`, with a subdirectory `post-template` that contains, at minimum,
#' the file `index.qmd`. They will copy all the files in `post-template` into a
#' directory `"./YYYY/day/DD"` where `YYYY` is the value of the year parameter and
#' `DD` is value of the day parameter (creating these directories if they do not
#' already exist). This path echoes the URL structure of the Advent of Code
#' website. Additionally, they replace any instances of `YYYY` and `DD` in the
#' `index.qmd` and (if present) `script.R` files with the values of the year and day
#' parameters, respectively. In addition, `aoc_new_day()` will also run
#' `aoc_get_input()` to download the puzzle input and save it into the post
#' directory. If you have your Advent of Code session token set in your
#' `.Renviron` file, we recommend using `aoc_new_day()` over `aoc_new_post()`.
#' If you wish to download and save your puzzle input manually, use
#' `aoc_new_post()`.
#'
#' @inheritParams aoc_url
#' @return The path to the new day (invisibly)
#' @export
#' @seealso [aoc_get_input()]
#' @examples \dontrun{aoc_new_post(1, 2022)}
aoc_new_post <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()

	year_path <- here::here(year)
	day_path <- here::here(year_path, "day", day)

	# if year doesn't exist, create it
	if (!dir.exists(year_path)) {
		dir.create(year_path)
	}

	# if day doesn't exist, create it
	if (!dir.exists(day_path)) {
		dir.create(day_path, recursive = TRUE)
	}

	template_path <- here::here("_templates", "post-template")

	if (!dir.exists(template_path)) {
		cli::cli_abort(c("You must have a directory {.file _templates/post-template} in your project.",
										 "!" = "See {.fun aochelpers::aoc_new_post} for more information."))
	}

	file.copy(list.files(template_path, full.names = TRUE),
						day_path,
						recursive = TRUE)

	# get index.qmd from the new post and swap YYYY and DD for the year and day

	if (!file.exists(paste0(day_path, "/index.qmd"))) {
		cli::cli_abort(c("You must have a file {.file index.qmd} in your {.file _templates/post-template} directory.",
										 "!" = "See {.fun aochelpers::aoc_new_post} for more information."))
	}

	index_path <- paste0(day_path, "/index.qmd")
	gsub_YYYY_DD(index_path, day, year)

	# if there's a script.R, read it in and substitute the year and day
	script_path <- paste0(day_path, "/script.R")
	if (file.exists(script_path)) {
		gsub_YYYY_DD(script_path, day, year)
	}

	invisible(day_path)

}

#' Set up up a new post
#'
#' @rdname aoc_new_post
#' @export
#'
#' @examples \dontrun{aoc_new_day(1, 2022)}
aoc_new_day <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()

	day_path <- here::here(year, "day", day)

	if (dir.exists(day_path)) {
		cli::cli_abort("A directory for Day {day} of {year} already exists.")
	}

	aoc_get_input(day, year)
	aoc_new_post(day, year)

	invisible(day_path)
}


# delete a post for a given day and year
aoc_delete_post <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()

	post <- here::here(year, "day", day, "index.qmd")
	unlink(post, recursive = TRUE)
}

# delete the input for a given day and year
aoc_delete_input <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()

	input_path <- here::here(year, "day", day, "input")
	unlink(input_path)
}

# delete post and input for a given day and year
aoc_delete_day <- function(day, year = NULL) {
	if (is.null(year)) year <- current_year()

	day_path <- here::here(year, "day", day)
	unlink(day_path, recursive = TRUE)
}

# aoc_new_day function gets input and creates a new post


# aoc_new_year copies the listing template _YYYY.qmd
# and creates new directories for the posts and input
#' New year
#'
#' @inheritParams aoc_url
#' @param intro Logical. Whether to include an introduction post. See Details.
#'
#' @return Creates a new year
#' @export
#'
#' @examples \dontrun{aoc_new_year(2022)}
aoc_new_year <- function(year = NULL, intro = TRUE) {
	if (is.null(year)) year <- current_year()

	# create new year directory if it doesn't exist
	if (!dir.exists(here::here(year))) {
		dir.create(here::here(year))
	}

	# copy the _YYYY.qmd to year.qmd and replace YYYY with the year
	# doesn't matter what we use as 'day' here, since there's no "DD" in "_YYYY.qmd"
	listing_path <- here::here(paste0(year, ".qmd"))
	template_path <- here::here("_templates", "YYYY.qmd")
	file.copy(template_path, listing_path)
	gsub_YYYY_DD(listing_path, "DD", year)


	intro_template_path <- here::here("_templates", "YYYY-intro")

	if (intro && dir.exists(intro_template_path)) {
		intro_post_path <- here::here(year, "day", paste0(year, "-introduction"))
		dir.create(intro_post_path, recursive = TRUE)
		file.copy(list.files(intro_template_path, full.names = TRUE),
							intro_post_path, recursive = TRUE)
		intro_index_path <- paste0(intro_post_path, "/index.qmd")
		gsub_YYYY_DD(intro_index_path, "DD", year)
	}

	metadata_path <- here::here("_templates", "_metadata.yml")
	if (file.exists(metadata_path)) {
		file.copy(metadata_path, here::here(year, "day", "_metadata.yml"))
	}

	# message reminder to update _quarto.yml
	cli::cli_bullets(c(
		"!" = "Don't forget to update _quarto.yml, to list {year}.qmd in the navbar."))
}

aoc_delete_year <- function(year = NULL) {
	if (is.null(year)) year <- current_year()

	year <- here::here(year)
	unlink(year, recursive = TRUE)
	unlink(paste0(year, ".qmd"))
	unlink(here::here("_freeze", year), recursive = TRUE)

	# message reminder to update _quarto.yml
	cli::cli_bullets(c(
		"!" = "You may need to update _quarto.yml, to remove {year}.qmd from the navbar."))
}

