# Internal utility functions used in the package --------------------------

# get current year
current_year <- function() {
  as.character(format(Sys.Date(), "%Y"))
}

check_published <- function(day, year) {
  # time of day and year at EST lubridate
  date <- paste0(year, "-12-", day)
  publish_time <- lubridate::ymd_hms(paste(date, "00:00:00"), tz = "EST")

  if (Sys.time() < publish_time) {
    cli::cli_abort("The puzzle for Day {day}, {year}, hasn't been released yet.")
  }
}

# check if current year is valid
check_year <- function(year) {
  if (!(year %in% 2015:current_year())) {
    cli::cli_abort("{.var year} must be an integer between 2015 and {current_year()}.",
      class = "invalid_year"
    )
  }

  invisible(year)
}

# check if current day is valid
check_day <- function(day) {
  if (!(day %in% 1:25)) {
    cli::cli_abort("{.var day} must be an integer between 1 and 25.",
      class = "invalid_day"
    )
  }

  invisible(day)
}

gsub_YYYY_DD <- function(path, day, year) {
  if (file.exists(path)) {
    file <- readLines(path)
    file_with_year <- gsub("YYYY", year, file)
    file_with_day <- gsub("DD", day, file_with_year)
    writeLines(file_with_day, path)
  } else {
    cli::cli_abort("The path {path} does not exist.")
  }
}
