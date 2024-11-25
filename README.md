
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aochelpers <a href="https://ellakaye.github.io/aochelpers/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

**aochelpers** is a companion package to the Advent of Code website
template:
<https://github.com/EllaKaye/advent-of-code-website-template/>. It
contains functions to create new listings and posts corresponding to
years and days of the Advent of Code challenges. It also contains
functions to download and save puzzle input from the Advent of Code
website, and to read it in to R in a variety of formats. In time, I will
add functions that are useful in solving the puzzles.

The functions are designed to be used in conjunction with the Advent of
Code website template, and so are easier to demonstrate in that context
than here, so see the repo
<https://github.com/EllaKaye/advent-of-code-website-template/> and the
corresponding demo site,
<https://ellakaye.github.io/advent-of-code-website-template/>, for
further documentation and an example.

## Installation

You can install the development version of aochelpers from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("EllaKaye/aochelpers")
```

## Adding listings pages and posts

The main functions in **aochelpers** are `aoc_new_year()` and
`aoc_new_day()`. They copy files from the website’s `"_template"`
directory, and paste them into the appropriate location in the website
directory. Moreover, in the copies, they replace any instances of `YYYY`
and `DD` in the templates with the values of the `year` and `day`
arguments, respectively, both in the directory and file names, and in
the contents of the .qmd files themselves. In the descriptions below,
the `YYYY` and `DD` placeholders are used to indicate where the year and
day values will be inserted.

``` r
library(aochelpers)
# Add a listing page a directory for a new year
aoc_new_year() # current year
aoc_new_year(2022) # specified year

# Add a post for a new day
aoc_new_day(1) # day 1 of current year
aoc_new_day(2, 2022) # day 2 of specified year
```

[`aoc_new_year()`](https://ellakaye.github.io/aochelpers/reference/aoc_new_year.html)
will

- create a new directory for the specified year, at `./YYYY/`.
- create a new listing page for the year, as `./YYYY.qmd`. The listing
  page will be created using the template `./_templates/YYYY.qmd`. The
  listing page picks up posts from the `YYYY/day` directory. (This
  directory structure echoes the structure of the Advent of Code
  website.)
- optionally create an introductory post for the year, as
  `./YYYY/day/YYYY-introduction`, using the template
  `./_templates/YYYY-intro`. The post will be created only if the
  `intro` argument is `TRUE` (the default). Note that, as of Quarto
  v1.4, there needs to be at least one post in the `YYYY/day` directory
  for the website to render without error.
- if `_templates/_metadata.yml` exists, it will be copied to
  `./YYYY/day/_metadata.yml`.

[`aoc_new_day()`](https://ellakaye.github.io/aochelpers/reference/aoc_new_day.html)
will

- create a new directory for the specified day, at `./YYYY/day/DD/`
- copy the contents of `_templates/post-template` into the above
  directory
- download the puzzle input for the day from the Advent of Code website,
  and save it as `./YYYY/day/DD/input`

See the [advent-of-code-website-template
repo](https://github.com/EllaKaye/advent-of-code-website-template) for
further details about the default contents of `_templates`.

There are other functions for creating and deleting directories and
files based on the advent-of-code-website-template. See the
[Reference](https://ellakaye.github.io/aochelpers/reference/index.html)
page for details.

## Reading in puzzle input

**aochelpers** provides a number of functions to read in puzzle input,
once saved from the Advent of Code website. The functions are designed
to be used in conjunction with the Advent of Code website template, so
to see the output of the functions, see the repo
<https://github.com/EllaKaye/advent-of-code-website-template/> and the
corresponding site,
<https://ellakaye.github.io/advent-of-code-website-template/>, for
examples of their output.

``` r
aoc_input_vector(1, 2022, "numeric")
aoc_input_data_frame(2, 2022)
```
