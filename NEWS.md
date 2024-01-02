# aochelpers 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Use `readr::read_table()` instead of `readr::read_delim()` in `aoc_input_data_frame()`
* Add user agent to `aoc_get_input()`
* Add `extract_numbers()` to extract all numbers from a string
* Add `GCD()` and `LCM()` for finding greatest common divisor and lowest common multiple
* Add `lines_to_matrix()` to convert a vector of strings to a matrix
* Add `split_at_blanks()` to create list from input, with one element per group originally separated by blank lines