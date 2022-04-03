#' Function to extract first two digits
#'
#' @param number numeric
#'
#' @return character
first_two_digits_ <- function(number){
  stringr::str_sub(number, 1, 2)
}

#' Function to extract first two digits
#'
#' @param number numeric
#'
#' @return character
first_digit_ <- function(number){
  stringr::str_sub(number, 1, 1)
}

#' Convert numeric to scientific notation
#'
#' @param numbers numeric.
#'
#' @return character
#' @export
#'
#' @examples
#' to_scientific(c(12.112,  435, -0.09))
to_scientific <- function(numbers) {
  format(abs(numbers), scientific = TRUE)
}


#' Extract and return leading digits as characters
#'
#' @param numbers numeric
#'
#' @return character
extract_leading_digits <- function(numbers) {

  # convert to scientific numbers to get leading digit
  scientific_numbers <- to_scientific(numbers)
  scientific_numbers <- stringr::str_replace_all(scientific_numbers, " ", "")

  # extract digits
  leading_digits <- stringr::str_remove_all(scientific_numbers, "[[:punct:][:alpha:]]")

  return(leading_digits)
}


#' Extract the first digit from a numeric vector
#'
#' @param numbers numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#'first_digit(c(123, 0.09, -678.9))
first_digit <- function(numbers) {

  numbers <- extract_leading_digits(numbers)
  return(as.numeric(first_digit_(numbers)))

}

#' Extract the first two digits from a numeric vector
#'
#' @param numbers numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#'first_two_digits(c(123, 0.09, -678.9))
first_two_digits <- function(numbers) {

  numbers <- extract_leading_digits(numbers)
  return(as.numeric(first_two_digits_(numbers)))

}
