#' Calculate the probability of a first digit according to Benford's Law.
#'
#' @param digit An integer ranging from 1-9.
#'
#' @return
#' @export
#'
#' @examples
#' # to get Benford's law expected digit distribution:
#' prob_first_digit(1:9)
prob_first_digit <- function(digit){
  return(log10(1 + (1/digit)))
}

single_prob_second_digit <- function(d2) {
  sum(log10(1 + (1 / (10*(1:9) + d2))))
}

#' Calculate the probabilty of a second digit.
#'
#' Can be used to detect bias in number reporting.
#' For example, when rounding up is occuring, numbers with the
#' second digit as zero will be overrepresented, while
#' higher numbers like (7,8,9) will be underrepresented
#' in the sample of numbers.
#'
#' @param d2 Integer or vector of integers that presents the second digit.
#'
#' @return Double or Numeric vector
#' @export
#'
#' @examples
#' # calculate probabilty of second digits
#' prob_second_digit(0:9)
#'
#' # cumulative probabilty must be equal to one
#' sum(prob_second_digit(0:9))
#'
prob_second_digit <- function(d2) {
  return(sapply(d2, single_prob_second_digit))
}

#' Calculate the probability of the first two digits according to Benford's Law.
#'
#' @param digits First two digits of the number.
#'
#' @return
#' @export
#'
#' @examples
#' # What is the probability of two ones
#' prob_first_two_digits(11)
prob_first_two_digits <- function(digits){
  return(log10(1 + (1/digits)))
}
