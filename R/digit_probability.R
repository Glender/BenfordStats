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
