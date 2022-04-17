#' Second-Order Transformation to create Benford Set
#'
#' From some data distributions it is expected that they don't follow
#' Benford's Law. In such cases it is annying that you can't use Benford's
#' Law to evaluate the data. However, after ordering and taking the differences
#' between these numbers and multiplying with a constant, the transformed data should conform to Benford's Law.
#'
#' This transformation can be applied to Normal, Uniform, Gamma and Triangular Distributions.
#'
#' @param numbers Numeric Vector
#' @param const_multiplier Integer. Constant used to multiply the sorted differences.
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' # generate some numbers
#' numbers <- rnorm(1000)
#'
#' # assure data complies to a Benford Set
#' tranformed_numbers <- second_order_transf(numbers)
#'
#' # Now the data must conform to Benford's Law
#' table(first_digit(tranformed_numbers))
second_order_transf <- function(numbers, const_multiplier = 10^6) {
  return(diff(sort(numbers, decreasing = FALSE))*const_multiplier)
}
