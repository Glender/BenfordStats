test_that("first digit can be found", {

  # some numbers
  numbers <- c(12.3, 34.9, 988, 0.05)

  result <- first_digit_(numbers)
  expected <- c("1", "3", "9", "0")

  expect_equal(result, expected)

  # test main function
  result2 <- first_digit(numbers)
  expected2 <- c(1, 3, 9, 5)

  expect_equal(result2, expected2)

})
