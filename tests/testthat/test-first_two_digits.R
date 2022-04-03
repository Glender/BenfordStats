test_that("find the first two digits", {

  # some numbers
  numbers <- c(12.3, 34.9, 988, -10)

  result <- first_two_digits_(numbers)
  expected <- c("12", "34", "98", "-1")

  expect_equal(result, expected)

  # test the main function
  result2 <- first_two_digits(numbers)
  expected2 <- c(12, 34, 98, 10)

  expect_equal(result2, expected2)


})
