test_that("['1'] -> '1' ", {
  result <- get_last_valid_command(c("1"))
  expect_equal(result, "1")
})


test_that("['1','2'] -> '2'", {
  result <- get_last_valid_command(c("1", "2"))
  expect_equal(result, "2")
})

test_that("['mtcars %>%', 'head()'] -> 'mtcars %>%\nhead()'", {
  result <- get_last_valid_command(c("mtcars %>%", "head()"))
  expect_equal(result, "mtcars %>%\nhead()")
})


test_that("['ggplot(mtcars, aes(mpg)) +', 'geom_histogram()'], -> 'ggplot(mtcars, aes(mpg)\ngeom_histogram()'", {
  result <- get_last_valid_command(c("mtcars %>%", "ggplot(aes(mpg)) +", "geom_histogram()"))
  expect_equal(result, "mtcars %>%\nggplot(aes(mpg)) +\ngeom_histogram()")
})

test_that("['ggplot(aes(mpg))', 'mtcars %>%', 'ggplot(aes(mpg))'] -> mtcars %>%\nggplot(aes(mpg))", {
  result <- get_last_valid_command(c("ggplot(aes(mpg))","mtcars %>%","ggplot(aes(mpg))"))
  expect_equal(result, "mtcars %>%\nggplot(aes(mpg))")
})
