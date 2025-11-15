test_that("validate() works", {
  dat <- read(
    pkg_file("testdata", "transactions.xlsx")
  )
  expect_no_error(validate(dat))
})
