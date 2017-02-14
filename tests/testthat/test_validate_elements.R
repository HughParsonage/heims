context("Validate elements")

test_that("validate_elements returns TRUE when valid", {
  # http://heimshelp.education.gov.au/sites/heimshelp/2016_data_requirements/2016dataelements/pages/306
  # says that a value, x, in variable E306 is valid iff x = 0 or x \in (1000, 9999)
  X <- data.frame(E306 = as.integer(c(0, 1011, 9999, 9998, 4500)),
                  E347 = c("0001", "A998", "1987", "1980", "2020"),
                  stringsAsFactors = FALSE)
  expect_true(all(validate_elements(X)))
})

test_that("validate_elements returns FALSE when invalid", {
  X <- data.frame(E306 = as.integer(c(0, 1011, 999, 9998)))
  expect_false(all(validate_elements(X)))
})

test_that("Valid elements for TER return TRUE or FALSE as expected", {
  x <- c(31, 29)
  y <- heims_data_dict$E369$ad_hoc_prepare(x)
  expect_true(heims_data_dict$E369$valid(y[1]))
  expect_false(heims_data_dict$E369$valid(29))
})
