context("Prepare sensory experiment")

design <- prepare(n_panelist = 20, product = 5, blind_code = TRUE)

test_that("fails early", {
  expect_error(prepare())
  expect_error(prepare(n_panelist = 50))
  expect_error(prepare(product = 15))
})

test_that("design is correct", {
  expect_equal(attr(design, "n_panelist"), 20)
  expect_equal(attr(design, "n_product"), 5)
  expect_equal(attr(design, "blind_code"), TRUE)
  expect_s3_class(design, "tbl_sensory_design")
})
