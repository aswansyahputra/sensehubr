context("Create template for sensory table")

design <- prepare(n_panelist = 20, product = 5, blind_code = TRUE)
template <- create_template(design, attribute = c("Sweetness", "Mint", "Green", "Herbal"))

test_that("fails early", {
  expect_error(create_template())
  expect_error(create_template(mtcars))
  expect_error(create_template(design))
})

test_that("template is correct", {
  expect_equal(attr(template, "n_panelist"), 20)
  expect_equal(attr(template, "n_product"), 5)
  expect_identical(attr(template, "attribute"), c("Sweetness", 
                                                "Mint", 
                                                "Green", 
                                                "Herbal"))
  expect_s3_class(template, "tbl_sensory_template")
})
