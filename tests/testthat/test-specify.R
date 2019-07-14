context("Specify sensory table")

df <- specify(
  data = perfume_qda_consumers,
  sensory_method = "QDA",
  panelist = consumer,
  product = product,
  attribute = intensity:green,
  hedonic = NULL
)

test_that("fails early", {
  expect_error(specify())
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "not_valid"))
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "QDA"))
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "QDA",
                       panelist = consumer))
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "QDA",
                       product = product))
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "QDA",
                       panelist = consumer,
                       product = product))
  expect_error(specify(data = perfume_qda_consumers, 
                       method = "QDA",
                       panelist = consumer,
                       product = not_valid))
})

test_that("sensory table is correct", {
  expect_equal(attr(df, "sensory_method"), "QDA")
  expect_equal(attr(df, "panelist"), "consumer")
  expect_equal(attr(df, "n_panelist"), 103)
  expect_equal(attr(df, "product"), "product")
  expect_equal(attr(df, "n_product"), 12)
  expect_equal(attr(df, "session"), "NULL")
  expect_equal(attr(df, "pres_order"), "NULL")
  expect_identical(attr(df, "attribute"), c('intensity', 
                                            'freshness', 
                                            'jasmin', 
                                            'rose',
                                            'camomille',
                                            'fresh_lemon', 
                                            'vanilla', 
                                            'citrus',
                                            'anis',
                                            'sweet_fruit',
                                            'honey', 
                                            'caramel', 
                                            'spicy',
                                            'woody', 
                                            'leather', 
                                            'nutty', 
                                            'musk', 
                                            'animal', 
                                            'earthy', 
                                            'incense', 
                                            'green'))
  expect_equal(attr(df, "n_attribute"), 21)
  expect_equal(attr(df, "hedonic"), "NULL")
  expect_s3_class(df, "tbl_sensory_qda")
})
