context("Sensory data analysis")

df <- specify(
  data = perfume_qda_consumers,
  sensory_method = "QDA",
  panelist = consumer,
  product = product,
  attribute = intensity:green,
  hedonic = NULL
)

qda_local <- analyse(df, "local")
qda_global <- analyse(df, "global")

test_that("fails early", {
  expect_error(analyse())
  expect_error(analyse(df))
  expect_error(analyse(df, choice = "not_valid"))
})

test_that("local sensory data analysis is correct", {
  expect_equal(attr(qda_local, "sensory_method"), "QDA")
  expect_equal(attr(qda_local, "method_local"), "Analysis of variance")
  expect_equal(attr(qda_local, "model"), "value ~ product + panelist")
  expect_s3_class(qda_local, "tbl_sensory_local")
})

test_that("global sensory data analysis is correct", {
  expect_equal(attr(qda_global, "sensory_method"), "QDA")
  expect_equal(attr(qda_global, "method_global"), "Principal Component Analysis")
  expect_s3_class(qda_global, "tbl_sensory_global")
})
