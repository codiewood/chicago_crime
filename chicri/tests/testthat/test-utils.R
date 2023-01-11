test_that("load_crimes", {
  expect_error(load_crimes(0))
  expect_error(load_crimes("invalid_file_path"))
  expect_error(load_crimes("data/raw/Crimes_-_2019.csv"))
  df <- load_crimes(filepath = "data/processed/Crimes_2019_Location_Type.csv")
  expect_true(object=is.factor(df$`Primary Type`))
  expect_true(object=is.factor(df$`Location Description`))
  expect_true(object=is.factor(df$District))
})
