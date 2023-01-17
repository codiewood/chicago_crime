test_that("load_crimes_csv works", {
  expect_error(load_crimes_csv(0))
  expect_error(load_crimes_csv("invalid_file_path"))
  expect_error(load_crimes_csv("data/raw/Crimes_-_2019.csv"))
  df <- load_crimes_csv(filepath = "data/processed/Crimes_2019_Location_Type.csv")
  expect_true(object=is.factor(df$`Primary Type`))
  expect_true(object=is.factor(df$`Location Description`))
  expect_true(object=is.factor(df$District))
})

test_that("othering works", {
})

