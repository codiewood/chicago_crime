test_that("count_cases returns errors on incorrect input", {
   expect_error(count_cases(iris, date_level = "weeek"))
   expect_error(count_cases(iris, location_levle = "florida"))
 })
