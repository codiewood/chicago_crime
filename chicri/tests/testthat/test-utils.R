test_that("load_crimes_csv works", {
  expect_error(load_crimes_csv(0))
  expect_error(load_crimes_csv("invalid_file_path"))
  expect_error(load_crimes_csv("data/raw/Crimes_-_2019.csv"))
})

test_that("othering works", {
  factor <- as.factor(c(rep("a",5),"b",rep("c",7),rep("d",2)))
  otherised <- othering(factor,3,TRUE) %>%
    fct_relevel(sort)
  expected <- as.factor(c(rep("a",5),"OTHER",rep("c",7),rep("OTHER",2))) %>%
    fct_relevel(sort)
  expect_equal(otherised,expected)
})

test_that("load_crimes_API gives errors", {
  expect_error(load_crimes_API(year = 2030))
  expect_error(load_crimes_API(year = c(2012, 2022)))
})

test_that("long_to_short works", {
  expect_equal(long_to_short("FBI Code"),"fbi_code")
  expect_equal(long_to_short("Location Description"),"location_description")
  expect_equal(long_to_short("IUCR"),"iucr")
})

test_that("short_to_long works", {
  expect_equal(short_to_long("fbi_code"),"Fbi Code")
  expect_equal(short_to_long("location_description"),"Location Description")
  expect_equal(short_to_long("iucr"),"Iucr")
})

test_that("short_variables works", {
  df_long <- dplyr::tibble(`Location Description` = NA,
                   `FBI Code` = NA,
                   IUCR = NA)
  df_short <- dplyr::tibble(location_description = NA,
                            fbi_code = NA,
                            iucr = NA)
  expect_equal(short_variables(df_long),df_short)
})

test_that("long_variables works", {
  df_long <- dplyr::tibble(`Location Description` = NA,
                           `FBI Code` = NA,
                           IUCR = NA)
  df_short <- dplyr::tibble(location_description = NA,
                            fbi_code = NA,
                            iucr = NA)
  expect_equal(long_variables(df_short),df_long)
})

test_that("process_data works", {
  df <- dplyr::tibble(`FBI Code` = c("1","2","3"),
                           IUCR = c("7","8",NA),
                           Arrest = c("TRUE", "TRUE", "FALSE"),
                           Domestic = c("true","false","true"),
                           Beat = c(1,2,3),
                           Ward = c(1,1,3),
                           District = c(1,5,6),
                           `Community Area` = c(16,54,3),
                           `Location Description` = c("a","b","b"),
                      `Primary Type`=c("CRIM SEXUAL ASSAULT","CRIMINAL SEXUAL ASSAULT","OTHER"))
  df2 <- process_data(df)
  expect_equal(levels(df2$`Primary Type`),c("CRIMINAL SEXUAL ASSAULT","OTHER"))
  expect_type(df2$Arrest,"logical")
  expect_s3_class(df2$IUCR,"factor")
})
