test_that("mean_repeat_metrics returns error", {
  dat_list <- list(byclass = tibble(v1 = NA, v2= NA),
                   overall = tibble(v3 = NA))
  expect_error(mean_repeat_metrics(dat_list,type="class"))
  expect_error(mean_repeat_metrics(dat_list,type="overall"))
})

test_that("mean_repeat_metrics takes expected input", {
  dat_list <- list(byclass = tibble(Repeat = rep(1,6),
                                    Fold = rep(1:2,3),
                                    Class = rep(c("a","b","c"),2),
                                    Sensitivity = rep(1,6),
                                    Specificity = rep(0,6),
                                    `Balanced Accuracy` = rep(0.5,6)),
                   overall = tibble(Fold = rep(1:2,3),
                                    Repeat = rep(1,6),
                                    Accuracy = rep(0.7,6),
                                    NIR = rep(0.5,6),
                                    Pval = rep(0,6)))
  expect_no_error(mean_repeat_metrics(dat_list,"overall"))
  expect_no_error(mean_repeat_metrics(dat_list,"class"))
})

test_that("mnlr_cv_indexed returns correct object", {
  X <- iris %>% select(-Species)
  y <- iris$Species
  index <- 1:25
  mnlr <- mnlr_cv_indexed(X,y,index = index,return_model = TRUE)
  expect_type(mnlr,"list")
  expect_equal(length(mnlr),2)
  mnlr2 <- mnlr_cv_indexed(X,y,index = index,return_model = FALSE)
  expect_type(mnlr2,"list")
  expect_equal(length(mnlr2),6)
})

test_that("mnlr_kfold_cv.df works", {
  X <- iris %>% select(-Species)
  y <- iris$Species
  metrics <- c("Sensitivity", "Specificity", "Balanced Accuracy")
  mnlr <- mnlr_kfold_cv.df(X,y,2,2, metrics = metrics)
  expect_type(mnlr,"list")
  expect_equal(length(mnlr),2)
  expect_equal(ncol(mnlr$overall),5)
  expect_equal(ncol(mnlr$byclass),6)
  expect_equal(nrow(mnlr$byclass),12)
})

