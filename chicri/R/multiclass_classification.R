# Script contains functions for multi-class classification
#' @importFrom nnet multinom
#' @import ggplot2
#' @import tidyverse
#' @import caret
NULL

#' Significance test for features
#'
#' @description
#' Tests if features are significant in a multinomial logistic regression model using 2 tail z tests
#'
#' @param model a multinomial logistic regression model
#' @param alpha numeric significance level
#'
#' @return list containing z statistics, p values and indicators of significance.
#' @export
significance_test <- function(model,alpha = 0.05){
  z <- summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  sig <- colSums(p < alpha) != 0
  return(list(zstat = z,pvals = p, significant_features = sig))
}

#' Relevels the `Location Description` factor variable by grouping categories and otherising
#'
#' @param data Data frame to be used
#' @param threshold Threshold count below which category will be converted to other.
#'
#' @return Data frame with modified factor
#' @export
regroup_locations <- function(data, threshold){
  locations <- levels(data$`Location Description`)
  school <- locations[grepl("SCHOOL", locations)]
  airport <- locations[grepl("AIRPORT", locations)]
  uni <- locations[grepl("UNIV", locations)]

  data$`Location Description` <- fct_collapse(data$`Location Description`,
                                              "SCHOOL" = school,
                                              "UNIVERSITY" = uni,
                                              "AIRPORT" = airport,
                                              "OTHER" = c("OTHER", "OTHER (SPECIFY)"))
  data$`Location Description` <- othering(data$`Location Description`, threshold)
  return(data)
}

#' Indexed cross-validation for multinomial regression
#'
#' @description
#' Performs cross-validation computing metrics using specified indexes of the data set.
#'
#' @param X Data to be used, not including the response variable or any variables not for use in model.
#' @param y Response variable, as a factor.
#' @param index The indices of the rows to be used as test data.
#'
#' @return A confusion matrix, see `?confusionMatrix`
#' @export
mnlr_cv_indexed <- function(X, y, index){
  # Separate data into training and test data
  X_training <- X[-index, ,drop = F]
  X_testing <- X[index, ,drop=F]
  y_training <- y[-index]
  y_testing <- y[index]
  # Obtaining full data for model fitting
  training <- X_training %>% mutate(y = y_training)

  # Fit model with training data
  model <- multinom(y ~ ., data=training)

  # Predict on test data
  pred.test <- predict(model, X_testing, type="class")
  # Obtain metrics
  conf.test <- confusionMatrix(pred.test,y_testing,mode="everything")
  return(conf.test)
}

#' K-fold cross-validation for multinomial regression
#'
#' @param X Data to be used, not including the response variable or any variables not for use in model.
#' @param y Response variable, as a factor.
#' @param k Number of folds.
#' @param n_reps Number of repeats.
#' @param metrics Vector of strings with metrics to be obtained. Overall accuracy will always be returned. Options are "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence", "Detection Rate","Detection Prevalence" and "Balanced Accuracy".
#'
#' @return List of length equal to that of `metrics` + 1, with each element containing a list or vector of the mean of the metric at each repeat, averaged over `k` folds.
#' @export
mnlr_kfold_cv <- function(X, y, k, n_reps = 5, metrics) {
  n <- nrow(X)
  m <- length(metrics)
  metric_list <- list()
  for (i in 1:n_reps) {
    met <- list()
    folds <- split(sample(1:n), 1:k)
    for (fold_num in 1:k) {
      cm <- mnlr_cv_indexed(X, y, index = folds[[fold_num]])
      overall_acc <- cm$overall[1]

      met[[fold_num]] <- list(overall_accuracy = overall_acc,
                       performance_measures = cm$byClass[,metrics])
    }
    metric_list[[i]] <- met
  }

  # Average the list of metrics
  class_names <- levels(y)
  c <- length(class_names)
  mean_metrics <- list()
  # Average overall accuracy
  mean_metrics[[1]] <- numeric(n_reps)
  for (rep_num in 1:n_reps) {
    mean_metrics[[1]][rep_num] <- lapply(1:k, function(fold_num) metric_list[[rep_num]][[fold_num]]$overall_accuracy) %>%
      unlist() %>%
      mean()
  }
  # Average class specific measures
  for (met_num in 1:m){
    mean_metrics[[met_num+1]] <- list()
    for(class_num in 1:c){
      mean_metrics[[met_num+1]][[class_num]] <- numeric(n_reps)
      for(rep_num in 1:n_reps){
        mean_metrics[[met_num+1]][[class_num]][rep_num] <- lapply(1:k, function(fold_num) metric_list[[rep_num]][[fold_num]]$performance_measures[class_num,met_num]) %>%
          unlist() %>%
          mean()
      }
    }
    names(mean_metrics[[met_num+1]]) <- class_names
  }
  names(mean_metrics) <- c("Overall Accuracy",metrics)
  return(mean_metrics)
}
