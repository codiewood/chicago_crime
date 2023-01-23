# Script contains functions for multi-class classification
#' @importFrom nnet multinom
#' @import ggplot2
#' @importFrom caret confusionMatrix
#' @import magrittr
#' @import forcats
#' @import dplyr
#' @importFrom rlang := .data
NULL

#' Relevels the `Location Description` factor variable by grouping categories and otherising
#'
#' @param data Data frame to be used
#' @param threshold Threshold count below which category will be converted to other.
#'
#' @return Data frame with modified factor
#' @export
regroup_locations <- function(data, threshold){ # nocov start
  locations <- levels(data$`Location Description`)
  school <- locations[grepl("SCHOOL", locations)]
  airport <- locations[grepl("AIRPORT", locations)]
  uni <- locations[grepl("UNIV", locations)]

  data$`Location Description` <- forcats::fct_collapse(data$`Location Description`,
                                              "SCHOOL" = school,
                                              "UNIVERSITY" = uni,
                                              "AIRPORT" = airport,
                                              "OTHER" = c("OTHER", "OTHER (SPECIFY)"))
  data$`Location Description` <- othering(data$`Location Description`, threshold)
  return(data)
} # nocov end

#' Indexed cross-validation for multinomial regression
#'
#' @description
#' Performs cross-validation computing metrics using specified indexes of the data set.
#'
#' @param X Data to be used, not including the response variable or any variables not for use in model.
#' @param y Response variable, as a factor.
#' @param index The indices of the rows to be used as test data.
#' @param return_model Boolean to indicate if the model object is returned. Default is FALSE.
#'
#' @return A confusion matrix, see `?confusionMatrix`. If return_model = TRUE, a list containing a confusion matrix and a multinomial logistic regression model object, see `?multinom`
#' @export
mnlr_cv_indexed <- function(X, y, index, return_model = FALSE){
  # Separate data into training and test data
  X_training <- X[-index, ,drop = F]
  X_testing <- X[index, ,drop=F]
  y_training <- y[-index]
  y_testing <- y[index]
  # Obtaining full data for model fitting
  training <- X_training %>% dplyr::mutate(y = y_training)

  # Fit model with training data
  model <- nnet::multinom(y ~ ., data=training)

  # Predict on test data
  pred.test <- stats::predict(model, X_testing, type="class")
  # Obtain metrics
  conf.test <- caret::confusionMatrix(pred.test,y_testing,mode="everything")
  if(return_model){
    return(list(conf.matrix = conf.test, model = model))
  }
  else {
    return(conf.test)
  }
}

#' K-fold cross-validation metrics for multinomial regression, in list format.
#'
#' @param X Data to be used, not including the response variable or any variables not for use in model.
#' @param y Response variable, as a factor.
#' @param k Number of folds.
#' @param n_reps Number of repeats.
#' @param metrics Vector of strings with metrics to be obtained. Overall accuracy, no information rate (NIR) and the p value for accuracy > NIR will always be returned. Options are "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence", "Detection Rate","Detection Prevalence" and "Balanced Accuracy". At least one must be given and must be in vector format.
#'
#' @return List containing two elements: a list of length equal to that of `metrics` + 3, with each element containing a list or vector of the mean of the metric at each repeat, averaged over `k` folds, and a list of length equal to `n_reps`, containing the metrics for each of the `k` folds.
#' @export
mnlr_kfold_cv.list <- function(X, y, k, n_reps = 5, metrics) { # nocov start
  n <- nrow(X)
  m <- length(metrics)
  metric_list <- list()
  for (rep_num in 1:n_reps) {
    met <- list()
    folds <- split(sample(1:n), 1:k)
    for (fold_num in 1:k) {
      cm <- mnlr_cv_indexed(X, y, index = folds[[fold_num]])
      overall_acc <- cm$overall[1]
      no_info_rate <- cm$overall[5]
      pval <- cm$overall[6]

      met[[fold_num]] <- list(overall_accuracy = overall_acc,
                              NIR = no_info_rate,
                              accuracy.NIR_pval = pval,
                       performance_measures = cm$byClass[,metrics])
    }
    metric_list[[rep_num]] <- met
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

  # Average NIR
  mean_metrics[[2]] <- numeric(n_reps)
  for (rep_num in 1:n_reps) {
    mean_metrics[[2]][rep_num] <- lapply(1:k, function(fold_num) metric_list[[rep_num]][[fold_num]]$NIR) %>%
      unlist() %>%
      mean()
  }

  # Average Acc > NIR P-value
  mean_metrics[[3]] <- numeric(n_reps)
  for (rep_num in 1:n_reps) {
    mean_metrics[[3]][rep_num] <- lapply(1:k, function(fold_num) metric_list[[rep_num]][[fold_num]]$accuracy.NIR_pval) %>%
      unlist() %>%
      mean()
  }

  # Average class specific measures
  for (met_num in 1:m){
    mean_metrics[[met_num+3]] <- list()
    for(class_num in 1:c){
      mean_metrics[[met_num+3]][[class_num]] <- numeric(n_reps)
      for(rep_num in 1:n_reps){
        mean_metrics[[met_num+3]][[class_num]][rep_num] <- lapply(1:k, function(fold_num) metric_list[[rep_num]][[fold_num]]$performance_measures[class_num,met_num]) %>%
          unlist() %>%
          mean()
      }
    }
    names(mean_metrics[[met_num+3]]) <- class_names
  }
  names(mean_metrics) <- c("Overall Accuracy", "NIR", "P-value (Acc > NIR)",metrics)
  return(list(mean_metrics = mean_metrics, all_metrics = metric_list))
} # nocov end

#' K-fold cross-validation metrics for multinomial regression, in data frame format.
#'
#' @param X Data to be used, not including the response variable or any variables not for use in model.
#' @param y Response variable, as a factor.
#' @param k Number of folds.
#' @param n_reps Number of repeats.
#' @param metrics Vector of strings with metrics to be obtained. Overall accuracy, no information rate (NIR) and the p value for accuracy > NIR will always be returned. Options are "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence", "Detection Rate","Detection Prevalence" and "Balanced Accuracy". At least one must be given and must be in vector format.
#'
#' @return List containing two elements: a data frame of the overall metrics (Overall accuracy, no information rate (NIR) and the p value for accuracy > NIR), and a data frame containing the class specific metrics given in `metrics`.
#' @export
mnlr_kfold_cv.df <- function(X, y, k, n_reps = 5, metrics) {
  n <- nrow(X)
  m <- length(metrics)
  r <- n_reps * k
  class_names <- levels(y)
  c <- length(class_names)
  # Initiate empty dfs with repeat, fold and class labels
  df_overall <- data.frame(Repeat = as.factor(rep(1:n_reps, each=k)),
                           Fold = as.factor(rep(1:k,n_reps)),
                           Accuracy= rep(NA, r),
                           NIR =rep(NA, r),
                           Pval = rep(NA, r))
  df_byclass <- data.frame(Repeat = as.factor(rep(1:n_reps, each=k*c)),
                           Fold = as.factor(rep(rep(1:k,each=c),n_reps)),
                           Class = as.factor(rep(class_names,n_reps)))
  # Add variables for each metric
  for(met_num in 1:m){
    df_byclass <- df_byclass %>%
      mutate("{metrics[met_num]}" := NA)
  }
  for(rep_num in 1:n_reps){
    folds <- split(sample(1:n), 1:k)
    for(fold_num in 1:k){
      cm <- mnlr_cv_indexed(X, y, index = folds[[fold_num]])
      # Create df for overall statistics
      ind <- df_overall$Repeat == rep_num & df_overall$Fold == fold_num
      df_overall[ind,]$Accuracy <- cm$overall[1]
      df_overall[ind,]$NIR <- cm$overall[5]
      df_overall[ind,]$Pval <- cm$overall[6]

      # Create df for statistics by class
      ind.c <- df_byclass$Repeat == rep_num & df_byclass$Fold == fold_num
      for(met_num in 1:m){
        df_byclass[ind.c,met_num+3] <- cm$byClass[,metrics][,met_num]
      }

    }
  }
  return(list(overall = df_overall, byclass = df_byclass))
}

#' Calculate the means across folds of performance metrics
#'
#' @param results Output of `mnlr_kfold_cv.df`
#' @param type String specifying which metric type to extract. Options are "class" or "overall". "class" should only be used for results containing exactly the by class metrics of Sensitivity, Specificty and Balanced Accuracy.
#'
#' @return A tibble containing the metrics, averages over all folds, for each repeat.
#' @export
mean_repeat_metrics <- function(results,type){
  if(type == "class"){
    expected_vars <- c("Repeat","Fold","Class","Sensitivity","Specificity","Balanced Accuracy")
    if(!setequal(expected_vars,colnames(results$byclass))){ #If metrics are not as expected
      stop("Please only call the type class if you are inputting results containing the by class metrics of Sensitivity, Specificty and Balanced Accuracy")
    }
    repeat_means <- results$byclass %>%
    dplyr::group_by(Class,Repeat) %>%
    dplyr::summarise("Average Sensitivity" = mean(Sensitivity),
                     "Average Specificity" = mean(Specificity),
                     "Average Balanced Accuracy" = mean(`Balanced Accuracy`))
  }
  else if(type == "overall"){
    repeat_means <- results$overall %>%
      dplyr::group_by(Repeat) %>%
      dplyr::summarise("Average Overall Accuracy" = mean(Accuracy),
                       "Average NIR" = mean(NIR),
                       "Maximum P Value (Acc > NIR)" = max(Pval))
  }
  repeat_means <- repeat_means %>%
    dplyr::relocate(Repeat) %>%
    dplyr::arrange(Repeat)
  return(repeat_means)
}

