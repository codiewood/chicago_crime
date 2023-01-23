# Script contains functions for binary classification
#' @import ggplot2
#' @import ROCit
NULL

#' ROC plot function
#'
#' @param fitval An numeric array of diagnostic score.
#' @param y An array of equal length of score, containing the class of the observations.
#'
#' @return A ggplot2 plot
#' @export
ggplotROC <- function(fitval, y){ # nocov start
  roc <- ROCit::rocit(score = fitval, class = y)
  meas <- ROCit::measureit(roc, measure = c("ACC", "SENS", "SPEC"))
  p <- ggplot() +
    geom_line(aes(x = 1 - meas$SPEC, y = meas$SENS)) +
    labs(title = 'ROC',x = '1 - Specifity', y = 'Sensitivity')
  return(p)
} # nocov end

#' AUC of ROC graph
#'
#' @param fitval An numeric array of diagnostic score.
#' @param y An array of equal length of score, containing the class of the observations.
#'
#' @return The AUC of an ROC graph
AUC <- function(fitval, y){ # nocov start
  roc_mod <- ROCit::rocit(score = fitval , class = y)
  return(roc_mod[['AUC']])
} # nocov end
