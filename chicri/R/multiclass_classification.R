# Script contains functions for multi-class classification

#' Significance test for features
#'
#' Tests if features are significant in a multinomial logistic regression model using 2 tail z tests
#'
#' @param model a multinomial logistic regression model
#' @param alpha numeric significance level
#'
#' @return list containing z statistics, p values and indicators of significance.
#' @export
#'
#' @examples
significance_test <- function(model,alpha = 0.05){
  z <- summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  sig <- colSums(p < alpha) != 0
  return(list(zstat = z,pvals = p, significant_features = sig))
}

#this broeken because tibble vs list dollar doesnt work
reorder_factors <- function(data, factor_list){
  for(factor in factor_list){
    data$factor <- reorder(data$factor,data$factor,FUN=length)
  }
  return(data)
}

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
