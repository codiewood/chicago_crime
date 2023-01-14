# Script containing general purpose utility functions
require(tidyverse)
require(lubridate)

#' Load chicago crime data from processed csv.
#'
#' @param filepath path to the processed csv file.
#'
#' @return tibble data frame of Chicago Crime data.
#' @export
load_crimes <- function(filepath){
  if (grepl("raw", filepath)) {
    stop("Please ensure the file specified contains processed data.")
  }
  message("Loading processed data...")
  data <- read_csv(filepath) %>%
    type_convert(col_types = list(`Primary Type` = col_factor(),
                                  `Location Description` = col_factor(),
                                  District = col_factor()))
  return(data)
}

#' Convert less common levels to "OTHER", to be used when `Primary Type` or `Location Description` is present to reduce category numbers.
#'
#' @param factor_vec Factor vector to be otherised.
#' @param threshold Threshold count below which will be converted to other.
#'
#' @return Otherised factor vector
#' @export
othering <- function(factor_vec, threshold){
    level <- levels(factor_vec)
    tab <- tabulate(factor_vec)
    other.levels <- level[tab < threshold]
    factor_vec <- fct_collapse(factor_vec, "OTHER" = other.levels)
    return(factor_vec)
}

