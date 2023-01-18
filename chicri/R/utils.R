# Script containing general purpose utility functions
#'
#' @import tidyverse
#' @import lubridate
#' @import RSocrata
NULL

#TODO: this should mimic the data processing R script and convert into tibble format
#' Title
#'
#' @param data Data set downloaded using API
#'
#' @return Processed data
#' @export
process_data <- function(data){

}

#' Renames data variable names to long format from short
#'
#' @param data The data set with the variables to be renamed
#'
#' @return data frame with long variable names
#' @export
long_variables <- function(data){
  short_names <- colnames(data)
  var_lkp <- read.table("data/raw/feature_names.csv", header = T, sep = ",")
  colnames(data) <- var_lkp$long_name[match(short_names, var_lkp$short_name)]
  return(data)
}

#' Renames data variable names to short format from long
#'
#' @param data The data set with the variables to be renamed
#'
#' @return data frame with short variable names
#' @export
short_variables <- function(data){
  long_names <- colnames(data)
  var_lkp <- read.table("data/raw/feature_names.csv", header = T, sep = ",")
  colnames(data) <- var_lkp$short_name[match(long_names, var_lkp$long_name)]
  return(data)
}

#' Load chicago crime data using Socrata API.
#'
#' @description
#' Returns a data frame of Chicago Crime data from specified year or range of years,
#' or returns entire dataset from 2001 to present if no years are specified.
#'
#' @param filepath path to the processed csv file.
#'
#' @return tibble data frame of Chicago Crime data.
#' @export
load_crimes_API <- function(){
  #at some point call process_data, maybe make this an option
}

#' Load chicago crime data from processed csv.
#'
#' @param filepath path to the processed csv file.
#'
#' @return tibble data frame of Chicago Crime data.
#' @export
load_crimes_csv <- function(filepath){
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
#' @param print_summary Boolean indicating if a summary of the operations should be printed. Default is FALSE.
#'
#' @return Otherised factor vector
#' @export
othering <- function(factor_vec, threshold, print_summary = FALSE){
    level <- levels(factor_vec)
    tab <- tabulate(factor_vec)
    other.levels <- level[tab < threshold]
    factor_vec <- fct_collapse(factor_vec, "OTHER" = other.levels)
    if (print_summary){
      cat(paste0(length(other.levels), " out of ", length(tab),
                 " categories converted to OTHER, ",
                 round(100 * length(factor_vec[factor_vec == "OTHER"]) / length(factor_vec),
                       digits = 2),
                 "% of data values. \n"))
    }
    return(factor_vec)
}

