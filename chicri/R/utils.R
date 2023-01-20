# Script containing general purpose utility functions
#'
#' @import tidyverse
#' @import lubridate
#' @importFrom RSocrata read.socrata
#' @import lubridate
#' @import dplyr
#' @import readr
#' @import forcats
NULL

#TODO: this should mimic the data processing R script
#' Title
#'
#' @param data Data set downloaded using API
#'
#' @return Processed data
#' @export
process_data <- function(data){

}

#' Renames data variable names to long format, with spaces, from short, with dots
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

#' Renames data variable names to short format, with dots, from long, with spaces
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
#' Returns a data frame of Chicago Crime data, optionally from specified year.
#'
#' @param year The year, between 2001 and present, of the data to be extracted. If NULL returns all data from 2001 to present. Default is "2019".
#'
#' @return tibble data frame of Chicago Crime data from the specified year.
#' @export
load_crimes_API <- function(year = "2019"){
  base_url <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv"
  if(is.null(year)){
    data_API <- RSocrata::read.socrata(base_url)
  }
  else {
    data_API <- RSocrata::read.socrata(paste0(base_url, "?year=", year))
  }
  data <- data_API %>%
    dplyr::as_tibble() %>%
    long_variables()
  return(data)
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



count_cases <- function(df, date_start = NULL, date_end = NULL, location_level =NULL, date_level = "month"){
  location_level <- enquo(location_level)

  if (is.null(date_start)){ #if no start date, set to before first observation in data
    date_start <- as.Date("01-01-2000")
  }
  if (is.null(date_end)){ #if no end date, set to current date
    date_end <- Sys.Date()
  }

  if (is.null(location_level)){ #if not including location
    if (date_level == "day"){ #if day selected
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(year = year(date), month = month(date), yday = day(date)) %>%
        group_by(year, month,day) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") { #if week selected
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        group_by(week_start) %>%
        dplyr::summarise(count = n()) %>%
        mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){ #if month selected
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month) %>%
        dplyr::summarise(count = n())
    }
  } else { #if we do include location

    if (date_level == "day"){
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(year = year(date), month = month(date), day = day(date)) %>%
        group_by(year, month,day, !!eval(location_level)) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") {
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        group_by(week_start, !!eval(location_level)) %>%
        dplyr::summarise(count = n()) %>%
        mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){
      count_dat <- df %>%
        filter(date > date_start, date < date_end) %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month, !!eval(location_level)) %>%
        dplyr::summarise(count = n())
    }
  }

  return(count_dat)
}
