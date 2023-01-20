# Script containing general purpose utility functions
#'
#' @import lubridate
#' @importFrom RSocrata read.socrata
#' @import dplyr
#' @import readr
#' @import forcats
#' @import utils
#' @import magrittr
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom rlang enquo
#' @importFrom tidyr drop_na
NULL

#' Process Chicago Crime data
#' @description
#' Converts columns to the correct types, formatting the date variable and merging the `Primary Type` categories of "CRIM SEXUAL ASSAULT" and "CRIMINAL SEXUAL ASSAULT".
#'
#' @param data Data set to process, in tibble format and with long variable names
#'
#' @return Processed data
#'
#' @export
process_data <- function(data){
  data <- data %>%
    readr::type_convert(col_types = list(Arrest = readr::col_logical(),
                                         Domestic = readr::col_logical(),
                                         IUCR = readr::col_factor(),
                                         `Primary Type` = readr::col_factor(),
                                         `Location Description` = readr::col_factor(),
                                         `FBI Code` = readr::col_factor())) %>%
    mutate(`Community Area` = as.factor(`Community Area`),
           Beat = as.factor(Beat),
           Ward = as.factor(Ward),
           District = as.factor(District)) %>%
    tidyr::drop_na()
  data$`Primary Type` <- forcats::fct_collapse(data$`Primary Type`, "CRIMINAL SEXUAL ASSAULT" = c("CRIM SEXUAL ASSAULT","CRIMINAL SEXUAL ASSAULT"))
  return(data)
}

#' Converts short name format with underscores to long name format
#'
#' @param string String to be converted
#'
#' @return the string, with underscores replaced by spaces and capitalised
#' @export
short_to_long <- function(string){
  string <- stringr::str_replace_all(string, "_", " ")
  string <- stringr::str_to_title(string)
  return(string)
}

#' Converts long name format with spaces to short name format with underscores
#'
#' @param string String to be converted
#'
#' @return the string, with underscores and uncapitalised
#' @export
long_to_short <- function(string){
  string <- stringr::str_replace_all(string, " ", "_")
  string <- tolower(string)
  return(string)
}

#' Renames data variable names to long format, with spaces, from short, with underscores
#'
#' @param data The data set with the variables to be renamed
#'
#' @return data frame with long variable names
#' @export
long_variables <- function(data){
  short_names <- colnames(data)
  long_names <- vector(length = length(short_names))
  for (i in 1:length(short_names)){
    if(short_names[i] == "iucr"){
      long_names[i] <- "IUCR"
    }
    else if(short_names[i] == "fbi_code"){
      long_names[i] <- "FBI Code"
    }
    else if(short_names[i] == "id"){
      long_names[i] <- "ID"
    }
    else{
      long_names[i] <- short_to_long(short_names[i])
    }
  }
  colnames(data) <- long_names
  return(data)
}

#' Renames data variable names to short format, with underscores, from long, with spaces.
#'
#' @param data The data set with the variables to be renamed
#'
#' @return data frame with short variable names
#' @export
short_variables <- function(data){
  long_names <- colnames(data)
  short_names <- vector(length = length(long_names))
  for (i in 1:length(short_names)){
    short_names[i] <- long_to_short(long_names[i])
  }
  colnames(data) <- short_names
  return(data)
}

#' Load chicago crime data using Socrata API.
#'
#' @description
#' Returns a data frame of Chicago Crime data, optionally from specified year.
#'
#' @param year The year, between 2001 and present, of the data to be extracted. If NULL returns all data from 2001 to present. Default is "2019".
#'
#' @return tibble data frame of Chicago Crime data from the specified year, with long variable names.
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
  data <- readr::read_csv(filepath) %>%
    readr::type_convert(col_types = list(`Primary Type` = readr::col_factor(),
                                  `Location Description` = readr::col_factor(),
                                  District = readr::col_factor()))
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
    factor_vec <- forcats::fct_collapse(factor_vec, "OTHER" = other.levels)
    if (print_summary){
      cat(paste0(length(other.levels), " out of ", length(tab),
                 " categories converted to OTHER, ",
                 round(100 * length(factor_vec[factor_vec == "OTHER"]) / length(factor_vec),
                       digits = 2), "% of data values. \n"))
    }
    return(factor_vec)
}

#' Count the number of crimes in a given time frame and location level
#'
#' @param df Data frame
#' @param date_start Date to begin the count
#' @param date_end Date to end the count
#' @param location_level Column name of the relevant location level from the data set.
#' @param date_level String specifying the date level required. Options are "day", "week" or "month".
#'
#' @return Data frame containing the relevant count data
#' @export
count_cases <- function(df, date_start = NULL, date_end = NULL, location_level =NULL, date_level = "month"){
  if (!all(names(df) == tolower(names(df)))){
    df <- short_variables(df)
  }

  if (is.null(date_start)){ #if no start date, set to before first observation in data
    date_start <- as.Date("01-01-2000")
  }
  if (is.null(date_end)){ #if no end date, set to current date
    date_end <- Sys.Date()
  }

  if (is.null(location_level)){ #if not including location
    if (date_level == "day"){ #if day selected
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date), yday = day(date)) %>%
        dplyr::group_by(year, month,day) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") { #if week selected
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        dplyr::group_by(week_start) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){ #if month selected
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date)) %>%
        dplyr::group_by(year, month) %>%
        dplyr::summarise(count = n())
    }
  } else { #if we do include location

    if (!(location_level == tolower(location_level))){
      location_level <- long_to_short(location_level)
    }
    location_level <- rlang::enquo(location_level)

    if (date_level == "day"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date), day = day(date)) %>%
        dplyr::group_by(year, month,day, !!eval(location_level)) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") {
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        dplyr::group_by(week_start, !!eval(location_level)) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date)) %>%
        dplyr::group_by(year, month, !!eval(location_level)) %>%
        dplyr::summarise(count = n())
    }
  }
  count_dat <- long_variables(count_dat)
  return(count_dat)
}
