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
#' @importFrom rlang enquo .data
#' @importFrom tidyr drop_na
#' @import knitr
#' @importFrom kableExtra kable_styling
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
           District = as.factor(District))
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
  else if(as.integer(year) > 2023 | as.integer(year) < 2001){
    stop("Please enter valid year (between 2001 and 2023)")
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

#' Generate table for report
#'
#' @param data tibble containing data to be tabulated
#' @param align String vector with ncol(data) elements, specifying the alignments of the columns. String options are "l", "c" and "r".
#'
#' @return knitr kable with latex format
#' @export
report_table <- function(data,align){ # nocov start
  tab <- knitr::kable(data,
                      format="latex",
                      digits = 4,
                      align = align) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
  return(tab)
} # nocov end

#' Code to produce bar plots of factor variables
#'
#' @param data Data to be plotted
#' @param column Variable to be plotted
#' @param threshold Threshold value to be shown on plot. If NULL, no value shown. Default is NULL.
#'
#' @return Bar plot
#' @export
plot_factors <-  function(data, column, threshold = NULL){ # nocov start
  p <- ggplot(data) +
    geom_bar(aes(y = get(column)), colour = "#56B4E9", fill = "#56B4E9", alpha = 0.7) +
    labs(x = "Count", y = column)

  if (!is.null(threshold)){
    p <- p + geom_vline(xintercept=threshold, colour="red")
  }

  return(p)
} # nocov end
