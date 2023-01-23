# Script containing functions for regression
#'
#' @importFrom RSocrata read.socrata
#' @import dplyr
#' @import sf
#' @importFrom tidyr drop_na
NULL

#' Load area mapping shapefile from the Chicago database
#'
#' @param map String specifying which map to load. Options are "community_area" and "district".
#'
#' @return The requested map as a shapefile consisting of geometries and the relevant area coding
#' @export
get_map <- function(map = "community_area"){ # nocov start
  if (map == "community_area"){
    community_map <- RSocrata::read.socrata("https://data.cityofchicago.org/resource/igwz-8jzy.csv")
    community_map <- community_map %>%
      dplyr::select(c(the_geom, Area = area_numbe)) %>%
      sf::st_as_sf(wkt = "the_geom") %>%
      dplyr::mutate(Area = as.factor(Area)) %>%
      dplyr::arrange(Area)
    return(community_map)
  } else if (map == "district"){
    district_map <- RSocrata::read.socrata("https://data.cityofchicago.org/resource/24zt-jpfn.csv")
    district_map <- district_map %>%
      dplyr::select(- dist_label) %>%
      sf::st_as_sf(wkt = "the_geom") %>%
      dplyr::mutate(District = as.factor(dist_num)) %>%
      dplyr::arrange(District)
    return(district_map)
  }
} # nocov end

#' Load socioecononic indicators data from the Chicago database
#'
#' @return The socioeconomic data frame from the Chicago databases
#' @export
get_indicators <- function(){ # nocov start
  socio_ind <- RSocrata::read.socrata("https://data.cityofchicago.org/resource/i9hv-en6g.csv")
  socio_ind <- socio_ind %>%
    dplyr::select(-c(community_area_name)) %>%
    dplyr::mutate(ca = as.factor(ca))
  colnames(socio_ind) = c("community_area", "poverty_rate", "income", "hardship_index")
  socio_ind <- socio_ind %>%
    long_variables() %>%
    tidyr::drop_na()
  return(socio_ind)
} # nocov end

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
  } else if (location_level == "community_area"){ #if we do include location

    if (date_level == "day"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date), day = day(date)) %>%
        dplyr::group_by(year, month,day, community_area) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") {
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        dplyr::group_by(week_start, community_area) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date)) %>%
        dplyr::group_by(year, month, community_area) %>%
        dplyr::summarise(count = n())
    }
  } else if (location_level == "district") {
    if (date_level == "day"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date), day = day(date)) %>%
        dplyr::group_by(year, month,day, district) %>%
        dplyr::summarise(count = n())
    } else if (date_level == "week") {
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
        dplyr::group_by(week_start, district) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(year = year(week_start), week = week(week_start))
    } else if (date_level == "month"){
      count_dat <- df %>%
        dplyr::filter(date > date_start, date < date_end) %>%
        dplyr::mutate(year = year(date), month = month(date)) %>%
        dplyr::group_by(year, month, district) %>%
        dplyr::summarise(count = n())
    }

  }
  count_dat <- long_variables(count_dat)
  return(count_dat)
}
