#' @importFrom RSocrata read.socrata
#' @import dplyr
#' @import sf
#' @importFrom tidyr drop_na

get_map <- function(map = "community_area"){
  if (map == "community_area"){
    community_map <- read.socrata("https://data.cityofchicago.org/resource/igwz-8jzy.csv")
    community_map <- community_map %>%
      select(c(the_geom, Area = area_numbe)) %>%
      st_as_sf(wkt = "the_geom") %>%
      mutate(Area = as.factor(Area)) %>%
      arrange(Area)

    return(community_map)

  } else if (map == "district"){
    district_map <- read.socrata("https://data.cityofchicago.org/resource/24zt-jpfn.csv")
    district_map <- district_map %>%
      select(- dist_label) %>%
      st_as_sf(wkt = "the_geom") %>%
      mutate(District = as.factor(dist_num)) %>%
      arrange(District)

    return(district_map)
  }

}


get_indicators <- function(){
  socio_ind <- read.socrata("https://data.cityofchicago.org/resource/i9hv-en6g.csv")
  socio_ind <- socio_ind %>% select(-c(community_area_name)) %>% mutate(ca = as.factor(ca))
  colnames(socio_ind) = c("community_area", "poverty_rate", "income", "hardship_index")
  socio_ind <- socio_ind %>% long_variables() %>% drop_na()

  return(socio_ind)
}
