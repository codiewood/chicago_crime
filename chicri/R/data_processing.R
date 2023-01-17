#' Script containing data processing
require(tidyverse)
require(lubridate)

data_all <- read_csv("data/raw/Crimes_-_2019.csv")

data <- data_all %>%
  select(-c(ID, `Case Number`, Block, IUCR, `FBI Code`, Description, Beat, Ward, `Community Area`, `Updated On`, Latitude, Longitude, Location, Year))

data$Date <- parse_datetime(data$Date, format = "%m/%d/%Y %I:%M:%S %p")

data <- drop_na(data)

data <- data %>%
  type_convert(col_types = list(`Primary Type` = col_factor(),
                                               `Location Description` = col_factor(),
                                               District = col_factor()))

data$`Primary Type` <- fct_collapse(data$`Primary Type`, "CRIMINAL SEXUAL ASSAULT" = c("CRIM SEXUAL ASSAULT","CRIMINAL SEXUAL ASSAULT"))

write_csv(data,"data/processed/Crimes_2019_Location_Type.csv")
