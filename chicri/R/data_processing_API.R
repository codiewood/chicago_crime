library(RSocrata)
library(tidyverse)

data_API <- read.socrata("https://data.cityofchicago.org/resource/ijzp-q8t2.csv")

data <- data_API %>%
  select(-c(id, case_number, block, iucr, fbi_code, description, beat, ward, updated_on, x_coordinate, y_coordinate, location, year))

write_csv(data_API, file = "data_API.csv")
