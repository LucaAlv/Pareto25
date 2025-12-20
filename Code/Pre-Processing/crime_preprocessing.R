library(tidyverse)
library(ipumsr)

crime_raw <- read_delim("Data/Crime/mexico-muni-month-homicide-rates-2000-2022.csv", delim = "|")
census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")
# Mapping data
census_sf_geo2_2020_ddi <- read_ipums_ddi("Data/IPUMS/Census_Data/ipumsi_00003.xml")
census_sf_geo2_2020_data_raw <- read_ipums_micro(census_sf_geo2_2020_ddi)


census_sf_geo2_2020_data <- census_sf_geo2_2020_data_raw %>%
  janitor::clean_names() %>%
  group_by(geo2_mx, geo2_mx2020) %>%
  summarise(.groups = "drop")