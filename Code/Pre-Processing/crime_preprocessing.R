library(tidyverse)
library(ipumsr)
library(foreign)

#### Load Mapping data ####
census_sf_geo2_2020_ddi <- read_ipums_ddi("Data/IPUMS/Census_Data/ipumsi_00003.xml")
census_sf_geo2_2020_data_raw <- read_ipums_micro(census_sf_geo2_2020_ddi)

# This is the mapping of official 2020 codes used by the mexican government to the harmonized IPUMS geo2_mx codes
# We need this to match the disaster data below to the ipums census data
census_sf_geo2_2020_data <- census_sf_geo2_2020_data_raw %>%
  janitor::clean_names() %>%
  group_by(geo2_mx, geo2_mx2020) %>%
  summarise(.groups = "drop") %>%
  mutate(
    geo2_mx = as.character(geo2_mx),
    geolevel2_alt = sprintf("%05d", geo2_mx2020))

census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")

#### Process crime data for all years ####

years <- 2005:2020

# Function to process a single year
process_crime_year <- function(year) {
  # Extract last two digits of year for filename
  year_short <- substr(as.character(year), 3, 4)
  
  # Construct file path
  file_path <- paste0("Data/Crime/defunciones_base_datos_", year, "_dbf/defun", year_short, ".dbf")
  
  # Read and process the data
  crime_data_raw <- read.dbf(file_path)
  
  crime_data <- crime_data_raw %>%
    janitor::clean_names() %>%
    select(ent_regis, mun_regis, causa_def, lista_mex, anio_ocur) %>%
    filter(lista_mex == 55) %>%
    filter(anio_ocur == year) %>%
    # Make sure that formatted the same for all years (only relevant for 2005-2009)
    mutate(
      ent_regis = sprintf("%02d", ent_regis),
      mun_regis = sprintf("%03d", mun_regis)  
    ) %>%
    mutate(
      geolevel2 = paste0("4840", ent_regis, mun_regis),
      geolevel2_alt = paste0(ent_regis, mun_regis))
  
  return(crime_data)
}

# Apply the function to all years and combine into one dataset
crime_data_all <- purrr::map_df(years, process_crime_year)

#### Map crime data to census data (geographically and time)

crime_data_census <- crime_data_all %>%
  mutate(
    # Time mapping
    year_census = case_when(
      # example: a disaster in 2000 is relevant for migration flows between 2000 and 2005 / the 2005 census data gives us information about something that happened in 2000
      anio_ocur %in% 1995:2000 ~ 2000, # Relevant for Census in 2000
      anio_ocur %in% 2001:2005 ~ 2005, 
      anio_ocur %in% 2006:2010 ~ 2010, # Relevant for Census in 2010
      anio_ocur %in% 2011:2015 ~ 2015, # Relevant for Census in 2015
      anio_ocur %in% 2016:2020 ~ 2020, # Relevant for Census in 2020
      anio_ocur %in% 2021:2025 ~ 2025
    )
  ) %>%
  filter(year_census %in% c(2010, 2015, 2020)) %>%
  group_by(geolevel2_alt, year_census) %>%
  summarise(crime_period = n(), .groups = "drop") %>%
  # Complete all combinations with geolevel2_alt from census data
  complete(
    # Set years and municipalities without entry in the original dataset to 0
    year_census = c(2010, 2015, 2020),
    geolevel2_alt = census_sf_geo2_2020_data$geolevel2_alt,  # Use this instead
    fill = list(crime_period = 0)
  ) %>%
  filter(!geolevel2_alt == "000NA") %>%
  left_join(census_sf_geo2_2020_data, by = c("geolevel2_alt" = "geolevel2_alt")) %>%
  select(-geo2_mx2020, -geolevel2_alt) %>%
  group_by(geo2_mx, year_census) %>%
  summarise(crime_period = sum(crime_period), .groups = "drop") 

saveRDS(crime_data_census, "Data/temp/crime_data_census.rds")
  
