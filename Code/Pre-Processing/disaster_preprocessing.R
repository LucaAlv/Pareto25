######################## Disaster Indicator ########################

library(tidyverse)
library(ipumsr)

#### Load Spatial Data for Disaster Indicator ####

# Mapping data
census_sf_geo2_2020_ddi <- read_ipums_ddi("Data/IPUMS/Census_Data/ipumsi_00003.xml")
census_sf_geo2_2020_data_raw <- read_ipums_micro(census_sf_geo2_2020_ddi)

# This is the mapping of official 2020 codes used by the mexican government to the harmonized IPUMS geo2_mx codes
# We need this to match the disaster data below to the ipums census data
census_sf_geo2_2020_data <- census_sf_geo2_2020_data_raw %>%
  janitor::clean_names() %>%
  group_by(geo2_mx, geo2_mx2020) %>%
  summarise(.groups = "drop")

cat("Loaded 2020 Census data and created mapping\n")
rm(census_sf_geo2_2020_data_raw)
gc()

#### Load Data for Disaster Indicator ####

temp <- readLines("Data/Weather Data/Atlas-Disaster-Indicator/declaratorias24112025152623.csv", encoding = "ISO-8859-1", warn = FALSE)
temp <- iconv(temp, from = "ISO-8859-1", to = "UTF-8", sub = "")
disaster_raw <- read_csv(paste(temp, collapse = "\n"))
cat("Loaded Disaster Indicator Data\n")

# Fix names, add consistent municipality codes, extract year 
disaster_data <- disaster_raw %>%
  janitor::clean_names() %>%
  # Adapt clave municipio column to the IPUMS naming standard
  left_join(census_sf_geo2_2020_data, by = c("clave_municipio" = "geo2_mx2020")) %>%
  mutate(geolevel2 = as.character(geo2_mx)) %>%
  # Extract the year out of the fecha_inicio column
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year_start"), cols_remove = FALSE) %>%
  separate_wider_delim(fecha_fin, "/", names = c(NA, NA, "year_end"), cols_remove = FALSE) %>%
  mutate(
    year_period = case_when(
      year_start %in% 1996:2000 ~ "1996-2000", # Relevant for Census in 2000
      year_start %in% 2001:2005 ~ "2001-2005", 
      year_start %in% 2006:2010 ~ "2006-2010", # Relevant for Census in 2010
      year_start %in% 2011:2015 ~ "2011-2015", # Relevant for Census in 2015
      year_start %in% 2016:2020 ~ "2016-2020", # Relevant for Census in 2020
      year_start %in% 2021:2025 ~ "2021-2025"
    ),
    year_census = case_when(
      # example: a disaster in 2000 is relevant for migration flows between 2000 and 2005 / the 2005 census data gives us information about something that happened in 2000
      year_start %in% 1996:2000 ~ 2000, # Relevant for Census in 2000
      year_start %in% 2001:2005 ~ 2005, 
      year_start %in% 2006:2010 ~ 2010, # Relevant for Census in 2010
      year_start %in% 2011:2015 ~ 2015, # Relevant for Census in 2015
      year_start %in% 2016:2020 ~ 2020, # Relevant for Census in 2020
      year_start %in% 2021:2025 ~ 2025
    )
  ) %>%
  select(-estado, -clave_estado, -municipio, -fecha_inicio, -fecha_fin)

saveRDS(disaster_data, "Data/temp/disaster_data.rds")
cat("Processed and saved Disaster Indicator Data\n")
rm(disaster_raw)
gc()