######################## Disaster Indicator ########################

library(tidyverse)
library(ipumsr)
library(sf)

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
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year_start"), cols_remove = TRUE) %>%
  separate_wider_delim(fecha_fin, "/", names = c(NA, NA, "year_end"), cols_remove = TRUE) %>%
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
      year_start %in% 1995:2000 ~ 2000, # Relevant for Census in 2000
      year_start %in% 2001:2005 ~ 2005, 
      year_start %in% 2006:2010 ~ 2010, # Relevant for Census in 2010
      year_start %in% 2011:2015 ~ 2015, # Relevant for Census in 2015
      year_start %in% 2016:2020 ~ 2020, # Relevant for Census in 2020
      year_start %in% 2021:2025 ~ 2025
    ),
    phenomenon_type = case_when(
      tipo_fenomeno == "Lluvias" ~ "Rainfall",
      tipo_fenomeno == "Sequía" ~ "Drought",
      tipo_fenomeno == "Ciclón Tropical" ~ "Tropical Cyclone",
      tipo_fenomeno == "Sismo" ~ "Earthquake",
      tipo_fenomeno == "Marea Roja" ~ "Red Tide",
      tipo_fenomeno == "Bajas Temperaturas" ~ "Low Temperatures",
      tipo_fenomeno == "Deslave" ~ "Landslide",
      tipo_fenomeno == "Nevadas, Heladas, Granizadas" ~ "Snowfalls, Frosts, Hail",
      tipo_fenomeno == "Inundación" ~ "Flood",
      tipo_fenomeno == "Fuertes Vientos" ~ "Strong Winds",
      tipo_fenomeno == "Incendio Forestal" ~ "Forest Fire",
      tipo_fenomeno == "Tornado" ~ "Tornado",
      tipo_fenomeno == "Granizadas, Nevadas, Heladas" ~ "Hail, Snowfalls, Frosts",
      tipo_fenomeno == "Heladas, Granizadas, Nevadas" ~ "Frosts, Hail, Snowfalls",
      tipo_fenomeno == "Actividad Volcánica" ~ "Volcanic Activity",
      tipo_fenomeno == "Deslizamiento" ~ "Landslide",
      tipo_fenomeno == "Granizadas" ~ "Hail",
      tipo_fenomeno == "Tormenta Severa" ~ "Severe Storm",
      tipo_fenomeno == "Nevadas" ~ "Snowfalls",
      tipo_fenomeno == "Heladas" ~ "Frosts",
      tipo_fenomeno == "Hundimiento" ~ "Subsidence",
      tipo_fenomeno == "Temperatura Extrema" ~ "Extreme Temperature",
      tipo_fenomeno == "Nevadas, Granizadas, Heladas" ~ "Snowfalls, Hail, Frosts",
      tipo_fenomeno == "Sequía, Heladas" ~ "Drought, Frosts",
      tipo_fenomeno == "Bajas Temperaturas, Ciclón Tropical" ~ "Low Temperatures, Tropical Cyclone",
      tipo_fenomeno == "Nevadas, Heladas" ~ "Snowfalls, Frosts",
      tipo_fenomeno == "Ciclón Tropical, Lluvias" ~ "Tropical Cyclone, Rainfall",
      tipo_fenomeno == "Lluvias, Ciclón Tropical" ~ "Rainfall, Tropical Cyclone",
      tipo_fenomeno == "Inundación, Ciclón Tropical" ~ "Flood, Tropical Cyclone",
      tipo_fenomeno == "Fuertes Vientos, Ciclón Tropical" ~ "Strong Winds, Tropical Cyclone",
      tipo_fenomeno == "Lluvias, Inundación" ~ "Rainfall, Flood",
      TRUE ~ tipo_fenomeno  # Fallback for any unmatched
    )
  )

saveRDS(disaster_data, "Data/temp/disaster_data.rds")
cat("Processed and saved Disaster Data\n")
rm(disaster_raw)
gc()

#### Aggregating disaster data and adding columns for each disaster type ####
cat("Aggregating disaster data\n")

disaster_indicator <- disaster_data %>%
  # Filter out years for which there is no migration information
  filter(!(year_census == 2005)) %>%
  filter(!(year_census == 2025)) %>%
  group_by(year_census, geolevel2, phenomenon_type) %>%
  summarise(
    num_disasters_period = n(),
    .groups = "drop"
  ) %>%
  # Add one column for each type of disaster
  pivot_wider(
    names_from = phenomenon_type,
    values_from = num_disasters_period,
    values_fill = 0,  # Fill NAs with 0
    names_prefix = "count_"
  ) %>%
  janitor::clean_names() %>%
  mutate(
    # Add all ambiguous columns to clear indicators. Note that this will lead to an increase of total events. 
    count_snowfalls = count_snowfalls + count_snowfalls_frosts + count_snowfalls_frosts_hail + count_hail_snowfalls_frosts + count_frosts_hail_snowfalls,
    count_frosts = count_frosts + count_snowfalls_frosts + count_snowfalls_frosts_hail + count_hail_snowfalls_frosts + count_frosts_hail_snowfalls + count_drought_frosts,
    count_low_temperatures = count_low_temperatures + count_low_temperatures_tropical_cyclone,
    count_tropical_cyclone = count_tropical_cyclone + count_low_temperatures_tropical_cyclone + count_rainfall_tropical_cyclone + count_tropical_cyclone_rainfall + count_strong_winds_tropical_cyclone + count_flood_tropical_cyclone,
    count_hail = count_hail + count_snowfalls_frosts_hail + count_hail_snowfalls_frosts + count_frosts_hail_snowfalls,
    count_rainfall = count_rainfall + count_rainfall_tropical_cyclone + count_tropical_cyclone_rainfall + count_rainfall_flood,
    count_strong_winds = count_strong_winds + count_strong_winds_tropical_cyclone,
    count_drought = count_drought + count_drought_frosts,
    count_flood = count_flood + count_flood_tropical_cyclone + count_rainfall_flood,
    # Add total count per mun per year
    num_disasters_period = rowSums(select(., starts_with("count_")))
    ) %>%
    # Remove ambiguous columns
    select(-count_snowfalls_frosts, -count_low_temperatures_tropical_cyclone, -count_snowfalls_frosts_hail, -count_rainfall_tropical_cyclone, -count_tropical_cyclone_rainfall, -count_hail_snowfalls_frosts, -count_strong_winds_tropical_cyclone, -count_frosts_hail_snowfalls, -count_drought_frosts, -count_flood_tropical_cyclone, -count_rainfall_flood)

saveRDS(disaster_indicator, "Data/temp/disaster_indicator.rds")
cat("Processed and saved Disaster Indicator Data\n")