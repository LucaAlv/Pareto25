######################## Disaster Indicator ########################

library(tidyverse)
library(ipumsr)
library(lubridate)

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
# Read in like this to avoid weird broken accented characters / encoding errors
temp <- readLines("Data/Weather Data/Atlas-Disaster-Indicator/declaratorias24112025152623.csv", encoding = "ISO-8859-1", warn = FALSE)
temp <- iconv(temp, from = "ISO-8859-1", to = "UTF-8", sub = "")
disaster_raw <- read_csv(paste(temp, collapse = "\n"))
cat("Loaded Disaster Indicator Data\n")

# Fix names, add consistent municipality codes, extract year 

# Create name translation table

phenomenon_table <- tribble(
  ~tipo_fenomeno, ~phenomenon_type,
  "Lluvias","Rainfall",
  "Sequía","Drought",
  "Ciclón Tropical","Tropical Cyclone",
  "Sismo","Earthquake",
  "Marea Roja","Red Tide",
  "Bajas Temperaturas","Low Temperatures",
  "Deslave","Landslide",
  "Nevadas, Heladas, Granizadas","Snowfalls, Frosts, Hail",
  "Inundación","Flood",
  "Fuertes Vientos","Strong Winds",
  "Incendio Forestal","Forest Fire",
  "Tornado","Tornado",
  "Granizadas, Nevadas, Heladas", "Hail, Snowfalls, Frosts",
  "Heladas, Granizadas, Nevadas", "Frosts, Hail, Snowfalls",
  "Actividad Volcánica", "Volcanic Activity",
  "Deslizamiento", "Landslide",
  "Granizadas", "Hail",
  "Tormenta Severa", "Severe Storm",
  "Nevadas", "Snowfalls",
  "Heladas", "Frosts",
  "Hundimiento", "Subsidence",
  "Temperatura Extrema", "Extreme Temperature",
  "Nevadas, Granizadas, Heladas", "Snowfalls, Hail, Frosts",
  "Sequía, Heladas", "Drought, Frosts",
  "Bajas Temperaturas, Ciclón Tropical", "Low Temperatures, Tropical Cyclone",
  "Nevadas, Heladas", "Snowfalls, Frosts",
  "Ciclón Tropical, Lluvias", "Tropical Cyclone, Rainfall",
  "Lluvias, Ciclón Tropical", "Rainfall,Tropical Cyclone",
  "Inundación, Ciclón Tropical", "Flood,Tropical Cyclone",
  "Fuertes Vientos, Ciclón Tropical", "StrongWinds, Tropical Cyclone",
  "Lluvias, Inundación", "Rainfall, Flood"
)

disaster_data <- disaster_raw %>%
  janitor::clean_names() %>%
  # Adapt clave municipio column to the IPUMS naming standard
  left_join(phenomenon_table, by = "tipo_fenomeno") %>%
  left_join(census_sf_geo2_2020_data, by = c("clave_municipio" = "geo2_mx2020")) %>%
  mutate(
    geolevel2 = as.character(geo2_mx),
    # Translation to english
    phenomenon_type = coalesce(phenomenon_type, "Unknown")
  )

cat("There are ", sum(is.na(disaster_data$geolevel2)), "NAs in geolevel 2 in the disaster dataset\n")

saveRDS(disaster_data, "Data/temp/disaster_data.rds")
cat("Processed and saved Disaster Data\n")
rm(disaster_raw)
gc()

######## Prepare disaster data for movement data ########

disaster_data_fb <- disaster_data %>%
  # Convert to correct date format and control for missing end date (e.g. event only lasted one day)
  mutate(
    date_start = dmy(fecha_inicio),
    date_end = dmy(fecha_fin),
    date_end = if_else(is.na(date_end), date_start, date_end),
    date_end = if_else(!is.na(date_start) & !is.na(date_end) & date_end < date_start,
                       date_start, date_end),
    # Convert date to first of each month
    month_start = floor_date(date_start, unit = "month"),
    month_end   = floor_date(date_end,   unit = "month")
  ) %>%
  # drop rows we cannot place in time
  filter(!is.na(month_start), !is.na(month_end)) %>%
  # expand to one row per month the event is active in
  rowwise() %>%
  mutate(month = list(seq(month_start, month_end, by = "1 month"))) %>%
  ungroup() %>%
  unnest(month) %>%
  # Reformat to YYYY-MM notation for sorting
  mutate(
    month_year = format(month, "%Y-%m")
  ) %>%
  # filter using the expanded month (not year_start)
  filter(year(month) %in% 2020:2025) %>%
  group_by(month_year, geolevel2, phenomenon_type) %>%
  summarise(num_disasters_month = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = phenomenon_type,
    values_from = num_disasters_month,
    values_fill = 0,
    names_prefix = "count_"
  ) %>%
  janitor::clean_names() %>%
  mutate(total_disasters_month = rowSums(select(., starts_with("count_"))))

saveRDS(disaster_data_fb, "Data/temp/disaster_data_fb.rds")
cat("Processed and saved Disaster Indicator Data for FB Movement\n")

######## Prepare disaster data for census ########

#### Extract years ####

disaster_data_census <- disaster_data %>%
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year_start"), cols_remove = TRUE) %>%
  separate_wider_delim(fecha_fin, "/", names = c(NA, NA, "year_end"), cols_remove = TRUE) %>%
  mutate(
    year_start = as.integer(year_start),
    year_end   = as.integer(year_end),
    year_census = case_when(
      year_start >= 1996 & year_start <= 2000 ~ 2000,
      year_start >= 2001 & year_start <= 2005 ~ 2005,
      year_start >= 2006 & year_start <= 2010 ~ 2010,
      year_start >= 2011 & year_start <= 2015 ~ 2015,
      year_start >= 2016 & year_start <= 2020 ~ 2020,
      year_start >= 2021 & year_start <= 2025 ~ 2025,
      TRUE ~ NA_integer_
    ),
    year_period = case_when(
      year_census == 2000 ~ "1996-2000",
      year_census == 2005 ~ "2001-2005",
      year_census == 2010 ~ "2006-2010",
      year_census == 2015 ~ "2011-2015",
      year_census == 2020 ~ "2016-2020",
      year_census == 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(year_census))

saveRDS(disaster_data_census, "Data/temp/disaster_data_census.rds")
#### Aggregating disaster data and adding columns for each disaster type ####
cat("Aggregating disaster data\n")


disaster_data_census_period_long <- disaster_data_census %>%
  group_by(year_census, geolevel2, phenomenon_type) %>%
  summarise(
    total_disasters_phenomenon_period = n(),
    .groups = "drop"
  ) 
  
disaster_data_census_period <- disaster_data_census_period_long %>%
  # Add one column for each type of disaster
  # This is basically the same as grouping by year_census and geolevel2
  pivot_wider(
    names_from = phenomenon_type,
    values_from = total_disasters_phenomenon_period,
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
    ) %>%
    # Remove ambiguous columns
    select(-count_snowfalls_frosts, -count_low_temperatures_tropical_cyclone, -count_snowfalls_frosts_hail, -count_rainfall_tropical_cyclone, -count_tropical_cyclone_rainfall, -count_hail_snowfalls_frosts, -count_strong_winds_tropical_cyclone, -count_frosts_hail_snowfalls, -count_drought_frosts, -count_flood_tropical_cyclone, -count_rainfall_flood) %>%
    mutate(    
      # Add total count per mun per year - this will be higher than the counter above
      total_unique_disasters_period = rowSums(select(., starts_with("count_")))
    ) %>%
    left_join(
      disaster_data_census_period_long %>%
        group_by(year_census, geolevel2) %>%
        summarise(total_disasters_period = sum(total_disasters_phenomenon_period), .groups = "drop"),
      by = c("year_census", "geolevel2")
    )

saveRDS(disaster_data_census_period, "Data/temp/disaster_data_census_period.rds")
cat("Processed and saved Disaster Indicator Data for Census\n")