######################## Constructing nodes and edges (OD) data frames ########################

library(tidyverse)
library(ipumsr)
library(sf)
library(purrr)
library(terra)

#### Load preprocessed Data ####
cat("Load preprocessed Data\n")

census_data <- read_csv("Data/temp/census_data.csv")
cat("Loaded census data\n")
census_data_controls <- readRDS("Data/temp/census_data_controls.rds")
cat("Loaded census cotrols data\n")
disaster_indicator <- readRDS("Data/temp/disaster_indicator.rds")
cat("Loaded disaster indicator data\n")

#### Load Spatial Data ####
cat("Load Spatial Data\n")

census_sf_geo1_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo1_mx1960_2020/geo1_mx1960_2020.shp")
census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")
cat("Loaded spatial data\n")

census_sf_geo1 <- census_sf_geo1_raw %>% 
  select(ADMIN_NAME, GEOLEVEL1, geometry) %>%
  janitor::clean_names()

rm(census_sf_geo1_raw)
gc()

census_sf_geo2 <- census_sf_geo2_raw %>%
  select(ADMIN_NAME, GEOLEVEL2, geometry) %>%
  janitor::clean_names() %>%
  # There are some municipalities with empty geometry - filter them out
  filter(!st_is_empty(geometry))

rm(census_sf_geo2_raw)
gc()

#### Generating nodes data frame ####
cat("Generating nodes data frame\n")

expanded_years <- expand_grid(
  geolevel2 = unique(census_sf_geo2$geolevel2),
  year_census = c(2000, 2010, 2015, 2020)
)

# Nodes Data Frame - one entry per municipality per year

mun_nodes_nomig <- census_sf_geo2 %>%
  # Delete pre-existing num_disasters_total column to avoid weird mergin issues when rerunning the code
  { if("num_disasters_period" %in% colnames(.)) select(-num_disasters_period) else . } %>%
  right_join(expanded_years, by = "geolevel2") %>%
  ### Add simple disaster indicator to node df ###
  left_join(
    disaster_indicator,
    by = c("geolevel2" = "geolevel2", "year_census" = "year_census")
  ) %>%
  ### Add control variables ###
  left_join(
    census_data_controls,
    by = c("geolevel2" = "geolevel2", "year_census" = "year")
  )

rm(census_sf_geo2)
gc()

#### Generating edges data frame ####
cat("Generating edges data frame\n")

# Edges Data Frame: Origin-destination (OD) matrix
od_edges <- census_data %>%
  select(year, hhwt, perwt, geolevel1, geolevel2, geomig1_5, mig1_5_mx, mig2_5_mx) %>%
  mutate(mig2_5_mx = as.character(mig2_5_mx)) %>%
  filter(!is.na(mig2_5_mx)) %>%
  filter(geolevel2 != mig2_5_mx) %>%
  mutate(
    year_census = case_when(
      year %in% 1996:2000 ~ 2000, # Relevant for Census in 2000
      year %in% 2001:2005 ~ 2005, 
      year %in% 2006:2010 ~ 2010, # Relevant for Census in 2010
      year %in% 2011:2015 ~ 2015, # Relevant for Census in 2015
      year %in% 2016:2020 ~ 2020, # Relevant for Census in 2020
      year %in% 2021:2025 ~ 2025
    )
  ) %>%
  group_by(year_census, geolevel2, mig2_5_mx) %>%
  summarise(
    # This is the number of people who moved from mig2_5_mx to geolevel2 between year_census - 5 and year_census
    num_migrants = sum(perwt),
    .groups = "drop"
  )

saveRDS(od_edges, "Data/temp/od_edges.rds")
cat("Loaded and saved edges data\n")
rm(census_data)
gc()

#### Add migration information to nodes ####
cat("Add migration information to nodes\n")

# Data indicating where people went
# Each row gives the amount of people who migrated TO geolevel2 in year_census
od_dest <- od_edges %>%
  group_by(year_census, geolevel2) %>%
  mutate(geolevel2 = as.character(geolevel2)) %>%
  summarise(
    period_immigration = sum(num_migrants),
    .groups = "drop"
  )

# Each row gives the amount of people who migrated FROM mig2_5_mx in year_census
od_orig <- od_edges %>%
  group_by(year_census, mig2_5_mx) %>%
  summarise(
    period_emmigration = sum(num_migrants),
    .groups = "drop"
  ) 

rm(od_edges)
gc()

# Merge and add net migration information
mun_nodes <- mun_nodes_nomig %>%
  full_join(od_dest, by = c("geolevel2" = "geolevel2", "year_census" = "year_census")) %>%
  full_join(od_orig, by = c("geolevel2" = "mig2_5_mx", "year_census" = "year_census")) %>%
  filter(!year_census == 2005) %>%
  # Assuming zero migration where I have no data !Change this later!
  mutate(
    net_immigration = period_immigration - period_emmigration,
    net_immigration_rate = net_immigration / mun_pop,
    immigration_rate = period_immigration / mun_pop,
    emmigration_rate = period_emmigration / mun_pop
  )

saveRDS(mun_nodes, "Data/temp/mun_nodes.rds")
cat("Loaded and saved municipality node data\n")
rm(mun_nodes_nomig)
gc()

# State level aggregation for sanity checks
state_nodes_postprocess <- mun_nodes %>%
  st_drop_geometry() %>%
  mutate(geolevel1 = as.character(geolevel1)) %>%
  group_by(geolevel1, year_census) %>%
  summarise(
    num_disasters_period = sum(num_disasters_period),
    mean_inc = mean(mean_inc, na.rm = TRUE),
    mean_popdensgeo2 = mean(popdensgeo2, na.rm = TRUE),
    mean_age = mean(mean_age, na.rm = TRUE),
    mean_rate_female = mean(rate_female, na.rm = TRUE),
    mean_rate_literacy = mean(rate_literacy, na.rm = TRUE),
    mean_yrschool = mean(mean_yrschool, na.rm = TRUE),
    state_pop = sum(mun_pop),
    period_immigration = sum(period_immigration),
    period_emmigration = sum(period_emmigration),
    net_immigration = sum(net_immigration),
    mean_immigration_rate = mean(immigration_rate, na.rm = TRUE),
    mean_emmigration_rate = mean(emmigration_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(census_sf_geo1, by = c("geolevel1" = "geolevel1"))

saveRDS(state_nodes_postprocess, "Data/temp/state_nodes_postprocess.rds")
cat("Loaded and saved state node postprocess data\n")
cat("Finished Succressfully\n")
gc()