library(tidyverse)
library(data.table)
library(ipumsr)
library(gpkg)
library(sf)

# Movement Data
movement_data_raw <- read_csv("Data/Meta Movement Distribution/movement-distribution-data-for-good-at-meta_2023-05-01_2023-06-01_csv/1922039342088483_2023-05-01.csv")

# Calculate average for each month here

# GADM Data
gadm4 <- st_read("Data/GADM/gadm41_MEX.gpkg", layer = "ADM_ADM_2")
gadm3 <- st_read("Data/GADM/gadm36_MEX.gpkg", layer = "gadm36_MEX_2")

# IPUMS shape data
ipums_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp") 

ipums_sf_geo2 <- ipums_sf_geo2_raw %>%
  select(ADMIN_NAME, GEOLEVEL2, geometry) %>% 
  janitor::clean_names() %>%
  # There are some municipalities with empty geometry - filter them out
  filter(!st_is_empty(geometry))

# Disaster data
census_data_controls <- readRDS("Data/temp/census_data_controls.rds")
cat("Loaded census cotrols data\n")
disaster_data_fb <- readRDS("Data/temp/disaster_data_fb.rds")
cat("Loaded disaster indicator data\n")

#### Create crosswalk between gadm and ipums sf

## Crosswalk ##

# Ensure both are in same CRS
if (st_crs(gadm3) != st_crs(ipums_sf_geo2)) {
  ipums_sf_geo2 <- st_transform(ipums_sf_geo2, st_crs(gadm3))
  cat("CRS mismatch detected. Transforming IPUMS to GADM CRS...\n")
}

# Transform to equal-area projection for accurate area calculations
if (st_is_longlat(gadm3)) {
  cat("Geographic CRS detected. Transforming to equal-area projection for accurate area calculations...\n")
  target_crs <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  gadm3 <- st_transform(gadm3, target_crs)
  ipums_sf_geo2 <- st_transform(ipums_sf_geo2, target_crs)
}

# Calculate original areas
gadm3$gadm_area <- st_area(gadm3)
ipums_sf_geo2$ipums_area <- st_area(ipums_sf_geo2)

# Perform intersection
cat("Performing intersection...\n")
intersection <- st_intersection(
  gadm3 %>% select(GID_2, gadm_area),
  ipums_sf_geo2 %>% select(geolevel2, ipums_area)
)

# Calculate overlap area and percentages
crosswalk <- intersection %>%
  mutate(overlap_area = st_area(geom)) %>%
  st_drop_geometry() %>%
  mutate(
    overlap_pct_gadm = as.numeric(overlap_area / gadm_area * 100),
    overlap_pct_ipums = as.numeric(overlap_area / ipums_area * 100),
    overlap_area_km2 = as.numeric(overlap_area) / 1e6
  ) %>%
  select(
    gadm_id = GID_2, 
    ipums_id = geolevel2,
    overlap_area_km2,
    overlap_pct_gadm,
    overlap_pct_ipums
  ) %>%
  filter(overlap_pct_gadm >= 1 | overlap_pct_ipums >= 1) %>%
  arrange(gadm_id, desc(overlap_pct_gadm))

# Create one-to-one mapping: each GADM gets its best-matching IPUMS
gadm_to_ipums <- crosswalk %>%
  group_by(gadm_id) %>%
  slice_max(overlap_pct_gadm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(gadm_id, ipums_id, overlap_pct_gadm, overlap_pct_ipums)

# Check for matches with very low overlap
low_overlap <- crosswalk %>%
  filter(overlap_pct_gadm < 90 & overlap_pct_ipums < 90)

## Match it ##

movement_data_matched <- movement_data_raw %>%
  filter(country == "MEX") %>%
  pivot_wider(
    names_from = home_to_ping_distance_category,
    values_from = distance_category_ping_fraction,
    names_glue  = "dist_{home_to_ping_distance_category}"
  ) %>%
  janitor::clean_names() %>%
  mutate(month_year = format(ds, "%m/%Y")) %>%
  group_by(gadm_id, month_year) %>%
  summarise(
    dist_0 = mean(dist_0, na.rm = TRUE),
    dist_0_10 = mean(dist_0_10, na.rm = TRUE),
    dist_10_100 = mean(dist_10_100, na.rm = TRUE),
    dist_100 = mean(dist_100, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # As each gadm matches multiple ipums and the other way around there will be a many-to-many relationship
  # When merged with other data this needs to be aggregated properly (probably group by gadm)
  left_join(crosswalk, by = c("gadm_id" = "gadm_id"), relationship = "many-to-many")

#### Merge with census and disaster data ####

movement_data <- movement_data_matched %>%
  left_join(disaster_data_fb, by = c("ipums_id" = "geolevel2", "month_year" = "month_year"), relationship = "many-to-many") %>%
  group_by(gadm_id, month_year) %>%
  summarise(
    # grouping gets rid of the situation where one gadm matches many ipums
    # weighing with overlap percentage controls for the situation where one gadm is only a part of one ipums 
    count_drought = sum(count_drought * overlap_pct_ipums / 100, na.rm = TRUE),
    count_forest_fire = sum(count_forest_fire * overlap_pct_ipums / 100, na.rm = TRUE),
    count_rainfall = sum(count_rainfall * overlap_pct_ipums / 100, na.rm = TRUE),
    count_tropical_cyclone = sum(count_tropical_cyclone * overlap_pct_ipums / 100, na.rm = TRUE),
    count_frosts = sum(count_frosts * overlap_pct_ipums / 100, na.rm = TRUE),
    count_flood = sum(count_flood * overlap_pct_ipums / 100, na.rm = TRUE),
    count_earthquake = sum(count_earthquake * overlap_pct_ipums / 100, na.rm = TRUE),
    count_snowfalls = sum(count_snowfalls * overlap_pct_ipums / 100, na.rm = TRUE),
    count_tornado = sum(count_tornado * overlap_pct_ipums / 100, na.rm = TRUE),
    count_landslide = sum(count_landslide * overlap_pct_ipums / 100, na.rm = TRUE),
    count_hail = sum(count_hail * overlap_pct_ipums / 100, na.rm = TRUE),       
    count_extreme_temperature = sum(count_extreme_temperature * overlap_pct_ipums / 100, na.rm = TRUE),
    total_disasters_month = sum(total_disasters_month * overlap_pct_ipums / 100, na.rm = TRUE),
    .groups = "drop"       
  )

saveRDS(movement_data, "Data/temp/movement_data")
