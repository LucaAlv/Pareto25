######################## Constructing nodes and edges (OD) data frames ########################

#### Load preprocessed Data ####

census_data <- readRDS("Data/temp/census_data.rds")
census_data_controls <- readRDS("Data/temp/census_data_controls.rds")
disaster_indicator <- readRDS("Data/temp/disaster_data.rds")

#### Load Spatial Data ####

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

#### Loading disaster data ####


#### Aggregating disaster data ####

disaster_period <- disaster_data %>%
  group_by(year_period, geolevel2) %>%
  summarise(
    num_disasters_period = n(),
    .groups = "drop"
  )

disaster_indicator <- disaster_data %>%
  group_by(year_census, geolevel2) %>%
  summarise(
    num_disasters_period = n(),
    .groups = "drop"
  )

disaster_total <- disaster_data %>%
  group_by(geolevel2) %>%
  summarise(
    num_disasters_total = n(),
    .groups = "drop"
  )



# Nodes Data Frame - one entry per municipality per year
mun_nodes_nomig <- census_sf_geo2 %>%
  # Delete pre-existing num_disasters_total column to avoid weird mergin issues when rerunning the code
  { if("num_disasters_period" %in% colnames(.)) select(-num_disasters_period) else . } %>%
  ### Add simple disaster indicator to node df ###
  full_join(
    disaster_indicator, 
    by = c("geolevel2" = "geolevel2")
  ) %>%
  ### Add control variables ###
  full_join(
    census_data_controls,
    by = c("geolevel2" = "geolevel2", "year_census" = "year")
  )

rm(census_sf_geo2)
gc()

# Edges Data Frame: Origin-destination (OD) matrix
od_edges <- census_data %>%
  select(year, hhwt, perwt, geolevel1, geolevel2, geomig1_5, mig1_5_mx, mig2_5_mx) %>%
  mutate(mig2_5_mx = as.character(mig2_5_mx)) %>%
  # For every state 4840xx998 decodes the situation where state is known but municipality is not. 484097997 decodes migration abroad, 484097998 and 484097999 and NAs
  mutate(
    mig2_5_mx = if_else(
      mig2_5_mx %in% c(
      "484001998", "484002998", "484003998", "484004998", "484005998", "484006998",
      "484007998", "484008998", "484009998", "484010998", "484011998", "484012998",
      "484013998", "484014998", "484015998", "484016998", "484017998", "484018998",
      "484019998", "484020998", "484021998", "484022998", "484023998", "484024998",
      "484025998", "484026998", "484027998", "484028998", "484029998", "484030998",
      "484098997", "484098998", "484098999", "484099999"
      ),
      NA_character_,
      mig2_5_mx
      )
  ) %>%
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

# Add migration information to nodes

# Data indicating where people went
# Each row gives the amount of people who migrated TO geolevel2 in year_census
od_dest <- od_edges %>%
  group_by(year_census, geolevel2) %>%
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
    net_migration = period_immigration - period_emmigration,
    immigration_rate = period_immigration / mun_pop,
    emmigration_rate = period_emmigration / mun_pop
  )

saveRDS(mun_nodes, "Data/temp/mun_nodes.rds")
cat("Loaded and saved municipality node data\n")
rm(mun_nodes_nomig)
gc()

state_nodes_postprocess <- mun_nodes %>%
  st_drop_geometry() %>%
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
    net_migration = sum(net_migration),
    mean_immigration_rate = mean(immigration_rate, na.rm = TRUE),
    mean_emmigration_rate = mean(emmigration_rate, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(mun_nodes, "Data/temp/state_nodes_postprocess.rds")
cat("Loaded and saved state node postprocess data\n")
cat("Finished Succressfully")
gc()