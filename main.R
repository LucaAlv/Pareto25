library(tidyverse)
library(sf)
library(purrr)
library(terra)
library(ipumsr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(fixest)


######################## Census Data ########################

#### Load Census Data ####

census_ddi <- read_ipums_ddi("Data/Migration Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi)

census_data_controls <- readRDS("Data/safe/census_data_controls.rds")

#### Formatting NAs correctly for geolevel2 ####
census_data <- census_data_raw %>%
  janitor::clean_names() %>%
  rename(geolevel2 = geolev2, geolevel1 = geolev1) %>%
  mutate(
    geolevel2 = as.character(geolevel2),
    geolevel2 = if_else(
      geolevel2 %in% c(
        "484001999", "484002999", "484003999", "484004999", "484005999", "484006999",
        "484007999", "484008999", "484009999", "484010999", "484011999", "484012999",
        "484013999", "484014999", "484015999", "484016999", "484017999", "484018999",
        "484019999", "484020999", "484021999", "484022999", "484023999", "484024999",
        "484025999", "484026999", "484027999", "484028999", "484029999", "484030999"
      ),
      NA_character_,
      geolevel2
    )
  )

#### Load Spatial Data ####

census_sf_geo1_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo1_mx1960_2020/geo1_mx1960_2020.shp")
census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")

census2 <- read_ipums_ddi("Data/Migration Data/ipumsi_00003.xml")
census2_raw <- read_ipums_micro(census2)

# This is the mapping of official 2020 codes used by the mexican government to the harmonized IPUMS geo2_mx codes
census2_ed <- census2_raw %>%
  janitor::clean_names() %>%
  group_by(geo2_mx, geo2_mx2020) %>%
  summarise(.groups = "drop")

census_sf_geo1 <- census_sf_geo1_raw %>% 
  select(ADMIN_NAME, GEOLEVEL1, geometry) %>%
  janitor::clean_names()

census_sf_geo2 <- census_sf_geo2_raw %>%
  select(ADMIN_NAME, GEOLEVEL2, geometry) %>%
  janitor::clean_names() %>%
  # There are some municipalities with empty geometry - filter them out
  filter(!st_is_empty(geometry))

######################## Disaster Indicator ########################

temp <- readLines("Data/Weather Data/Atlas-Disaster-Indicator/declaratorias24112025152623.csv", encoding = "ISO-8859-1", warn = FALSE)
temp <- iconv(temp, from = "ISO-8859-1", to = "UTF-8", sub = "")
disaster_raw <- read_csv(paste(temp, collapse = "\n"))

disaster_test <- disaster_raw %>%
  janitor::clean_names() %>%
  left_join(census2_ed, by = c("clave_municipio" = "geo2_mx2020")) %>%
  select(municipio, clave_municipio, geo2_mx)


# Fix names, add consistent municipality codes, extract year, group to get one entry per year per municipality
disaster_data <- disaster_raw %>%
  janitor::clean_names() %>%
  # Adapt clave municipio column to the IPUMS naming standard
  left_join(census2_ed, by = c("clave_municipio" = "geo2_mx2020")) %>%
  mutate(geolevel2 = as.character(geo2_mx)) %>%
  # Extract the year out of the fecha_inicio column
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year_start"), cols_remove = FALSE) %>%
  separate_wider_delim(fecha_fin, "/", names = c(NA, NA, "year_end"), cols_remove = FALSE) %>%
  mutate(
    year_period = case_when(
      year_start %in% 1995:1999 ~ "1995-2000", # Relevant for Census in 2000
      year_start %in% 2000:2004 ~ "2000-2005", 
      year_start %in% 2005:2009 ~ "2005-2010", # Relevant for Census in 2010
      year_start %in% 2010:2014 ~ "2010-2015", # Relevant for Census in 2015
      year_start %in% 2015:2019 ~ "2015-2020", # Relevant for Census in 2020
      year_start %in% 2020:2024 ~ "2020-2025"
    ),
    year_census = case_when(
      year_start %in% 1995:1999 ~ 2000, # Relevant for Census in 2000
      year_start %in% 2000:2004 ~ 2005, 
      year_start %in% 2005:2009 ~ 2010, # Relevant for Census in 2010
      year_start %in% 2010:2014 ~ 2015, # Relevant for Census in 2015
      year_start %in% 2015:2019 ~ 2020, # Relevant for Census in 2020
      year_start %in% 2020:2024 ~ 2025
    )
  ) %>%
  select(-estado, -clave_estado, -municipio, -fecha_inicio, -fecha_fin)

disaster_yearly <- disaster_data %>%
  group_by(year_start, geolevel2) %>%
  summarise(
    # Hier noch counter für Arten / und maybe Länge der Disaster hinzufügen
    num_disasters_yearly = n(),
    .groups = "drop"
  )

disaster_period <- disaster_data %>%
  group_by(year_period, geolevel2) %>%
  summarise(
    # Hier noch counter für Arten / und maybe Länge der Disaster hinzufügen
    num_disasters_period = n(),
    .groups = "drop"
  )

disaster_census <- disaster_data %>%
  group_by(year_census, geolevel2) %>%
  summarise(
    # Hier noch counter für Arten / und maybe Länge der Disaster hinzufügen
    num_disasters_period = n(),
    .groups = "drop"
  )

disaster_total <- disaster_data %>%
  group_by(geolevel2) %>%
  summarise(
    num_disasters_total = n(),
    .groups = "drop"
  )

######################## Constructing nodes and edges (OD) data frames ########################

# Nodes Data Frame
mun_nodes_nomig <- census_sf_geo2 %>%
  # Delete pre-existing num_disasters_total column to avoid weird mergin issues when rerunning the code
  { if("num_disasters_period" %in% colnames(.)) select(-num_disasters_period) else . } %>%
  ### Add simple disaster indicator to node df ###
  full_join(
    disaster_census, 
    by = c("geolevel2" = "geolevel2")
  ) %>%
  ### Add control variables ###
  full_join(
    census_data_controls,
    by = c("geolevel2" = "geolevel2", "year_census" = "year")
  )

gc()

# Edges Data Frame: Origin-destination (OD) matrix
od_edges <- census_data %>%
  select(year, hhwt, geolevel1, geolevel2, geomig1_5, mig1_5_mx, mig2_5_mx) %>%
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
      year %in% 1995:1999 ~ 2000, # Relevant for Census in 2000
      year %in% 2000:2004 ~ 2005, 
      year %in% 2005:2009 ~ 2010, # Relevant for Census in 2010
      year %in% 2010:2014 ~ 2015, # Relevant for Census in 2015
      year %in% 2015:2019 ~ 2020, # Relevant for Census in 2020
      year %in% 2020:2024 ~ 2025
    )
  ) %>%
  group_by(year_census, geolevel2, mig2_5_mx) %>%
  summarise(
    # This is the number of people who moved from mig2_5_mx to geolevel2
    num_migrants = n(),
    .groups = "drop"
  )

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

# Merge and add net migration information
mun_nodes <- mun_nodes_nomig %>%
  full_join(od_dest, by = c("geolevel2" = "geolevel2", "year_census" = "year_census")) %>%
  full_join(od_orig, by = c("geolevel2" = "mig2_5_mx", "year_census" = "year_census")) %>%
  # Assuming zero migration where I have no data !Change this later!
  mutate(
    period_immigration = coalesce(period_immigration, 0),
    period_emmigration = coalesce(period_emmigration, 0),
    net_migration = period_immigration - period_emmigration
  )

########### Municipality Plots #############

mun_nodes_sf <- st_as_sf(mun_nodes)

disaster_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = num_disasters_period)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by Municipality",
       x = "Municipality",
       y = "Number of Disasters")
ggsave("Plots/disaster_plot.png", disaster_plot)

popdensgeo_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = popdensgeo2)) 

########### Migration Plots #############


# Destination 

od_dest_sf <- st_as_sf(od_dest) # Convert to sf object

destination_plot <- ggplot(od_dest_sf) +
  geom_sf(aes(fill = yearly_immigration))

# Origin

od_orig_sf <- st_as_sf(od_orig)

origin_plot <- ggplot(od_orig_sf) +
  geom_sf(aes(fill = total_emigration))

######################## Regressions ########################



mod1 = lm(period_emmigration ~ num_disasters_period, data = mun_nodes)
mod2 = lm(period_immigration ~ num_disasters_period, data = mun_nodes)
mod3 = lm(net_migration ~ num_disasters_period, data = mun_nodes)

# TWFE model for out migration rate as function of simple disaster indicator 
model_out <- feols(
  out_rate ~ num_disasters_period |
    geolevel2 + year_census,
  data = panel,
  cluster = ~ geolevel2
)

summary(model_out)

# TWFE model for net migration rate as function of simple disaster indicator 
model_net <- feols(
  net_rate ~ num_disasters_period |
    geolevel2 + year_census,
  data = panel,
  cluster = ~ geolevel2
)

summary(model_net)

