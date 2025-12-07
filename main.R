library(tidyverse)
library(data.table)
library(sf)
library(purrr)
library(terra)
library(ipumsr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)      # For reading NetCDF files
# library(raster)     # For raster data manipulation
library(viridis)    # For color scales
library(data.table)


######################## Census Data ########################

#### Load Census Data for 20xx ####

census_ddi <- read_ipums_ddi("Data/Migration Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi) 

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


# Convert to data.table because of memory issues when working with data.frame
census_dt <- as.data.table(census_data)

# Process efficiently
census_data_controls <- census_dt[, .(
  year, geolevel2, incearn, popdensgeo2, age, sex, lit, yrschool, mig2_5_mx
)][, `:=`(
  incearn = fifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
  age = fifelse(age == 999, NA_real_, age),
  sex = fifelse(sex == 9, NA_real_, fifelse(sex == 2, 1, 0)),
  lit = fifelse(lit %in% c(0, 9), NA_real_, fifelse(lit == 2, 1, 0))
)][, .(
  mean_inc = mean(incearn, na.rm = TRUE),
  popdensgeo2 = mean(popdensgeo2, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE),
  rate_female = mean(sex, na.rm = TRUE),
  rate_literacy = mean(lit, na.rm = TRUE),
  mean_yrschool = mean(as.integer(yrschool), na.rm = TRUE)
), by = geolevel2]





census_data_controls <- census_data %>%
  select(year, geolevel2, incearn, popdensgeo2, age, sex, lit, yrschool, mig2_5_mx) %>%
  mutate(
    # Recode data to indicate non-valid entries as NAs - do all at once
    incearn = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
    age = ifelse(age == 999, NA_real_, age),
    sex = ifelse(sex == 9, NA_real_, ifelse(sex == 2, 1, 0)),
    lit = ifelse(lit %in% c(0, 9), NA_real_, ifelse(lit == 2, 1, 0)),
    yrschool = as.integer(yrschool)
  ) %>%
  group_by(geolevel2) %>%
  summarise(
    mean_inc = mean(incearn, na.rm = TRUE),
    popdensgeo2 = mean(popdensgeo2, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    rate_female = mean(sex, na.rm = TRUE),
    rate_literacy = mean(lit, na.rm = TRUE),
    mean_yrschool = mean(yrschool, na.rm = TRUE),
    .groups = "drop"
  )





#### Load Spatial Data ####

census_sf_geo1_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo1_mx1960_2020/geo1_mx1960_2020.shp")
census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")

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

# Fix names, add consistent municipality codes, extract year, group to get one entry per year per municipality
disaster_data <- disaster_raw %>%
  janitor::clean_names() %>%
  # Adapt clave municipio column to the IPUMS naming standard
  mutate(geolevel2 = ifelse(as.numeric(clave_municipio) < 10000, paste0("48400", as.character(clave_municipio)), paste0("4840", as.character(clave_municipio)))) %>%
  # Extract the year out of the fecha_inicio column
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year_start"), cols_remove = FALSE) %>%
  separate_wider_delim(fecha_fin, "/", names = c(NA, NA, "year_end"), cols_remove = FALSE) %>%
  select(-estado, -clave_estado, -municipio, -clave_municipio, -fecha_inicio, -fecha_fin)

disaster_yearly <- disaster_data %>%
  group_by(year_start, geolevel2) %>%
  summarise(
    # Hier noch counter für Arten / und maybe Länge der Disaster hinzufügen
    num_disasters_yearly = n(),
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
mun_nodes <- census_sf_geo2 %>%
  # Delete pre-existing num_disasters_total column to avoid weird mergin issues when rerunning the code
  { if("num_disasters_total" %in% colnames(.)) select(-num_disasters_total) else . } %>%
  ### Add simple disaster indicator to node df ###
  left_join(
    disaster_total, 
    by = c("geolevel2" = "geolevel2")
  ) %>%
  ### Add control variables ###
  left_join(
    census_data_controls,
    by = c("geolevel2" = "geolevel2")
  )

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
  group_by(year, geolevel2, mig2_5_mx) %>%
  summarise(
    num_migrants = n(),
    .groups = "drop"
  )

# Add migration information to nodes

# Data indicating where people went
od_dest <- od_edges %>%
  group_by(year, geolevel2) %>%
  summarise(
    yearly_immigration = sum(num_migrants),
    .groups = "drop"
  ) %>%
  # Join data from the INEGI shapefiles
  full_join(
    mun_nodes,
    by = c("geolevel2" = "geolevel2")
  )

mun_nodes <- od_dest

########### Municipality Plots #############

mun_nodes_sf <- st_as_sf(mun_nodes)

disaster_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = num_disasters_total)) +
  scale_fill_distiller(palette="RdYlGn")

popdensgeo_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = popdensgeo2)) 

########### Migration Plots #############


# Destination 

od_dest_sf <- st_as_sf(od_dest) # Convert to sf object

destination_plot <- ggplot(od_dest) +
  geom_sf(aes(fill = yearly_immigration))

# Data indicating from where people came
od_origin <- od_edges %>%
  group_by(mig2_5_mx) %>%
  summarise(
    yearly_emigration = sum(num_migrants),
    .groups = "drop"
  ) %>%
  # Join data from the INEGI shapefiles
  left_join(
    mun_nodes,
    by = c("origin_id" = "mun_id")
  ) %>%
  left_join(
    disaster_total, 
    by = c("origin_id" = "mun_id")) %>%
  st_as_sf() # Convert to sf object

origin_plot <- ggplot(od_origin) +
  geom_sf(aes(fill = total_emigration))

mod1 = lm(total_emigration ~ num_disasters_total.y, data = od_origin)