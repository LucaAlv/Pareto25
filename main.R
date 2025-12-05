library(tidyverse)
library(data.table)
library(sf)
library(purrr)
library(terra)
library(ipumsr)

######################## Census Data ########################

#### Load Census Data for 20xx ####
# NOTE: To load data, you must download both the extract's data and the DDI

census_ddi <- read_ipums_ddi("Data/Migration Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi)

census_sf_geo1 <- read_ipums_sf("Data/IPUMS/shapefiles/geo1_mx1960_2020/geo1_mx1960_2020.shp")




######################## Disaster Indicator ########################

temp <- readLines("Data/Weather Data/Atlas-Disaster-Indicator/declaratorias24112025152623.csv", encoding = "ISO-8859-1", warn = FALSE)
temp <- iconv(temp, from = "ISO-8859-1", to = "UTF-8", sub = "")
disaster_no_edit <- read_csv(paste(temp, collapse = "\n"))
# Fix names, add consistent municipality codes, extract year, group to get one entry per year per municipality
disaster <- disaster_no_edit %>%
  janitor::clean_names() %>%
  mutate(clave_municipio = sprintf("%05d", as.integer(clave_municipio))) %>%
  separate_wider_delim(fecha_inicio, "/", names = c(NA, NA, "year"), cols_remove = FALSE)

disaster_yearly <- disaster %>%
  group_by(year, clave_municipio) %>%
  summarise(
    # Hier noch counter für Arten / und maybe Länge der Disaster hinzufügen
    num_disasters_yearly = n(),
    .groups = "drop"
  )

disaster_total <- disaster %>%
  group_by(clave_municipio) %>%
  summarise(
    num_disasters_total = n(),
    .groups = "drop"
  )

######################## Constructing nodes and edges (OD) data frames ########################

# Nodes Data Frame
mun_nodes <- inegi_shape_data %>%
  select(mun_id = cvegeo, nomgeo, geometry) %>%
  full_join(
    disaster_yearly, 
    by = c("mun_id" = "clave_municipio")
  ) %>%
  mutate(
    num_disasters_yearly = as.character(num_disasters_yearly),
    num_disasters_yearly = na_if(num_disasters_yearly, "Invalid Number"),
    num_disasters_yearly = as.integer(num_disasters_yearly)
  ) %>%
  replace_na(list(num_disasters_yearly = 0L)) %>%
  full_join(
    disaster_total,
    by = c("mun_id" = "clave_municipio")
  )

# Edges Data Frame: Origin-destination (OD) matrix
od_edges <- census_ind %>%
  # Filter out people who didn't move in the past five years
  filter(origin_id != dest_id) %>%
  group_by(origin_id, dest_id) %>%
  summarise(
    num_migrants = n(),
    mage = mean(edad, na.rm = TRUE),
    msex = mean(sexo, na.rm = TRUE),
    .groups = "drop"
  )

# Data indicating the amount of disasters per municipality
disaster_total <- mun_nodes %>%
  group_by(mun_id) %>%
  summarise(
    num_disasters_total = sum(num_disasters_yearly),
    .groups = "drop"
  )

disaster_total_sf <- st_as_sf(disaster_total)

disaster_plot <- ggplot(disaster_total_sf) +
  geom_sf(aes(fill = num_disasters_total))

# Data indicating where people went
od_dest <- od_edges %>%
  group_by(dest_id) %>%
  summarise(
    total_immigration = sum(num_migrants),
    .groups = "drop"
  ) %>%
  # Join data from the INEGI shapefiles
  left_join(
    mun_nodes,
    by = c("dest_id" = "mun_id")
  )

#Desitnation 

od_dest_sf <- st_as_sf(od_dest) # Convert to sf object

destination_plot <- ggplot(od_dest) +
  geom_sf(aes(fill = total_immigration))

# Data indicating from where people came
od_origin <- od_edges %>%
  group_by(origin_id) %>%
  summarise(
    total_emigration = sum(num_migrants),
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