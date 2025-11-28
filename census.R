library(tidyverse)
library(data.table)
library(sf)
library(purrr)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)

######################## Census Data ########################
#### Loading the INEGI Shapefiles ####

# Creating list of municipios and entidads
municipios_csv <- read_delim("Data/Socioeconomic Data/Census 2020/Shapefiles/889463807469_s/01_aguascalientes/catalogos/municipios.csv", delim = ";", locale = locale(encoding = "ISO-8859-1"))
colnames(municipios_csv) <-  c(municipios_csv[2, ])
municipios_csv <- municipios_csv[c(-1, -2), ]
entidad_csv <- municipios_csv %>%
  janitor::clean_names() %>%
  group_by(nombre_de_entidad) %>%
  summarise()

shapefiles_root <- "Data/Socioeconomic Data/Census 2020/Shapefiles/889463807469_s"

# find all mun.shp files in the dir
shp_files <- list.files(shapefiles_root, pattern = "mun\\.shp$", recursive = TRUE, full.names = TRUE)

# extrahiere aussagekräftigen State-Namen aus dem Ordnernamen (z.B. "01_aguascalientes" -> "aguascalientes")
state_names <- shp_files %>% dirname() %>% dirname %>% basename() %>% sub("^\\d+_", "", .)

# lade alle shapefiles als named list
shp_list <- set_names(shp_files, state_names) %>%
  map(~ st_read(.x, quiet = TRUE))

inegi_shape_data <- do.call(rbind, shp_list) %>%
  janitor::clean_names()

# plot(st_geometry(inegi_shape_data))

#### Load Census Data ####

census_ind_noedit <- fread("Data/Socioeconomic Data/Census 2020/ExtendedQuestionaire/Censo2020_CA_eum_csv/Personas00.CSV")

census_ind <- census_ind_noedit %>%
  select(
    "ID_PERSONA", # Unique Person Identifier
    "ID_VIV", # Unique Housing Identifier
    "NUMPER", # Number of Person
    "ENT", # State
    "ENT_PAIS_NAC", # State of birth
    "ENT_PAIS_RES_5A", # State of residence five years ago
    "MUN", # Current municipio
    "MUN_RES_5A", # Municipio five years ago
    "CAUSA_MIG_V", # Reason for migration
    "LOC50K", # Town key of 50.000 or more inhabitants
    "ESTRATO", # Stratum - might be indicator of local wealth - influences costs of utilities
    "SEXO", # (Name) is male, (Name) is female
    "EDAD" # Age
  ) %>%
  janitor::clean_names() %>%
  # Removing all NAs from origin municipality and entada columns
  filter(!is.na(mun_res_5a) & !is.na(ent_pais_res_5a)) %>% 
  # Adapting municipality codes to INEGI shapefile geocodes
  mutate(
    dest_id = sprintf("%02d%03d", as.integer(ent), as.integer(mun)),
    origin_id = sprintf("%02d%03d", as.integer(ent_pais_res_5a), as.integer(mun_res_5a)),
  )

######################## ERA5 weather data ########################


# Extracting era5 weather data
era5_2020 <- rast("Data/Weather Data/ERA5/era5_2020_rain.grib")

# Construct index out of mean and standard deviation

# Aggregating monthly 
era5_2020_monthly <- tapp(era5_2020, "months", mean)
era5_2020_monthly_mun <- extract(era5_2020_monthly, inegi_shape_data, fun = mean, na.rm = TRUE)
mm <- rowMeans(era5_2020_monthly_mun, na.rm = TRUE)


######################## IBTrACS ########################

ibtracs <- read_csv("Data/Weather Data/IBTrACS/ibtracs.since1980.list.v04r01.csv")

ibtracs_shape_lines <- st_read("Data/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.lines")
ibtracs_shape_points <- st_read("Data/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.points")
ibtracs_shape_points_edit <- ibtracs_shape_points %>%
  janitor::clean_names() %>%
  filter(basin == "EP"| basin == "NA") %>%
  filter(season >= 2000) %>%
  select(
    sid, season, number, basin, subbasin, name, iso_time, nature, lat, lon,
    wmo_wind, wmo_pres, track_type, dist2land, landfall, iflag, storm_spd, storm_dr, year, month, day, hour, min, geometry
  ) %>%
  arrange(sid, iso_time) %>%         # sort within each storm
  group_by(sid) %>%
  summarise(do_union = FALSE) %>%    # keep separate geometries per storm
  st_cast("LINESTRING")



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

