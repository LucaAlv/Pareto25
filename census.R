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
municipios_csv <- read_delim("Data/Mexico/Census 2020/Shapefiles/889463807469_s/01_aguascalientes/catalogos/municipios.csv", delim = ";")
colnames(municipios_csv) <-  c(municipios_csv[2, ])
municipios_csv <- municipios_csv[c(-1, -2), ]
entidad_csv <- municipios_csv %>%
janitor::clean_names() %>%
  group_by(nombre_de_entidad) %>%
  summarise()

shapefiles_root <- "Data/Mexico/Census 2020/Shapefiles/889463807469_s"

# finde alle mun.shp im Verzeichnis (rekursiv)
shp_files <- list.files(shapefiles_root, pattern = "mun\\.shp$", recursive = TRUE, full.names = TRUE)

# extrahiere aussagekräftigen State-Namen aus dem Ordnernamen (z.B. "01_aguascalientes" -> "aguascalientes")
state_names <- shp_files %>% dirname() %>% dirname %>% basename() %>% sub("^\\d+_", "", .)

# lade alle shapefiles als named list
shp_list <- set_names(shp_files, state_names) %>%
  map(~ st_read(.x, quiet = TRUE))

inegi_shape_data <- do.call(rbind, shp_list) %>%
  janitor::clean_names()

# Plot with
plot(st_geometry(inegi_shape_data))

#### Load Census Data ####

census_ind_noedit <- fread("Data/Mexico/Census 2020/ExtendedQuestionaire/Censo2020_CA_eum_csv/Personas00.CSV")

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
  mutate(
    mun_cur = mun_cur <- sprintf("%02d%03d", as.integer(ent), as.integer(mun)),
    mun_5a = sprintf("%02d%03d", as.integer(ent_pais_res_5a), as.integer(mun_res_5a)),
  )

# Origin-destination matrix that contains 
census_ind_OD <- census_ind %>%
  # Filter out people who didn't move in the past five years
  filter(mun_cur != mun_5a) %>%
  group_by(mun_cur, mun_5a) %>%
  summarise(
    num_migrants = n(),
    mage = mean(edad, na.rm = TRUE),
    msex = mean(sexo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join data from the INEGI shapefiles
  left_join(
    inegi_shape_data %>% select(cvegeo, geometry),
    by = c("mun_cur" = "cvegeo")
  )

ggplot() +
  geom_sf(dat = census_ind_OD$geometry)


  

######################## ERA5 weather data ########################


# Extracting era5 weather data
era5_2020 <- rast("Data/Mexico/Weather Data/ERA5/era5_2020_rain.grib")

# Construct index out of mean and standard deviation

# Aggregating monthly 
era5_2020_monthly <- tapp(era5_2020, "months", mean)
era5_2020_monthly_mun <- extract(era5_2020_monthly, shp_list, fun = mean, na.rm = TRUE)
mm <- rowMeans(era5_2020_monthly_mun, na.rm = TRUE)


######################## IBTrACS ########################

ibtracs <- read_csv("Data/Mexico/Weather Data/IBTrACS/ibtracs.since1980.list.v04r01.csv")

ibtracs_shape_lines <- st_read("Data/Mexico/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.lines/IBTrACS.since1980.list.v04r01.lines.shp")
ibtracs_shape_points <- st_read("Data/Mexico/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.points/IBTrACS.since1980.list.v04r01.points.shp")
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

mexico <- rnaturalearth::ne_countries(
  country = "mexico",
  scale = "medium",
  returnclass = "sf"
)

ggplot() +
  geom_sf(data = mexico, linewidth = 0.2) +
  geom_sf(
    data = ibtracs_shape_points,
    linewidth = 0.001,
    alpha = 0.7
  ) +
  coord_sf(expand = FALSE) +
  scale_color_viridis_c(option = "C", end = 0.9) +
  theme_minimal() +
  labs(
    title = "IBTrACS North Atlantic Tropical Cyclone Tracks (≥ 2000)",
    x = "Longitude", y = "Latitude",
    color = "Season"
  )