library(tidyverse)
library(lubridate)
library(ipumsr)
library(sf)
library(readxl)
library(scales)

#### Load Data ####

disaster_data_pre <- readRDS("Data/temp/disaster_data.rds")
disaster_data_post <- readRDS("Data/temp/disaster_data_census.rds")
mun_nodes <- readRDS("Data/temp/mun_nodes.rds")

idmc_displacement_raw <- read_excel("Data/Weather Data/IDMC/IDMC_GIDD_Disasters_Internal_Displacement_Data.xlsx")

#### Load Spatial Data ####

census_sf_geo1_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo1_mx1960_2020/geo1_mx1960_2020.shp")
census_sf_geo2_raw <- read_ipums_sf("Data/IPUMS/shapefiles/geo2_mx1960_2020/geo2_mx1960_2020.shp")

census_sf_geo1 <- census_sf_geo1_raw %>% 
  select(ADMIN_NAME, GEOLEVEL1, geometry) %>% 
  janitor::clean_names()

census_sf_geo2 <- census_sf_geo2_raw %>%
  select(ADMIN_NAME, GEOLEVEL2, geometry) %>%
  janitor::clean_names() %>%
  # There are some municipalities with empty geometry
  # These are the #999 municipalities that were also filtered out of the census data
  # -> filter them out
  filter(!st_is_empty(geometry))

#### State Node Preprocessing ####

state_nodes <- mun_nodes %>%
  st_drop_geometry() %>%
  mutate(geolevel1 = as.character(geolevel1)) %>%
  group_by(geolevel1, year_census) %>%
  summarise(
    total_disasters_period = sum(total_disasters_period),
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
  left_join(census_sf_geo1, by = c("geolevel1" = "geolevel1"))

#### Disaster geo plot ####

## Municipalities

disaster_plot_mun <- ggplot(data = mun_nodes) +
  geom_sf(aes(fill = total_disasters_period)) +
  facet_wrap(~ year_census, nrow = 3) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by Municipality",
       x = "Municipality",
       y = "Number of Disasters")
ggsave("Output/Plots/disaster_plot_muns_map.png", disaster_plot_mun, height = 40)

## States

disaster_plot_state <- ggplot() +
  geom_sf(data = state_nodes$geometry, aes(fill = state_nodes$total_disasters_period)) +
  facet_wrap(~ state_nodes$year_census, nrow = 3) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by States",
       x = "State",
       y = "Number of Disasters")

ggsave("Output/Plots/disaster_plot_states_map.png", disaster_plot_state, height = 40)

#### Disasters development years plot ####

## Aggregate raw disaster data for graph

disasters_yearly <- disaster_data_pre %>%
  mutate(
    date = dmy(fecha_inicio),
    year = year(date)) %>%
  group_by(year) %>%
  summarize(num_disasters_year = n(), .groups = "drop")

## Aggregate municipality data for graph

mun_nodes_yearly <- mun_nodes %>%
  st_drop_geometry() %>%
  group_by(year_census) %>%
  summarise(total_disasters_period = sum(total_disasters_period), .groups = "drop")
    
## Aggregate census disaster data for graph

disaster_data_post_yearly <- disaster_data_post %>%
  group_by(year_census) %>%
  summarise(total_disasters_period = n(), .groups = "drop")


## Plot aggregated yearls

mean_disasters <- mean(disasters_yearly$num_disasters_year, na.rm = TRUE)

disaster_development_plot <- ggplot(disasters_yearly, aes(x = year, y = num_disasters_year)) +
  geom_line(color = "#2C7FB8") +
  geom_point(color = "#2C7FB8") +
  geom_hline(yintercept = mean_disasters, linetype = "longdash", color = "#ff6600f0") +
  annotate(
    "text", x = min(disasters_yearly$year), 
    y = mean_disasters,
    label = paste0(
      "Mean Annual Disasters = ",
      round(mean_disasters, 1)
    ),
    hjust = -0.05,
    vjust = -0.6,
    color = "#ff6600f0",
    size = 7
  ) +
  labs(
    x = "Year",
    y = "Number of Disasters"
  ) +
  theme_light(base_size = 30) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave("Output/Plots/disaster_development_plot.png", disaster_development_plot)

## Plot aggregated for census

disaster_development_agg_plot <- ggplot() +
geom_point(data = disasters_yearly, aes(x = year, y = num_disasters_year)) +
geom_line(data = disasters_yearly, aes(x = year, y = num_disasters_year)) +
geom_point(data = disaster_data_post_yearly, aes(x = year_census, y = total_disasters_period)) +
geom_line(data = disaster_data_post_yearly, aes(x = year_census, y = total_disasters_period)) +
geom_point(data = mun_nodes_yearly, aes(x = year_census, y = total_disasters_period, color = "#2C7FB8")) +
geom_line(data = mun_nodes_yearly, aes(x = year_census, y = total_disasters_period, color = "#ff6600f0")) +
  labs(
    x = "Year",
    y = "Number of Disasters"
  ) +
  theme_light(base_size = 30) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave("Output/Plots/disaster_development_agg_plot.png", disaster_development_agg_plot)

#### IDMC Displacement Plot ####

## Aggregate data by year

idmc_displacement <- idmc_displacement_raw %>%
  janitor::clean_names() %>%
  group_by(year) %>%
  summarise(disaster_internal_displacements_year = sum(disaster_internal_displacements), .groups = "drop")

median_displaced <- median(idmc_displacement$disaster_internal_displacements_year)

idmc_displacement_plot <- ggplot(data = idmc_displacement, aes(x = year, y = disaster_internal_displacements_year, fill = "lightblue", color = "darkblue", alpha = 0.7)) +
  geom_col(fill = "#2C7FB8", color = "darkblue") +
  geom_hline(yintercept = median_displaced, linetype = "longdash", color = "#ff6600f0") +
  annotate(
    "text", x = min(idmc_displacement$year), 
    y = median_displaced,
    label = paste0(
      "Median Annual Displaced = ",
      round(median_displaced, 1)
    ),
    hjust = -0.05,
    vjust = -0.6,
    color = "#ff6600f0",
    size = 7
  ) +
  labs(
    x = "Year",
    y = "Number of Displaced"
  ) +
  theme_light(base_size = 30) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave("Output/Plots/idmc_displacement_plot.png", idmc_displacement_plot)