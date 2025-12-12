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

mun_nodes <- readRDS("Data/demp/mun_nodes.rds")

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



mod1 = lm(emmigration_rate ~ num_disasters_period, data = mun_nodes)
mod2 = lm(period_immigration ~ num_disasters_period, data = mun_nodes)
mod3 = lm(net_migration ~ num_disasters_period, data = mun_nodes)

# TWFE model for out migration rate as function of simple disaster indicator 
model_out <- feols(
  emmigration_rate ~ num_disasters_period |
    geolevel2 + year_census,
  data = mun_nodes,
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

