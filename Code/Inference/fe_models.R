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

mun_nodes_raw <- readRDS("Data/temp/mun_nodes.rds")
state_nodes <- readRDS("Data/temp/state_nodes_postprocess.rds")

########### Municipality Plots #############

mun_nodes <- mun_nodes_raw %>%
  # Put this in preprocessing
  # Set missing values to 0
  mutate(num_disasters_period = ifelse(is.na(num_disasters_period), 0, num_disasters_period))

mun_nodes_sf <- st_as_sf(mun_nodes)

state_nodes_sf <- state_nodes %>%
  # Set missing values to 0
  mutate(num_disasters_period = ifelse(is.na(num_disasters_period), 0, num_disasters_period)) %>%
  st_as_sf()

disaster_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = num_disasters_period)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by Municipality",
       x = "Municipality",
       y = "Number of Disasters")
ggsave("Output/Plots/disaster_plot.png", disaster_plot)

disaster_plot_state <- ggplot(state_nodes_sf) +
  geom_sf(aes(fill = num_disasters_period)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by States",
       x = "State",
       y = "Number of Disasters")
ggsave("Output/Plots/disaster_plot_state.png", disaster_plot_state)

popdensgeo_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = popdensgeo2)) 

########### Migration Plots #############

# Immigration 

immigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = period_immigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Amount of Immigrants per Inhabitant")
ggsave("Output/Plots/immigration_plot_municipality.png", immigration_plot)

# Emmigration

emmigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = period_emmigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Amount of Emmigrants per Inhabitant")
ggsave("Output/Plots/immigration_plot_municipality.png", emmigration_plot)

# Net Migration

net_migration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = net_immigration_rate)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Net amount of Migrants per Inhabitant")
ggsave("Output/Plots/net_migration_plot_municipality.png", net_migration_plot, width = 14, height = 14)

net_migration_plot_states <- ggplot(state_nodes_sf) +
  geom_sf(aes(fill = net_immigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by State",
       x = "State",
       y = "Net amount of Migrants per Inhabitant")
ggsave("Output/Plots/net_migration_plot_state.png", net_migration_plot_states)


mun_nodes_yearly <- mun_nodes %>%
  st_drop_geometry() %>%
  group_by(year_census) %>%
  summarise(
    num_disasters_period = sum(num_disasters_period, na.rm = TRUE)
  )

ggplot(mun_nodes_yearly) +
  geom_line(aes(x = year_census, y = count_tropical_cyclone)) +
  geom_line(aes(x = year_census, y = count_drought)) +
  geom_line(aes(x = year_census, y = num_disasters_period))

######################## Regressions ########################

mod1 <- lm(period_emmigration ~ num_disasters_period, data = mun_nodes)
mod2 <- lm(period_immigration ~ num_disasters_period, data = mun_nodes)
mod3 <- lm(net_immigration ~ num_disasters_period, data = mun_nodes)

summary(lm(period_emmigration ~ num_disasters_period + mun_pop, data = mun_nodes))
summary(lm(period_immigration ~ num_disasters_period + mun_pop, data = mun_nodes))
summary(lm(net_immigration ~ num_disasters_period + mun_pop, data = mun_nodes))


# TWFE model for out migration rate as function of simple disaster indicator 
model_out <- feols(
  emmigration_rate ~ num_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

summary(model_out)

# TWFE model for net migration rate as function of simple disaster indicator 
model_net <- feols(
  net_immigration ~ num_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy |
    geolevel2 + year_census + (geolevel2*year_census),
  data = mun_nodes,
  cluster = ~ geolevel2
)

summary(model_net)

