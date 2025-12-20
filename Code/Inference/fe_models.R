library(tidyverse)
library(estimatr)
library(fixest)

mun_nodes <- readRDS("Data/temp/mun_nodes.rds")
state_nodes <- readRDS("Data/temp/state_nodes_postprocess.rds")

######################## Regressions ########################

mod1 <- lm(period_emmigration ~ total_disasters_period, data = mun_nodes)
mod2 <- lm(period_immigration ~ total_disasters_period, data = mun_nodes)
mod3 <- lm(net_immigration ~ total_disasters_period, data = mun_nodes)
mod4 <- lm(net_immigration ~ total_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy, data = mun_nodes)

summary(mod4)

# TWFE model for out migration rate as function of simple disaster indicator 
model_out <- feols(
  emmigration_rate ~ total_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2,
  vcov = "cluster"
)

summary(model_out)

# TWFE model for net migration rate as function of simple disaster indicator 
model_net <- feols(
  net_immigration ~ total_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2,
  vcov = "cluster"
)

summary(model_net)