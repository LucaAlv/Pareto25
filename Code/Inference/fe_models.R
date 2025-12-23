library(tidyverse)
library(estimatr)
library(fixest)
library(sf)
library(knitr)
library(kableExtra)

# Nodes with pre and post periods
mun_nodes_ll <- readRDS("Data/temp/mun_nodes.rds")

# Nodes without leads and lags
mun_nodes <- mun_nodes_ll %>%
  filter(!year_census == 2005) %>%
  filter(!year_census == 2025)

# Add lat lon columns for conley SEs
coords <- st_coordinates(st_point_on_surface(mun_nodes$geometry))
mun_nodes$lon <- coords[, 1]
mun_nodes$lat <- coords[, 2]

######################## Regressions ########################

clean_names <- c(
    net_immigration = "Net Immigration",
    total_disasters_period = "Total disasters (period)",
    mun_pop = "Municipal population",
    mean_age = "Average age",
    rate_female = "Female share",
    rate_literacy = "Literacy rate",
    mean_urban = "Urbanization rate",
    crime_period = "Crime rate (homicides)",
    mean_inc = "Mean income",
    geolevel2 = "Municipality FE",
    geolevel1 = "State FE",
    year_census = "Year FE",
    total_disasters_lag1 = "Total disasters (lag 1)",
    total_disasters_lead1 = "Total disaster (lead 1)"
  )

# base model -> main table
twfe_net_ols <- feols(
  net_immigration ~ total_disasters_period,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# with FEs -> main table 
model_net_fe_nocontrol <- feols(
  net_immigration ~ total_disasters_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# main specification -> main table
model_net <- feols(
  net_immigration ~ total_disasters_period + 
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# main specification with lag1 -> main table
model_net_lag1 <- feols(
  net_immigration ~ total_disasters_period + total_disasters_lag1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

table1 <- etable(
  twfe_net_ols, model_net_fe_nocontrol, model_net, model_net_lag1,
  se = "cluster",
  cluster = ~geolevel2,
  tex = TRUE,
  title = "Main Results",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2,
  dict = clean_names
)

# lead1 specification -> second table
model_net_only_lead1 <- feols(
  net_immigration ~ total_disasters_lead1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# without income -> second table
model_net_noincome <- feols(
  net_immigration ~ total_disasters_period + 
    mun_pop + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# main specification with state x time FEs -> second table
model_net_statefe <- feols(
  net_immigration ~ total_disasters_period + 
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census + geolevel1^year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

table2 <- etable(
  model_net_only_lead1, model_net_noincome, model_net_statefe,
  se = "cluster",
  cluster = ~geolevel2,
  tex = TRUE,
  title = "Alternative Model Specifications",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2,
  dict = clean_names
)

# main specification with lag1 without controls -> third table
model_net_lag1 <- feols(
  net_immigration ~ total_disasters_period + total_disasters_lag1 |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# main specification with lead1 -> third table
model_net_lead1 <- feols(
  net_immigration ~ total_disasters_period + total_disasters_lead1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census + geolevel1^year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# full dynamic specification -> third table
model_net_dynamic <- feols(
  net_immigration ~ total_disasters_lag1 + total_disasters_period + total_disasters_lead1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census + geolevel1^year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

# main specification with conley SEs
model_net_conley_50 <- feols(
  net_immigration ~ total_disasters_period + 
  mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 50)
)

# main specification with conley SEs
model_net_conley_100 <- feols(
  net_immigration ~ total_disasters_period + 
  mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 100)
)

table3 <- etable(
  model_net_lag1, model_net_lead1, model_net_dynamic, model_net_conley_50, model_net_conley_100,
  cluster = ~geolevel2,
  tex = TRUE,
  title = "Alternative Model Specifications",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2,
  dict = clean_names
)

######################## Graphs ########################

#### Figure 1 ####

# residualize migration
res_y <- feols(
  net_immigration ~ 1 | geolevel2 + year_census,
  data = mun_nodes
) %>% resid()

# residualize disasters 
res_x <- feols(
  total_disasters_period ~ 1 | geolevel2 + year_census,
  data = mun_nodes
) %>% resid()

plot_data <- mun_nodes %>%
  mutate(
    y_res = res_y,
    x_res = res_x
  )

ggplot(plot_data, aes(x = x_res, y = y_res)) +
  stat_summary_bin(
    bins = 20,
    fun = mean,
    geom = "point"
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Disaster intensity (residualized)",
    y = "Net immigration rate (residualized)"
  ) +
  theme_minimal()
