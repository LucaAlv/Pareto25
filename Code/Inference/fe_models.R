library(tidyverse)
library(estimatr)
library(fixest)
library(sf)

mun_nodes <- readRDS("Data/temp/mun_nodes.rds")

# Add lat lon columns for conley SEs
coords <- st_coordinates(st_point_on_surface(mun_nodes$geometry))
mun_nodes$lon <- coords[, 1]
mun_nodes$lat <- coords[, 2]

######################## Regressions ########################

#### Absolute Net Immigration ####

# TWFE model for net migration rate as function of simple disaster indicator 
twfe_net_ols <- feols(
  net_immigration ~ total_disasters_period,
  data = mun_nodes,
  cluster = ~ geolevel2
)

model_net_fe_nocontrol <- feols(
  net_immigration ~ total_disasters_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

model_net <- feols(
  net_immigration ~ total_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  cluster = ~ geolevel2
)

net_immigration_table_print <- etable(
  twfe_net_ols, model_net_fe_nocontrol, model_net,
  se = "cluster",
  cluster = ~geolevel2,
  digits = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2, 
  headers = c("(1)", "(2)", "(3)")
)

net_immigration_table_tex <- etable(
  twfe_net_ols, model_net_fe_nocontrol, model_net,
  se = "cluster",
  cluster = ~geolevel2,
  tex = TRUE,
  digits = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2, 
  headers = c("(1)", "(2)", "(3)")
)

writeLines(net_immigration_table_tex, "Output/Results/net_immigration_table.tex")

#### Absolute Net Immigration with Conley SEs ####

# TWFE model for net migration rate as function of simple disaster indicator 
twfe_net_ols_conley <- feols(
  net_immigration ~ total_disasters_period,
  data = mun_nodes,
  vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 50)
)

model_net_fe_nocontrol_conley <- feols(
  net_immigration ~ total_disasters_period |
    geolevel2 + year_census,
  data = mun_nodes,
  vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 50)
)

model_net_conley <- feols(
  net_immigration ~ total_disasters_period + mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes,
  vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 50)
)

net_immigration_conley_table_print <- etable(
  twfe_net_ols_conley, model_net_fe_nocontrol_conley, model_net_conley,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2, 
  headers = c("(1)", "(2)", "(3)")
)

net_immigration_conley_table_tex <- etable(
  twfe_net_ols_conley, model_net_fe_nocontrol_conley, model_net_conley,
  tex = TRUE,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ n + r2, 
  headers = c("(1)", "(2)", "(3)")
)

writeLines(net_immigration_conley_table_tex, "Output/Results/net_immigration_conley_table.tex")

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

# “After removing municipality and year fixed effects, higher disaster intensity is associated with lower/higher net migration.”

# “Figure 1 plots binned averages of net immigration and disaster intensity after residualizing both variables with respect to municipality and year fixed effects. 

# Each point represents the mean within one of 20 equally sized bins.”


#### Figure 2 - leads and lags ####

# Show No anticipation (future disasters shouldn’t predict current migration)

# Show Timing/persistence (is the effect concentrated in the most recent 5 years or does it linger?)

# build municipality dataset with leads and lags for disasters
mun_nodes_ll <- mun_nodes %>%
  st_drop_geometry() %>%
  arrange(geolevel2, year_census) %>%
  group_by(geolevel2) %>%
  mutate(
    shock_0 = total_disasters_period, # disasters in previous 5 years
    shock_lag1 = lag(total_disasters_period), # previous interval (5–10 years ago)
    shock_lead1 = lead(total_disasters_period) # next interval as placebo
  ) %>%
  ungroup()


# Check if future disaster intensity predicts current migration - it shouldn't
m_lead <- feols(
  net_immigration ~ shock_0 + shock_lead1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_nodes_ll,
  cluster = ~ geolevel2
)
# shock_lead1 should be approx. 0 or at least insignificant

summary(m_lead)


# Check if disaster intensity from previous 5-year interval still matters
m_lag <- feols(
  net_immigration ~ shock_0 + shock_lag1 +
    mun_pop + mean_inc + mean_age + rate_female + rate_literacy + mean_urban + crime_period |
    geolevel2 + year_census,
  data = mun_es,
  cluster = ~ geolevel2
)
# shock_lag1 gives information about whether effets persist beyond the immediate 5-year window

summary(m_lag)

get_term <- function(model, term, event_time){
  broom::tidy(model, conf.int = TRUE) %>%
    subset(term == !!term) %>%
    transform(event_time = event_time)
}

df_plot <- rbind(
  get_term(m_lag,  "shock_lag1",  -1),
  get_term(m_lag,  "shock_0",      0),
  get_term(m_lead, "shock_lead1",  1)
)

df_plot$label <- factor(df_plot$event_time,
                        levels = c(-1,0,1),
                        labels = c("Lag (5–10y ago)", "Current (0–5y)", "Lead (next 5y, placebo)"))

ggplot(df_plot, aes(x = label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_point(size = 2) +
  labs(x = "", y = "Effect per 1-unit increase in disasters (clustered 95% CI)") +
  theme_minimal()
