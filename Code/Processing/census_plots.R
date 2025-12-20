library(tidyverse)
library(sf)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(knitr)
library(kableExtra)
library(stargazer)

state_nodes_pre <- readRDS("Data/temp/state_nodes_preprocess.rds")
mun_nodes <- readRDS("Data/temp/mun_nodes.rds")
state_nodes <- readRDS("Data/temp/state_nodes_postprocess.rds")

mun_nodes_sf <- st_as_sf(mun_nodes)
state_nodes_sf <- st_as_sf(state_nodes)

mun_nodes_nosf <- st_drop_geometry(mun_nodes)
state_nodes_nosf <- st_drop_geometry(state_nodes)

########### Municipality Tables #############

#### Table 1 ####

## Create dataset for summary statistics table ##

# Variables for panels
outcomes <- c(
  "period_immigration", "period_emmigration", "net_immigration",
  "net_immigration_rate", "immigration_rate", "emmigration_rate"
)

disasters <- c(
  "total_unique_disasters_period", "total_disasters_period"
  # optionally add selected hazard counts here, e.g. "count_flood", "count_drought"
)

controls <- c(
  "mean_inc", "popdensgeo2", "mean_age", "rate_female", "rate_literacy",
  "mean_yrschool", "mun_pop"
)

vars <- c(outcomes, disasters, controls)

# function to compute summary stats 
one_var_stats <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Median = as.numeric(quantile(x, 0.50, names = FALSE)),
    Min = min(x),
    Max = max(x)
  )
}

# prepare data for table
sumtab <- mun_nodes_nosf %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  summarise(one_var_stats(value), .groups = "drop") %>%
  mutate(
    Panel = case_when(
      Variable %in% outcomes ~ "Panel A: Migration outcomes",
      Variable %in% disasters ~ "Panel B: Disaster exposure",
      Variable %in% controls ~ "Panel C: Controls",
      TRUE ~ "Other"
    ),
    # nice formatting columns (adjust decimals if you want)
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD),
    `Median` = sprintf("%.2f", Median),
    Min = sprintf("%.2f", Min),
    Max = sprintf("%.2f", Max)
  ) %>%
  select(Panel, Variable, N, `Mean (SD)`, `Median`, Min, Max) %>%
  arrange(
    factor(Panel, levels = c(
      "Panel A: Migration outcomes",
      "Panel B: Disaster exposure",
      "Panel C: Controls"
    )),
    Variable
  )

# create the indices for the panels
idxA <- which(sumtab$Panel == "Panel A: Migration outcomes")
idxB <- which(sumtab$Panel == "Panel B: Disaster exposure")
idxC <- which(sumtab$Panel == "Panel C: Controls")

# Convert to latex
sumtab %>%
  select(-Panel) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    caption = "Summary statistics (municipality--year level)",
    col.names = c("Variable", "N", "Mean (SD)", "Median", "Min", "Max"),
    align = c("l", "r", "r", "r", "r", "r"),
    escape = TRUE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  ) %>%
  pack_rows("Panel A: Migration outcomes", min(idxA), max(idxA), bold = TRUE) %>%
  pack_rows("Panel B: Disaster exposure", min(idxB), max(idxB), bold = TRUE) %>%
  pack_rows("Panel C: Controls", min(idxC), max(idxC), bold = TRUE) %>%
  footnote(
    general = paste(
      "Unit of observation is municipality--year.",
      "N reports non-missing observations."
    ),
    general_title = "Notes: ",
    threeparttable = TRUE
  )

#### Table 1A ####

period_var <- "year_census"

cell_mean_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

n_nonmiss <- function(x) sum(!is.na(x))


## Set up dataset for table 

mean_sd_by_period <- mun_nodes_nosf %>%
  select(all_of(c(period_var, vars))) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "value") %>%
  group_by(Variable, Period = .data[[period_var]]) %>%
  summarise(
    N = n_nonmiss(value),
    MeanSD = cell_mean_sd(value, digits = 2),
    .groups = "drop"
  )

# Make a wide table with Mean(SD) columns for each period
wide_meansd <- mean_sd_by_period %>%
  select(Variable, Period, MeanSD) %>%
  mutate(Period = as.character(Period)) %>%
  pivot_wider(names_from = Period, values_from = MeanSD)

# Add an overall (All) column
wide_all <- mun_nodes_nosf %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  summarise(All = cell_mean_sd(value, digits = 2), .groups = "drop")

# Add an N column (overall non-missing)
wide_n <- mun_nodes_nosf %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  summarise(N = n_nonmiss(value), .groups = "drop")

sumtab <- wide_meansd %>%
  left_join(wide_all, by = "Variable") %>%
  left_join(wide_n, by = "Variable") %>%
  relocate(N, .after = Variable) %>%
  relocate(All, .after = N)

## Add panels and sort ##
sumtab <- sumtab %>%
  mutate(
    Panel = case_when(
      Variable %in% outcomes ~ "Panel A: Migration outcomes",
      Variable %in% disasters ~ "Panel B: Disaster exposure",
      Variable %in% controls ~ "Panel C: Controls",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(
    factor(Panel, levels = c(
      "Panel A: Migration outcomes",
      "Panel B: Disaster exposure",
      "Panel C: Controls"
    )),
    Variable
  )

period_cols <- setdiff(names(sumtab), c("Panel", "Variable", "N", "All"))

# Indices for panels
idxA <- which(sumtab$Panel == "Panel A: Migration outcomes")
idxB <- which(sumtab$Panel == "Panel B: Disaster exposure")
idxC <- which(sumtab$Panel == "Panel C: Controls")

# Print table

sumtab_print <- sumtab %>%
  select(-Panel) 

kable(
  sumtab,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary statistics by period (Mean (SD))",
  align = "l",
  escape = TRUE
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 8
  ) %>%
  pack_rows("Panel A: Migration outcomes", min(idxA), max(idxA), bold = TRUE) %>%
  pack_rows("Panel B: Disaster exposure", min(idxB), max(idxB), bold = TRUE) %>%
  pack_rows("Panel C: Controls", min(idxC), max(idxC), bold = TRUE) %>%
  footnote(
    general = paste(
      "Unit of observation is municipality--year.",
      "Cells report Mean (SD).",
      "N is the number of non-missing observations."
    ),
    general_title = "Notes: ",
    threeparttable = TRUE
  )

########### Municipality Plots #############

# Income 

income_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = mean_incearn_log1p)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Income by Municipality",
       x = "Municipality",
       y = "Income per Inhabitant")
ggsave("Output/Plots/income_plot_municipality.png", income_plot, width = 14, height = 14)

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
    total_disasters_period = sum(total_disasters_period, na.rm = TRUE)
  )

ggplot(mun_nodes_yearly) +
  geom_line(aes(x = year_census, y = count_tropical_cyclone)) +
  geom_line(aes(x = year_census, y = count_drought)) +
  geom_line(aes(x = year_census, y = total_disasters_period))
