library(tidyverse)
library(sf)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(knitr)
library(kableExtra)

state_nodes_pre <- readRDS("Data/temp/state_nodes_preprocess.rds")
mun_nodes <- readRDS("Data/temp/mun_nodes.rds")
state_nodes <- readRDS("Data/temp/state_nodes_postprocess.rds")

mun_nodes_sf <- st_as_sf(mun_nodes)
state_nodes_sf <- st_as_sf(state_nodes)

mun_nodes_nosf <- st_drop_geometry(mun_nodes)
state_nodes_nosf <- st_drop_geometry(state_nodes)

########### Municipality Tables #############

#### Variable Name Mapping ####

# Create a mapping for pretty variable names
var_labels <- c(
  "period_immigration" = "Period Immigration",
  "period_emmigration" = "Period Emmigration",
  "net_immigration" = "Net Immigration",
  "net_immigration_rate" = "Net Immigration Rate",
  "immigration_rate" = "Immigration Rate",
  "emmigration_rate" = "Emmigration Rate",
  "total_unique_disasters_period" = "Total Unique Disasters (Period)",
  "total_disasters_period" = "Total Disasters (Period)",
  "mean_inc" = "Mean Income",
  "popdensgeo2" = "Population Density",
  "mean_age" = "Mean Age",
  "rate_female" = "Female Rate",
  "rate_literacy" = "Literacy Rate",
  "mean_yrschool" = "Mean Years of Schooling",
  "mun_pop" = "Municipal Population"
)

#### Table 1: Overall Summary Statistics ####

# Define variable groups
outcomes <- c(
  "period_immigration", "period_emmigration", "net_immigration",
  "immigration_rate", "emmigration_rate", "net_immigration_rate"
)

disasters <- c(
  "total_unique_disasters_period", "total_disasters_period"
)

controls <- c(
  "mean_inc", "popdensgeo2", "mean_age", "rate_female", "rate_literacy",
  "mean_yrschool", "mun_pop"
)

vars <- c(outcomes, disasters, controls)

# Function to compute summary statistics
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

# Prepare data for table
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
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD),
    Median = sprintf("%.2f", Median),
    Min = sprintf("%.2f", Min),
    Max = sprintf("%.2f", Max),
    # Rename variables using the mapping
    Variable = recode(Variable, !!!var_labels)
  ) %>%
  select(Panel, Variable, N, `Mean (SD)`, Median, Min, Max) %>%
  arrange(
    factor(Panel, levels = c(
      "Panel A: Migration outcomes",
      "Panel B: Disaster exposure",
      "Panel C: Controls"
    )),
    # Custom order for variables within each panel
    factor(Variable, levels = c(
      # Panel A order
      "Period Immigration", "Period Emmigration", "Net Immigration",
      "Immigration Rate", "Emmigration Rate", "Net Immigration Rate",
      # Panel B variables (will maintain their order)
      "Total Unique Disasters (Period)", "Total Disasters (Period)",
      # Panel C variables (will maintain their order)
      "Mean Income", "Mean Age", "Female Rate", "Literacy Rate", 
      "Mean Years of Schooling", "Municipal Population", "Population Density"
    ))
  )

# Create panel indices
idxA <- which(sumtab$Panel == "Panel A: Migration outcomes")
idxB <- which(sumtab$Panel == "Panel B: Disaster exposure")
idxC <- which(sumtab$Panel == "Panel C: Controls")

# Generate LaTeX table
table1 <- sumtab %>%
  select(-Panel) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    caption = "Summary statistics (municipality-year level)",
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
      "Unit of observation is municipality-year.",
      "N reports non-missing observations."
    ),
    general_title = "Notes: ",
    threeparttable = TRUE
  )


#### Table 1A: Summary Statistics by Period ####

period_var <- "year_census"

# Helper functions
n_nonmiss <- function(x) sum(!is.na(x))

# Calculate mean and SD by period
mean_sd_by_period <- mun_nodes_nosf %>%
  select(all_of(c(period_var, vars))) %>%
  pivot_longer(cols = all_of(vars),
               names_to = "Variable",
               values_to = "value") %>%
  group_by(Variable, Period = .data[[period_var]]) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Get periods for ordering
periods <- sort(unique(mean_sd_by_period$Period))

# Make wide table with Mean and SD columns for each period
wide_meansd <- mean_sd_by_period %>%
  mutate(
    # Round Mean and SD to 2 decimal places
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  ) %>%
  pivot_wider(
    names_from = Period,
    values_from = c(Mean, SD),
    names_glue = "{Period}_{.value}"
  )

# Calculate overall N for each variable
wide_n <- mun_nodes_nosf %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  summarise(N = n_nonmiss(value), .groups = "drop")

# Combine into summary table
sumtab <- wide_meansd %>%
  left_join(wide_n, by = "Variable") %>%
  relocate(N, .after = Variable)

# Add panels and sort
sumtab <- sumtab %>%
  mutate(
    Panel = case_when(
      Variable %in% outcomes ~ "Panel A: Migration outcomes",
      Variable %in% disasters ~ "Panel B: Disaster exposure",
      Variable %in% controls ~ "Panel C: Controls",
      TRUE ~ "Other"
    ),
    # Rename variables using the mapping
    Variable = recode(Variable, !!!var_labels)
  ) %>%
  arrange(
    factor(Panel, levels = c(
      "Panel A: Migration outcomes",
      "Panel B: Disaster exposure",
      "Panel C: Controls"
    )),
    # Custom order for variables within each panel
    factor(Variable, levels = c(
      # Panel A order
      "Period Immigration", "Period Emmigration", "Net Immigration",
      "Immigration Rate", "Emmigration Rate", "Net Immigration Rate",
      # Panel B variables (will maintain their order)
      "Total Unique Disasters (Period)", "Total Disasters (Period)",
      # Panel C variables (will maintain their order)
      "Mean Income", "Mean Age", "Female Rate", "Literacy Rate", 
      "Mean Years of Schooling", "Municipal Population", "Population Density"
    ))
  )

# Create panel indices
idxA <- which(sumtab$Panel == "Panel A: Migration outcomes")
idxB <- which(sumtab$Panel == "Panel B: Disaster exposure")
idxC <- which(sumtab$Panel == "Panel C: Controls")

# Get period-stat columns and order them properly
period_cols <- names(sumtab)[grepl("_(Mean|SD)$", names(sumtab))]

# Extract period and stat type from column names
get_period <- function(x) as.numeric(sub("_(Mean|SD)$", "", x))
get_stat <- function(x) sub("^.*_(Mean|SD)$", "\\1", x)

# Order by period, then Mean before SD
period_cols <- period_cols[order(
  get_period(period_cols),
  match(get_stat(period_cols), c("Mean", "SD"))
)]

# Prepare table for printing
sumtab_print <- sumtab %>%
  select(Variable, N, all_of(period_cols))

# Create column headers
periods_chr <- as.character(sort(unique(get_period(period_cols))))

# Top header: period labels spanning Mean and SD columns
header_top <- c(" " = 2, setNames(rep(2, length(periods_chr)), periods_chr))

# Bottom header: Mean and SD labels
header_bottom <- c("Variable" = 1, "N" = 1, 
                   rep(c("Mean", "SD"), times = length(periods_chr)))

# Alignment vector
align_vec <- c("l", "r", rep(c("r", "r"), length(periods_chr)))

# Generate LaTeX table
table2 <- kable(
  sumtab_print,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary statistics by period",
  align = align_vec,
  col.names = c("Variable", "N", rep(c("Mean", "SD"), length(periods_chr))),
  escape = TRUE
) %>%
  add_header_above(header_top) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 8
  ) %>%
  pack_rows("Panel A: Migration outcomes", min(idxA), max(idxA), bold = TRUE) %>%
  pack_rows("Panel B: Disaster exposure", min(idxB), max(idxB), bold = TRUE) %>%
  pack_rows("Panel C: Controls", min(idxC), max(idxC), bold = TRUE) %>%
  footnote(
    general = paste(
      "Unit of observation is municipality-year.",
      "Cells report means and standard deviations.",
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
  labs(title = "Average Income by Municipality",
       x = "Municipality",
       y = "Average Income per Inhabitant")
ggsave("Output/Plots/income_plot_municipality.png", income_plot, dpi = 700)

# Age 

age_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = mean_age)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Average Age by Municipality",
       x = "Municipality",
       y = "Mean Age")
ggsave("Output/Plots/age_plot_municipality.png", age_plot, dpi = 700)

# Female 

female_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = rate_female)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Average Percent Female by Municipality",
       x = "Municipality",
       y = "Percent Females")
ggsave("Output/Plots/female_plot_municipality.png", female_plot, dpi = 700)

# Literacy 

literacy_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = rate_literacy)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Average Percent Literate by Municipality",
       x = "Municipality",
       y = "Percent Literate")
ggsave("Output/Plots/literacy_plot_municipality.png", literacy_plot, dpi = 700)

# Schooling 

schooling_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = mean_yrschool)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Average Years of Schooling by Municipality",
       x = "Municipality",
       y = "Average Years of Schooling")
ggsave("Output/Plots/schooling_plot_municipality.png", schooling_plot, dpi = 700)

# Total Population 

total_population_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = mun_pop)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Total Population by Municipality",
       x = "Municipality",
       y = "Total Population")
ggsave("Output/Plots/total_population_plot_municipality.png", total_population_plot, dpi = 700)

# Population Density 

population_density_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = popdensgeo2)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Average Population Density by Municipality",
       x = "Municipality",
       y = "Average Population Density")
ggsave("Output/Plots/population_density_plot_municipality.png", population_density_plot, dpi = 700)

########### Migration Plots #############

# Immigration 

immigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = period_immigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Amount of Immigrants")
ggsave("Output/Plots/immigration_plot_municipality.png", immigration_plot, dpi = 700)

# Emmigration

emmigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = period_emmigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Amount of Emmigrants")
ggsave("Output/Plots/immigration_plot_municipality.png", emmigration_plot, dpi = 700)

# Net Immigration

net_immigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = net_immigration_rate)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Net amount of Migrants")
ggsave("Output/Plots/net_immigration_plot.png", net_immigration_plot, dpi = 700)

# Immigration Rate

immigration_rate_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = net_immigration)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Immigration rate by Municipality",
       x = "Municipality",
       y = "Immigration rate (Immigrants / inhabitant)")
ggsave("Output/Plots/immigration_rate_plot.png", immigration_rate_plot, dpi = 700)

# Emmigration Rate

emmigration_rate_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = emmigration_rate)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Emmigration rate by Municipality",
       x = "Municipality",
       y = "Emmigration rate (Emmigrants / inhabitant)")
ggsave("Output/Plots/emmigration_rate_plot.png", emmigration_rate_plot, dpi = 700)

# Net Immigration Rate

net_immigration_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = net_immigration_rate)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Net Immigration rate by Municipality",
       x = "Municipality",
       y = "Net Immigration rate (Net Immigrants / inhabitant)")
ggsave("Output/Plots/net_immigration_plot.png", net_immigration_plot, dpi = 700)