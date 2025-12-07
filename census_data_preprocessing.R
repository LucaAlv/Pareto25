######## This file does all preprocessing of the Census Data ########

#### Load packages ####

library(tidyverse)
library(ipumsr)
library(data.table)

#### Load Census Data ####

census_ddi <- read_ipums_ddi("Data/Migration Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi)

#### Formatting NAs correctly for geolevel2 ####
census_data <- census_data_raw %>%
  janitor::clean_names() %>%
  rename(geolevel2 = geolev2, geolevel1 = geolev1) %>%
  mutate(
    geolevel2 = as.character(geolevel2),
    geolevel2 = if_else(
      geolevel2 %in% c(
        "484001999", "484002999", "484003999", "484004999", "484005999", "484006999",
        "484007999", "484008999", "484009999", "484010999", "484011999", "484012999",
        "484013999", "484014999", "484015999", "484016999", "484017999", "484018999",
        "484019999", "484020999", "484021999", "484022999", "484023999", "484024999",
        "484025999", "484026999", "484027999", "484028999", "484029999", "484030999"
      ),
      NA_character_,
      geolevel2
    )
  )

# Remove unprocessed data to free up space
rm(census_data_raw)
gc()

fwrite(census_data, "Data/safe/census_data.csv")
cat("Processed and saved census data\n")

#### Process census data controls ####

# Get unique geolevel2 values
unique_geolevels <- unique(census_data$geolevel2)
  
# Process in chunks
chunk_size <- 1  # Adjust based on your memory
n_chunks <- ceiling(length(unique_geolevels) / chunk_size)

results_list <- list()

for(i in 1:n_chunks) {
  start_idx <- (i-1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(unique_geolevels))

  geolevels_chunk <- unique_geolevels[start_idx:end_idx]

  chunk_result <- census_data %>%
    filter(geolevel2 %in% geolevels_chunk) %>%  
    select(year, geolevel2, incearn, popdensgeo2, age, sex, lit, yrschool, mig2_5_mx) %>%
    mutate(
      # Recode data to indicate non-valid entries as NAs - do all at once
      incearn = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
      age = ifelse(age == 999, NA_real_, age),
      sex = ifelse(sex == 9, NA_real_, ifelse(sex == 2, 1, 0)),
      lit = ifelse(lit %in% c(0, 9), NA_real_, ifelse(lit == 2, 1, 0)),
      yrschool = as.integer(yrschool)
    ) %>%
    group_by(geolevel2) %>%
    summarise(
      mean_inc = mean(incearn, na.rm = TRUE),
      popdensgeo2 = mean(popdensgeo2, na.rm = TRUE),
      mean_age = mean(age, na.rm = TRUE),
      rate_female = mean(sex, na.rm = TRUE),
      rate_literacy = mean(lit, na.rm = TRUE),
      mean_yrschool = mean(yrschool, na.rm = TRUE),
      .groups = "drop"
    )
    
  results_list[[i]] <- chunk_result
  gc()  # Force garbage collection after each chunk
    
  cat("Processed chunk", i, "of", n_chunks, "\n")
}
  
# Combine all results
census_data_controls <- bind_rows(results_list)

# Save to file
fwrite(census_data_controls, "Data/safe/census_data_controls.csv")
cat("Processed and saved census control data\n")