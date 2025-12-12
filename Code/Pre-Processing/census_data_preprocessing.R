######## This file does all preprocessing of the Census Data ########

#### Load packages ####

library(tidyverse)
library(ipumsr)

#### Load Census Data ####

census_ddi <- read_ipums_ddi("Data/IPUMS/Census_Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi)

state_nodes_preprocess <- census_data_raw %>%
  janitor::clean_names() %>%
  select(perwt, hhwt, geolev1, year, incearn, popdensgeo2, areamollwgeo2) %>%
  filter(year == 2020) %>%
  mutate(
    popdensgeo2 = ifelse(popdensgeo2 == 0, NA_real_, popdensgeo2),
    areamollwgeo2 = ifelse(areamollwgeo2 == 0, NA_real_, areamollwgeo2),
    incearn = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
    mun_pop = sum(perwt, na.rm = TRUE)
  ) %>%
  group_by(geolev1, year) %>%
  summarise(
    mean_inc = weighted.mean(incearn, perwt, na.rm = TRUE),
    mun_pop = first(mun_pop),
    state_pop = sum(mun_pop),
    .groups = "drop"
  )

saveRDS(state_nodes_preprocess, "Data/temp/state_nodes_preprocess")
cat("Processed and saved pre process state level census data\n")
rm(state_nodes_preprocess)
gc()

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

rm(census_data_raw)
saveRDS(census_data, "Data/temp/census_data.rds")
cat("Processed and saved census data\n")
gc()

#### Process census data controls ####

# Get unique geolevel2 values
unique_geolevels <- unique(census_data$geolevel2)
  
# Process in chunks
chunk_size <- 10  # Adjust based on your memory
n_chunks <- ceiling(length(unique_geolevels) / chunk_size)

results_list <- list()

for(i in 1:n_chunks) {
  start_idx <- (i-1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(unique_geolevels))

  geolevels_chunk <- unique_geolevels[start_idx:end_idx]

  chunk_result <- census_data %>%
    filter(geolevel2 %in% geolevels_chunk) %>%  
    select(year, perwt, hhwt, geolevel1, geolevel2, incearn, popdensgeo2, age, sex, lit, yrschool, mig2_5_mx, areamollwgeo2) %>%
    mutate(
      # Recode data to indicate non-valid entries as NAs - do all at once
      incearn = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
      age = ifelse(age == 999, NA_real_, age),
      sex = ifelse(sex == 9, NA_real_, ifelse(sex == 2, 1, 0)),
      lit = ifelse(lit %in% c(0, 9), NA_real_, ifelse(lit == 2, 1, 0)),
      yrschool = as.integer(yrschool),
      popdensgeo2 = ifelse(popdensgeo2 == 0, NA_real_, popdensgeo2),
    ) %>%
    group_by(geolevel2, year) %>%
    summarise(
      geolevel1 = first(geolevel1),
      mean_inc = weighted.mean(incearn, perwt, na.rm = TRUE),
      popdensgeo2 = first(popdensgeo2),
      mean_age = weighted.mean(age, perwt, na.rm = TRUE),
      rate_female = weighted.mean(sex, perwt, na.rm = TRUE),
      rate_literacy = weighted.mean(lit, perwt, na.rm = TRUE),
      mean_yrschool = weighted.mean(yrschool, perwt, na.rm = TRUE),
      mun_pop = sum(perwt, na.rm = TRUE),
      .groups = "drop"
    )
    
  results_list[[i]] <- chunk_result
  gc()  # Force garbage collection after each chunk
    
  cat("Processed chunk", i, "of", n_chunks, "\n")
}
  
# Combine all results
census_data_controls <- bind_rows(results_list)

# Save to file

rm(census_data)
gc()

fwrite(census_data_controls, "Data/temp/census_data_controls.csv")
saveRDS(census_data_controls, "Data/temp/census_data_controls.rds")
cat("Processed and saved census control data\n")