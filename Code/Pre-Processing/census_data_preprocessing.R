######## This file does all preprocessing of the Census Data ########

#### Load packages ####

library(tidyverse)
library(ipumsr)
library(data.table)

#### Load Census Data ####

census_ddi <- read_ipums_ddi("Data/IPUMS/Census_Data/ipumsi_00002.xml")
census_data_raw <- read_ipums_micro(census_ddi)

state_nodes_preprocess <- census_data_raw %>%
  janitor::clean_names() %>%
  select(perwt, hhwt, geolev1, year, incearn, popdensgeo2, areamollwgeo2) %>%
  mutate(
    popdensgeo2 = ifelse(popdensgeo2 == 0, NA_real_, popdensgeo2),
    areamollwgeo2 = ifelse(areamollwgeo2 == 0, NA_real_, areamollwgeo2),
    incearn = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
    mun_pop = sum(perwt, na.rm = TRUE)
  ) %>%
  group_by(geolev1, year) %>%
  summarise(
    mean_inc = weighted.mean(incearn, perwt, na.rm = TRUE),
    state_pop = sum(mun_pop),
    .groups = "drop"
  )

saveRDS(state_nodes_preprocess, "Data/temp/state_nodes_preprocess.rds")
cat("Processed and saved pre process state level census data\n")
rm(state_nodes_preprocess)
gc()

#### Formatting NAs correctly for geolevel2 ####
na_pre_geolevel2 <- sum(is.na(census_data_raw$GEOLEV2))
na_pre_mig2_5_mx <- sum(is.na(census_data_raw$MIG2_5_MX))

cat("There are ", na_pre_geolevel2, " NAs in geolevel2 in the raw data\n")
cat("There are ", na_pre_mig2_5_mx, " NAs in mig2_5_mx in the raw data\n")

census_data <- census_data_raw %>%
  janitor::clean_names() %>%
  rename(geolevel2 = geolev2, geolevel1 = geolev1) %>%
  select(year, perwt, hhwt, geolevel1, geolevel2, incearn, popdensgeo2, age, sex, lit, yrschool, geomig1_5, mig1_5_mx, mig2_5_mx, areamollwgeo2) %>%
  mutate(
    geolevel2 = as.character(geolevel2),
    geolevel2 = if_else(
      # Each of the following municipality codes gives information about state but not in which municipality a person was in
      geolevel2 %in% c(
        "484001999", "484002999", "484003999", "484004999", "484005999", "484006999",
        "484007999", "484008999", "484009999", "484010999", "484011999", "484012999",
        "484013999", "484014999", "484015999", "484016999", "484017999", "484018999",
        "484019999", "484020999", "484021999", "484022999", "484023999", "484024999",
        "484025999", "484026999", "484027999", "484028999", "484029999", "484030999"
      ),
      NA_character_,
      geolevel2
    ),
    mig2_5_mx = as.character(mig2_5_mx),
    mig2_5_mx = if_else(
      mig2_5_mx %in% c(
      # For every state 4840xx998 decodes the situation where state is known but municipality is not. 484097997 decodes migration abroad, 484097998 and 484097999 and NAs
      "484001998", "484002998", "484003998", "484004998", "484005998", "484006998",
      "484007998", "484008998", "484009998", "484010998", "484011998", "484012998",
      "484013998", "484014998", "484015998", "484016998", "484017998", "484018998",
      "484019998", "484020998", "484021998", "484022998", "484023998", "484024998",
      "484025998", "484026998", "484027998", "484028998", "484029998", "484030998",
      "484031998", "484032999", "484097997", "484098998", "484099999"
      ),
      NA_character_,
      mig2_5_mx
    )
  )

na_post_geolevel2 <- sum(is.na(census_data$geolevel2))
na_post_mig2_5_mx <- sum(is.na(census_data$mig2_5_mx))

cat("There are ", na_post_geolevel2, " NAs in geolevel2 in the transformed data\n")
cat("There are ", na_post_mig2_5_mx, " NAs in mig2_5_mx in the transformed data\n")
cat("Transformation added ", na_post_geolevel2 - na_pre_geolevel2, "NAs in geolevel2\n")
cat("Transformation added ", na_post_mig2_5_mx - na_pre_mig2_5_mx, "NAs in mig2_5_mx\n")

rm(census_data_raw)
gc()
# saveRDS(census_data, "Data/temp/census_data.rds")
fwrite(census_data, "Data/temp/census_data.csv")
cat("Processed and saved census data\n")
gc()

#### Process census data controls for municipality node dataset ####

# Get unique geolevel2 values
unique_geolevels <- unique(census_data$geolevel2)
  
# Process in chunks
chunk_size <- 100  # Adjust based on memory - i.e. if R crashes because working memory is not enough, lower this number!
n_chunks <- ceiling(length(unique_geolevels) / chunk_size)

results_list <- list()

for(i in 1:n_chunks) {
  start_idx <- (i-1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(unique_geolevels))

  geolevels_chunk <- unique_geolevels[start_idx:end_idx]

  chunk_result <- census_data %>%
    filter(geolevel2 %in% geolevels_chunk) %>%  
    mutate(
      # Recode data to indicate non-valid entries as NAs
      incearn_clean = ifelse(incearn %in% c(99999999, 99999998), NA_real_, incearn),
      # Add topcode indicator for income
      incearn_topcode = ifelse(!is.na(incearn_clean) & incearn == 1000000, 1, 0),
      incearn_log1p = ifelse(is.na(incearn_clean), NA_real_, log1p(incearn_clean)),
      age = ifelse(age == 999, NA_real_, age),
      sex = ifelse(sex == 9, NA_real_, ifelse(sex == 2, 1, 0)),
      lit = ifelse(lit %in% c(0, 9), NA_real_, ifelse(lit == 2, 1, 0)),
      yrschool = ifelse(yrschool %in% c(90, 91, 92, 93, 94, 95, 96, 97, 98, 99), NA_real_, yrschool)
    ) %>%
    group_by(geolevel2, year) %>%
    summarise(
      n = n(),
      geolevel1 = first(geolevel1),
      mean_inc = weighted.mean(incearn_clean, perwt, na.rm = TRUE),
      mean_incearn_log1p = weighted.mean(incearn_log1p, perwt, na.rm = TRUE),
      share_incearn_topcode = mean(incearn_topcode[!is.na(incearn_clean)], na.rm = TRUE),
      popdensgeo2 = max(popdensgeo2, na.rm = TRUE),
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