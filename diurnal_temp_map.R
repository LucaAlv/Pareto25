######################## Diurnal Temperature data ########################
ceda_temp_data_world <- rast("Data/Weather Data/CEDA_Temperature/cru_ts4.09.2001.2010.dtr.dat.nc")

### Visualizing grid data

# 1. Read the NetCDF file
# Replace with your actual filename
nc_file <- "Data/Weather Data/CEDA_Temperature/cru_ts4.09.2001.2010.dtr.dat.nc"  # or whatever your file is named
nc_data <- nc_open(nc_file)

# Examine the structure
print(nc_data)

# 2. Convert to raster
dtr_raster <- brick(nc_file, varname = "dtr")

# If you want a specific time period (e.g., most recent year or average)
# For a single time slice:
dtr_single <- raster(nc_file, varname = "dtr", band = 1)

# Or calculate the mean across all time periods:
dtr_mean <- mean(dtr_raster, na.rm = TRUE)

# 3. Get Mexico boundary
mexico <- ne_countries(country = "mexico", scale = "medium", returnclass = "sf")

# 4. Crop and mask to Mexico
dtr_mexico <- crop(dtr_mean, mexico)
dtr_mexico <- mask(dtr_mexico, mexico)

# 5. Convert to data frame for ggplot
dtr_df <- as.data.frame(dtr_mexico, xy = TRUE)
colnames(dtr_df) <- c("lon", "lat", "dtr")

# 6. Create the visualization
ggplot() +
  geom_raster(data = dtr_df, aes(x = lon, y = lat, fill = dtr)) +
  geom_sf(data = mexico, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis(option = "plasma", na.value = "transparent",
                     name = "DTR (°C)") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Diurnal Temperature Range in Mexico",
       subtitle = "Based on CRU TS 4.09 data",
       x = "Longitude", y = "Latitude")

### Merging CEDA data with geolevel2 codes