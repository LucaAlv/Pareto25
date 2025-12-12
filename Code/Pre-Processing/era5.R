######################## ERA5 weather data ########################


# Extracting era5 weather data
era5_2020 <- rast("Data/Weather Data/ERA5/era5_2020_rain.grib")

# Construct index out of mean and standard deviation

# Aggregating monthly 
era5_2020_monthly <- tapp(era5_2020, "months", mean)
era5_2020_monthly_mun <- extract(era5_2020_monthly, inegi_shape_data, fun = mean, na.rm = TRUE)
mm <- rowMeans(era5_2020_monthly_mun, na.rm = TRUE)


