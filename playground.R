library(tidyverse)
library(data.table)

# Movement Data
movement_data <- read_csv("Data/Meta Movement Distribution/movement-distribution-data-for-good-at-meta_2023-05-01_2023-06-01_csv/1922039342088483_2023-05-01.csv")

movement_data_mex <- movement_data %>%
  filter(country == "MEX")


enadid_2023 <- read_csv("Data/Mexico/ENADID/base_datos_enadid23_csv/TMIGRANTE.csv")


