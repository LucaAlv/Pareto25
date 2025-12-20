library(tidyverse)

disaster_data_pre <- readRDS("Data/temp/disaster_data.rds")
disaster_data_post <- readRDS("")

disaster_plot <- ggplot(mun_nodes_sf) +
  geom_sf(aes(fill = total_disasters_period)) +
  facet_wrap(~ year_census) +
  scale_fill_distiller(palette="RdYlGn") +
  labs(title = "Disasters by Municipality",
       x = "Municipality",
       y = "Number of Disasters")
ggsave("Output/Plots/disaster_plot.png", disaster_plot, dpi = 700)

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