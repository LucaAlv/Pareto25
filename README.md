# Pareto25

Create mun_nodes dataset:
1. Run census_data_preprocessing.R
-> If this crashes because RAM is exceeded, lower chunk_size in line 97 (1 is lowest)
3. Run crime_preprocessing.R
4. Run disaster_preprocessing.R
5. Run nodes_edges.R

TWFE models:
Run fe_models

Census Tables (including summary tables):
Run census_plots.R

Disaster Graphs:
Run disaster_plots.R
