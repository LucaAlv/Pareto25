# Pareto25

Create mun_nodes dataset:
1. Run census_data_preprocessing.R - requires IPUMS Data
-> If this crashes because RAM is exceeded, lower chunk_size in line 97 (1 is lowest)
3. Run crime_preprocessing.R - requires Crime Data
4. Run disaster_preprocessing.R - requires Weather/Atlas-Disaster-Indicator Data
5. Run nodes_edges.R -  requires IPUMS Data

TWFE models:
Run fe_models

Census Tables (including summary tables):
Run census_plots.R

Disaster Graphs:
Run disaster_plots.R

Unfortunately the files are too large for Github, so I put them in a google drive folder:
https://drive.google.com/drive/folders/1OY_8ssKf8NFMBCCpFIcISMnxJ-ATF3ux?usp=drive_link

Alternatively all IPUMS Data can be downloaded from IPUMS International: 
https://international.ipums.org/international/

IPUMS shapefiles are available from:
https://international.ipums.org/international/gis_harmonized_2nd.shtml

Crime Data is available from:
https://www.inegi.org.mx/programas/edr/#microdatos

The Atlas Disaster Data is available from:
http://www.atlasnacionalderiesgos.gob.mx/apps/Declaratorias/
