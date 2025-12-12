######################## IBTrACS ########################

ibtracs <- read_csv("Data/Weather Data/IBTrACS/ibtracs.since1980.list.v04r01.csv")

ibtracs_shape_lines <- st_read("Data/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.lines")
ibtracs_shape_points <- st_read("Data/Weather Data/IBTrACS/IBTrACS.since1980.list.v04r01.points")
ibtracs_shape_points_edit <- ibtracs_shape_points %>%
  janitor::clean_names() %>%
  filter(basin == "EP"| basin == "NA") %>%
  filter(season >= 2000) %>%
  select(
    sid, season, number, basin, subbasin, name, iso_time, nature, lat, lon,
    wmo_wind, wmo_pres, track_type, dist2land, landfall, iflag, storm_spd, storm_dr, year, month, day, hour, min, geometry
  ) %>%
  arrange(sid, iso_time) %>%         # sort within each storm
  group_by(sid) %>%
  summarise(do_union = FALSE) %>%    # keep separate geometries per storm
  st_cast("LINESTRING")
