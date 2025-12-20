library(tidyverse)
library(fixest)

#### Load data ####

od_edges <- readRDS("Data/temp/od_edges.rds")

# mig2_5_mx = origin = .x controls = disaster data
# geolevel2 = destination = .y controls

#### Regressions ####

# compare where migrants from the same origin go in the same period, 
# netting out all origin push factors and all destination pull factors.
m1 <- fepois(
  num_migrants ~ 1 | mig2_5_mx^year_census + geolevel2^year_census,
  data = od_edges,
  vcov = ~ mig2_5_mx
)

summary(m1)
