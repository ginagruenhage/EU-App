require(plyr)
require(stringr)
library(rworldmap)
library(cmdsr)

source("helpers.R")

info.df <- read.table("data/df.csv")
Ds <- compute.dmat(info.df)

DistL <- compute.Dlist(Ds, T = 10, convex = FALSE)

res <- cmds(DistL, k = 2)

embed <- make.df(res,info.df)

embed %>% ggvis(~cmds.x1, ~cmds.x2) %>% layer_points(size := 10, opacity := 0.3, fill := mycols[1]) %>%
  add_tooltip(function(df) df$iso)
