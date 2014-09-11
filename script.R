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

alpha.val = 0

limits <- compute.limits(embed)

vis <- plot.timestep(embed,alpha.val,limits)

print(vis)
