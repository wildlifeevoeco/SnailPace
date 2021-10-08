library(targets)

library(amt)
library(data.table)
library(sf)
library(sp)
library(ggplot2)
library(glmmTMB)
library(raster)
library(broom.mixed)
library(patchwork)

library(conflicted)

conflict_prefer('shift', 'data.table')
