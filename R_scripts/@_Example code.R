#### Import data ####

habitats <- readRDS("./cache/North Sea Habitats.rds") # Load the habitat polygon

rock <- readRDS("./cache/North Sea Rock.rds")

#### Tidyverse ####

library(ggplot2)
ggplot(data = habitats) +
    geom_sf() # ggplot can handle sf objects

ggplot(data = rock) + # and can plot rasters directly from dataframes
    geom_rast(aes(x = Longitude, y = Latitude, fill = Rock))

#### Base R ####

library(sf) # Load sf to plot polygons quickly
plot(habitats)

library(raster) # Load raster to plot raster quickly
plot(rasterFromXYZ(rock))
