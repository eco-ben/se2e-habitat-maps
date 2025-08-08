
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "stars")                           # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

theme_BES <- function(){ 
  
  theme(text = element_blank(),
#        axis.title = element_text(size = 14),
#        axis.text = element_text(colour = "white", size = 12),
#        legend.text=element_text(size = 10),
        
        legend.background = element_blank(), 
        legend.key = element_blank(),
        legend.position = "top",
        
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),  
        
        axis.line.x.bottom= element_blank(),
        axis.line.y.left = element_blank(),
        axis.ticks = element_blank())
  
}


base <- raster("../../Barents Sea/Data/GEBCO_2019.nc")                      # Import bathymetry

domain <- readRDS("./data/Greenland Sea Habitats.rds") %>% 
  st_transform(crs = 3035) %>% 
  st_union() %>% 
  sfheaders::sf_remove_holes()

clip <- c(-75, 5, 59, 87) %>% 
  extent() %>% 
  as("SpatialPolygons")
crs(clip) <- crs(base)                                                         # Match crs to bathymetry

base <- crop(base, clip)                                                       # Crop bathymetry

plot(base)

star <- base
star[star < 0] <- NA
star[is.finite(star)] <- 0

plot(star)

star2 <- st_as_stars(star) %>% 
  st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  st_transform(crs = 3035)

#### Plotting ####

ggplot() +
  geom_sf(data = domain, fill = "yellow", colour = NA) +
  geom_sf(data = star2, fill = "white", colour = NA) +
  theme_BES() +
  labs(x = NULL, y = NULL) +
  NULL

ggsave("./img/Greenland BES22.svg", width = 21, height = 11, units = "cm", bg = "transparent")
