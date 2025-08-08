
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "stars")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

base <- raster("../../Barents Sea/Data/GEBCO_2019.nc")                      # Import bathymetry

domain <- readRDS("./data/Norwegian Shelf Domain.rds") 

buffer <- summarise(domain, area = mean(st_area(domain))) %>% 
  st_buffer(dist = 230000)

ggplot() +
  geom_sf(data = buffer) +
  geom_sf(data = domain, colour = "yellow")

window <- st_bbox(buffer)

clip <- c(-10, 25, 55.4, 75) %>% 
  extent() %>% 
  as("SpatialPolygons")
crs(clip) <- crs(base)                                                         # Match crs to bathymetry

base <- crop(base, clip)                                                       # Crop bathymetry

plot(base)

line <- rasterToContour(base, levels = c(-600)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 3035)

ggplot() +
  geom_sf(data = line) +
  geom_sf(data = domain, fill = "red")

star <- base
star[star < 0] <- NA
star[is.finite(star)] <- 0

plot(star)

star2 <- st_as_stars(star) %>% 
  st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  st_transform(crs = 3035)

#### Plotting ####

overlay <- domain

#G_Y2 <- c(S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
#          D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd")

G_Y2 <- c(Inshore = "#9097CC", Offshore = "#FFD25F")

ggplot() +
  geom_sf(data = overlay, aes(fill = Shore), size = 0.05, colour = "white") +
  geom_sf(data = star2, fill = "black", size = 0) +
  geom_sf(data = line, colour = "grey69") +
  scale_fill_manual(values = G_Y2) +
  coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
  theme_minimal() +
  theme(text = element_text(family = "Avenir", size = 10),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(0.75, 0.09),
        #  legend.key = element_rect(colour = NA),
        legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_legend(ncol = 2, title.hjust = 0.5, title.position = "top",
                             title.theme = element_text(angle = 0, size = 8, colour = "white"),
                             label.theme = element_text(size = 6, colour = "white"),
                             #                             override.aes = list(colour = NA))) +
                             override.aes = list(colour = "black"))) +
  labs(fill = "Zones",
       x = NULL, y = NULL) +
  annotate("text", label = "600 m", x = window["xmin"]+ 130000 , y = window["ymin"]+ 300000, 
           vjust = 0, hjust = 0, angle = 30, size = 3, colour = "grey69") +
  NULL

#ggsave("./img/Norewgian_Shelf.png", width = 11, height = 11, units = "cm", dpi = 500, bg = "white")

#### App ####

ggplot() +
  geom_sf(data = overlay, aes(fill = Shore), size = 0.01, colour = "white") +
  geom_sf(data = star2, fill = "white", size = 0) +
  geom_sf(data = line, colour = "#B01313") +
  scale_fill_manual(values = G_Y2) +
  coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
  theme_minimal() +
  theme(text = element_text(family = "Avenir", size = 10, colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.border = element_rect(colour = "white", fill = NA, size = 1),
        legend.position = c(0.75, 0.09),
        #  legend.key = element_rect(colour = NA),
        legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_legend(ncol = 2, title.hjust = 0.5, title.position = "top",
                             title.theme = element_text(angle = 0, size = 8, colour = "black"),
                             label.theme = element_text(size = 6, colour = "black"),
                             #                             override.aes = list(colour = NA))) +
                             override.aes = list(colour = "white"))) +
  labs(fill = "Zones",
       x = NULL, y = NULL) +
  annotate("text", label = "600 m", x = window["xmin"]+ 130000 , y = window["ymin"]+ 300000, 
           vjust = 0, hjust = 0, angle = 30, size = 3, colour = "#B01313") +
  NULL

ggsave("./img/Norewgian_Shelf_app.png", width = 11, height = 11, units = "cm", dpi = 500)
