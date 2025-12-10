rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "stars", "terra") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

base <- rast("./data/GEBCO_2020.nc") # Import bathymetry

coast <- read_stars("./data/GEBCO_2020_TID.nc") %>%
    .[st_bbox(as(extent(-110, 120, 30, 89), "SpatialPolygons"))] %>%
    st_as_stars(downsample = 3) %>%
    st_as_sf(as_points = FALSE, merge = TRUE) %>%
    filter(tid == 0) %>% # Land is coded as 0
    rename(TID = GEBCO_2020_TID.nc) %>%
    st_cast("LINESTRING")

st_crs(coast) <- crs(base)

coast <- st_transform(coast, 3035)

plot(coast)

saveRDS(coast, "./cache/coast.rds")
