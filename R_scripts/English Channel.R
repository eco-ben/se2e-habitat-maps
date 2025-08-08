#### Set up ####

rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "stars", "patchwork") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

data <- read.csv("./data/roberts_regions.csv") %>%
    dplyr::select(-Zone) %>%
    mutate(Code = as.numeric(as.factor(Type))) %>%
    split(f = .$Region)

labels <- distinct(data[["English Channel"]][, c("Code", "Type")])

domain <- rasterFromXYZ(data[["English Channel"]][, c(2, 3, 5)], digits = 3) %>%
    st_as_stars() %>%
    st_as_sf(as_points = F, merge = T) %>%
    group_by(Code) %>%
    summarise(Code = mean(Code)) %>%
    left_join(labels) %>%
    transmute(Class = case_when(
        Type == "Offshore Fine" ~ "D1",
        Type == "Offshore Medium" ~ "D2",
        Type == "Offshore Coarse" ~ "D3",
        Type == "Inshore Fine" ~ "S1",
        Type == "Inshore Medium" ~ "S2",
        Type == "Inshore Coarse" ~ "S3"
    ))

buffer <- summarise(domain, area = mean(st_area(domain))) %>%
    st_buffer(dist = 1)

ggplot() +
    geom_sf(data = buffer) +
    geom_sf(data = domain, colour = "yellow")

window <- st_bbox(buffer)

base <- rast("../../Barents Sea/Data/GEBCO_2019.nc") # Import bathymetry

st_crs(domain) <- crs(base)

# clip <-st_bbox(buffer) %>%
#   .[c("xmin", "xmax", "ymin", "ymax")] %>%
#   as.numeric() %>%
#   extent() %>%
#   as("SpatialPolygons")
#
# crs(clip) <- crs(base)                                                         # Match crs to bathymetry
#
# base <- crop(base, clip)                                                       # Crop bathymetry
#
# plot(base)
#
# line <- rasterToContour(base, levels = c(-400)) %>%
#   st_as_sf()
#
# ggplot() +
#   geom_sf(data = line) +
#   geom_sf(data = domain, fill = "red")

land <- read_stars("./data/GEBCO_2020_TID.nc") %>%
    st_as_stars() %>%
    .[window] %>%
    st_as_sf(as_points = FALSE, merge = TRUE) %>%
    filter(GEBCO_2020_TID.nc == 0) %>% # Land is coded as 0
    rename(TID = GEBCO_2020_TID.nc)

st_crs(land) <- crs(base)

#### Plotting ####

G_Y2 <- c(
    S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
    D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd"
)

hab_p <- ggplot() +
    geom_sf(data = land, fill = "black", colour = "black", size = 0.25) +
    geom_sf(data = domain, aes(fill = Class), size = 0.05, colour = "white") +
    geom_sf(data = land, fill = "black", colour = "black", size = 0) +
    #  geom_sf(data = line) +
    scale_fill_manual(values = G_Y2) +
    coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
    theme_minimal() +
    theme(
        text = element_text(, size = 9),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(0.84, 0.13),
        legend.key.size = unit(0.3, "cm")
    ) +
    guides(fill = guide_legend(
        ncol = 2, title.hjust = 0.5, title.position = "left",
        title.theme = element_text(angle = 90, size = 8, colour = "white"),
        label.theme = element_text(size = 6, colour = "white"),
        override.aes = list(colour = "white")
    )) +
    labs(
        fill = "Habitats",
        #       subtitle = "The English Channel Model Domain",
        x = NULL, y = NULL
    ) +
    #  annotate("text", label = "400 m", x = window["xmin"]+ 1 , y = window["ymax"]- 2,
    #           vjust = 0, hjust = 0, angle = 45, size = 3) +
    NULL

hab_p

#### Save sf object ####

domain %>%
    dplyr::select(Habitat = Class) %>%
    saveRDS("./cache/Channel Habitats.rds")

#### Add Rock ####

rock <- read.csv("./data/roberts_rock.csv") %>%
    rasterFromXYZ() %>%
    st_as_stars()

st_crs(rock) <- st_crs(domain)

rock <- rock[domain]

rock[[1]][rock[[1]] == 0] <- NA # Replace 0s with NA

as.data.frame(rock) %>%
    drop_na() %>%
    setNames(c("Longitude", "Latitude", "Rock")) %>%
    saveRDS("./cache/Channel Rock.rds")

rock_p <- ggplot() +
    geom_sf(data = land, fill = "black", colour = "black", size = 0.25) +
    geom_sf(data = domain, fill = "white", colour = "white") +
    geom_stars(data = rock) +
    geom_sf(data = domain, fill = NA, colour = "#284481", size = 0.05) +
    geom_sf(data = land, fill = "black", colour = "black", size = 0) +
    #  geom_sf(data = line) +
    scale_fill_gradient(low = "white", high = "#8c17ae", na.value = NA, labels = scales::percent) +
    coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
    theme_minimal() +
    theme(
        text = element_text(, size = 9),
        legend.position = c(0.84, 0.13),
        legend.key.size = unit(0.3, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_blank()
    ) +
    guides(fill = guide_colourbar(
        title.position = "left", title.hjust = 0.5,
        title.theme = element_text(angle = 90, size = 8, colour = "white"),
        label.theme = element_text(size = 6, colour = "white"),
        barheight = 2, barwidth = 0.5,
        frame.colour = "black", ticks.colour = "black",
        override.aes = list(colour = "white")
    )) +
    labs(
        fill = "Rock",
        x = NULL, y = NULL
    ) +
    #  annotate("text", label = "200 m", x = window["xmin"]+ 1 , y = window["ymax"]- 2,
    #           vjust = 0, hjust = 0, angle = 45, size = 3) +
    NULL

rock_p

hab_p + rock_p

ggsave("./img/Channel.png", width = 16, height = 8, units = "cm", dpi = 500)
