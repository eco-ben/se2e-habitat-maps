#### Set up ####

rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "terra", "stars") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

base <- rast("./data/GEBCO_2020.nc") # Import bathymetry

domain <- readRDS("./data/Greenland Sea Habitats.rds") %>%
    st_transform(crs = 3035)

buffer <- summarise(domain, area = mean(st_area(domain))) %>%
    st_buffer(dist = 230000)

ggplot() +
    geom_sf(data = buffer) +
    geom_sf(data = domain, colour = "yellow")

window <- st_bbox(buffer)
window_extent <- window %>%
    ext()

clip <- # st_bbox(st_transform(buffer, 4326)) %>%
    #  .[c("xmin", "xmax", "ymin", "ymax")] %>%
    #  as.numeric() %>%
    c(-40, 100, 55, 89) %>%
    #  c(0, 96, 55, 83) %>%
    #  c(0, 97.7, 55.47, 82.9) %>%
    #  c(0, 98, 55.4, 83) %>%
    ext()

base <- crop(base, clip) # Crop bathymetry

plot(base)

line <- as.contour(base, levels = c(-400)) %>%
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

overlay <- mutate(domain, class = case_when(
    Habitat == "Rock" ~ 0,
    Habitat == "Gravel" ~ 3,
    Habitat == "Sand" ~ 2,
    Habitat == "Silt" ~ 1
)) %>%
    mutate(class = case_when(
        Shore == "Inshore" ~ paste0("S", class),
        T ~ paste0("D", class)
    ))

G_Y2 <- c(
    S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
    D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd"
)

ggplot() +
    geom_sf(data = star2, fill = "black", size = 0) +
    geom_sf(data = overlay, aes(fill = class), size = 0.05, colour = "white") +
    geom_sf(data = line) +
    scale_fill_manual(values = G_Y2) +
    coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
    theme_minimal() +
    theme(
        text = element_text(, size = 9),
        legend.position = c(0.87, 0.09),
        #  legend.key = element_rect(colour = NA),
        legend.key.size = unit(0.3, "cm")
    ) +
    guides(fill = guide_legend(
        ncol = 2, title.hjust = 0.5, title.position = "left",
        title.theme = element_text(angle = 90, size = 8, colour = "white"),
        label.theme = element_text(size = 6, colour = "white"),
        #                             override.aes = list(colour = NA))) +
        override.aes = list(colour = "black")
    )) +
    labs(
        fill = "Habitats",
        subtitle = "The Greenland Sea Model Domain",
        x = NULL, y = NULL
    ) +
    annotate("text",
        label = "400 m", x = window["xmin"] + 220000, y = window["ymin"] + 70000,
        vjust = 0, hjust = 0, angle = 45, size = 3
    ) +
    NULL

ggsave("Greenland.png", width = 11, height = 11, units = "cm", dpi = 500)
