#### Set up ####

rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "terra", "stars") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

base <- rast("./data/GEBCO_2020.nc") # Import bathymetry

domain <- readRDS("./data/Mauritania-Senegal Domain.rds")

buffer <- summarise(domain, area = mean(st_area(domain))) %>%
    st_make_valid() %>%
    st_buffer(dist = 80000)

ggplot() +
    geom_sf(data = buffer) +
    geom_sf(data = domain, colour = "yellow")

window <- st_bbox(buffer)
window_extent <- window %>%
    ext()

clip <- st_bbox(buffer) %>%
    .[c("xmin", "xmax", "ymin", "ymax")] %>%
    as.numeric() %>%
    ext()

base <- crop(base, clip) # Crop bathymetry

plot(base)

line <- as.contour(base, levels = c(-900)) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(domain))

ggplot() +
    geom_sf(data = line) +
    geom_sf(data = domain, fill = "red")

star <- base
star[star < 0] <- NA
star[is.finite(star)] <- 0

plot(star)

star2 <- st_as_stars(star) %>%
    st_as_sf(as_points = FALSE, merge = TRUE) %>%
    st_transform(crs = st_crs(domain)) %>%
    sfheaders::sf_remove_holes()

#### Plotting ####

overlay <- domain

G_Y2 <- c(Inshore = "#9097CC", Offshore = "#FFD25F")

ggplot() +
    geom_sf(data = overlay, aes(fill = Shore), size = 0.05, colour = "white") +
    geom_sf(data = star2, fill = "black", size = 0) +
    geom_sf(data = line, colour = "grey69") +
    scale_fill_manual(values = G_Y2) +
    coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"] - 2, window["xmax"] + 2), expand = F) +
    theme_minimal() +
    theme(
        text = element_text(, size = 10),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(0.75, 0.35),
        #  legend.key = element_rect(colour = NA),
        legend.key.size = unit(0.3, "cm")
    ) +
    guides(fill = guide_legend(
        ncol = 2, title.hjust = 0.5, title.position = "top",
        title.theme = element_text(angle = 0, size = 8, colour = "white"),
        label.theme = element_text(size = 6, colour = "white"),
        #                             override.aes = list(colour = NA))) +
        override.aes = list(colour = "black")
    )) +
    labs(
        fill = "Zones",
        x = NULL, y = NULL
    ) +
    annotate("text",
        label = "900 m", x = window["xmin"] + .5, y = window["ymax"] - 2,
        vjust = 0, hjust = 0, angle = -45, size = 3, colour = "grey69"
    ) +
    NULL

ggsave("./outputs/Mauritania-Senegal.png", width = 11, height = 11, units = "cm", dpi = 500, bg = "white")
