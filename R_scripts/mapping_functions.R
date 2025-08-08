rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "terra", "stars") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages


prepare_roberts_reg_domains <- function(roberts_regions_fn, region_name) {
    data <- read.csv(roberts_regions_fn) %>%
        dplyr::select(-Zone) %>%
        mutate(Code = as.numeric(as.factor(Type))) %>%
        split(f = .$Region)

    labels <- distinct(data[[region_name]][, c("Code", "Type")])

    domain <- rast(data[[region_name]][, c(2, 3, 5)], digits = 3) %>%
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

    return(domain)
}

create_domain_mapping_data <- function(domain_data_fn, region_name, buffer_dist, contour_depth) {
    if (str_detect(domain_data_fn, "roberts_regions.csv")) {
        domain <- prepare_roberts_reg_domains(domain_data_fn, region_name)
    } else {
        domain <- readRDS(domain_data_fn)
    }

    # domain_crs <- st_crs(domain)

    buffer <- summarise(domain, area = mean(st_area(domain))) %>%
        st_buffer(dist = buffer_dist)

    # Transform all data to EPSG:4326 for consistency
    buffer <- buffer %>% st_transform(crs = 4326)
    domain <- domain %>% st_transform(crs = 4326)

    window <- st_bbox(buffer)
    window_extent <- window %>%
        ext()

    clip <- st_bbox(buffer) %>%
        .[c("xmin", "xmax", "ymin", "ymax")] %>%
        as.numeric() %>%
        ext()

    base <- rast("./data/GEBCO_2020.nc") # Import bathymetry
    # if (crs(base) != domain_crs) {
    #     base <- project(base, domain)
    # }
    base <- crop(base, clip) # Crop bathymetry

    line <- as.contour(base, levels = c(contour_depth)) %>%
        st_as_sf()

    if (!is.na(st_crs(domain))) {
        line <- st_transform(line, crs = st_crs(domain))
    } else {
        st_crs(domain) <- st_crs(line)
    }

    star <- base
    star[star < 0] <- NA
    star[is.finite(star)] <- 0

    land <- st_as_stars(star) %>%
        st_as_sf(as_points = FALSE, merge = TRUE)

    if (!("Class" %in% names(domain))) {
        domain <- domain %>%
            mutate(Class = paste0(Habitat, Shore)) %>%
            mutate(Class = case_when(
                Class == "RockInshore" ~ "S0",
                Class %in% c("SiltInshore", "MudInshore") ~ "S1",
                Class == "SandInshore" ~ "S2",
                Class == "GravelInshore" ~ "S3",
                Class == "RockOffshore" ~ "D0",
                Class %in% c("SiltOffshore", "MudInshore") ~ "D1",
                Class == "SandOffshore" ~ "D2",
                Class == "GravelOffshore" ~ "D3",
                Class == "OverhangOffshore" ~ "DO"
            ))
    }

    return(
        list("domain" = domain, "land" = land, "contour" = line)
    )
}

create_rock_mapping_data <- function(rock_raster_fn = "./data/roberts_rock.csv", domain) {
    rock <- read.csv(rock_raster_fn) %>%
        rast() %>%
        st_as_stars()

    st_crs(rock) <- st_crs(domain)

    rock <- rock[domain]

    rock[[1]][rock[[1]] == 0] <- NA

    return(rock)
}

compile_mapping_data <- function(domain_data_fn, region_name, b_dist, contour_d) {
    mapping_data <- create_domain_mapping_data(domain_data_fn, region_name, b_dist, contour_d)

    if (region_name %in% roberts_regions) {
        rock_data <- create_rock_mapping_data(domain = mapping_data[["domain"]])
        return(c(mapping_data, list("rock" = rock_data)))
    }

    return(mapping_data)
}

plot_implementation_doc_map <- function(region_name, buffer_dist, contour_depth) {
    mapping_data <- compile_mapping_data(region_name, buffer_dist, contour_depth)

    G_Y2 <- c(
        S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
        D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd"
    )

    hab_p <- ggplot() +
        geom_sf(data = land, fill = "black", size = 0) +
        geom_sf(data = domain, aes(fill = Class), size = 0.05, colour = "white") +
        geom_sf(data = land, fill = "black", size = 0) +
        geom_sf(data = line, colour = "grey69") +
        scale_fill_manual(values = G_Y2) +
        coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
        theme_minimal() +
        theme(
            text = element_text(size = 9),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            legend.position = c(0.15, 0.07),
            legend.key.size = unit(0.3, "cm")
        ) +
        guides(fill = guide_legend(
            ncol = 2, title.hjust = 0.5, title.position = "left",
            title.theme = element_text(angle = 90, size = 8, colour = "black"),
            label.theme = element_text(size = 6, colour = "black"),
            override.aes = list(colour = "black")
        )) +
        labs(
            fill = "Habitats",
            #       subtitle = "The Celtic Sea Model Domain",
            x = NULL, y = NULL
        ) +
        annotate("text",
            label = "200 m", x = window["xmin"] + 1, y = window["ymax"] - 2,
            vjust = 0, hjust = 0, angle = 45, size = 3, colour = "grey69"
        ) +
        NULL
}
