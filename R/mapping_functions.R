rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "terra", "stars", "patchwork", "docstring") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

roberts_regions <- c("Celtic Sea", "North Sea", "Clyde and Irish Sea", "English Channel", "West of Scotland")

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

safe_buffer <- function(domain, buffer_dist) {
    tryCatch(
        buffer <- summarise(st_make_valid(domain), area = mean(st_area(st_make_valid(domain)))) %>%
            st_buffer(dist = buffer_dist),
        error = function(e) {
            warning(
                "⚠ st_buffer() failed with s2 enabled: ", conditionMessage(e), "\n",
                "Retrying with s2 disabled (GEOS)."
            )
            sf_use_s2(FALSE)
            domain <- st_make_valid(domain)
            buffer <- summarise(domain, area = mean(st_area(domain))) %>%
                st_buffer(dist = buffer_dist)
            sf_use_s2(TRUE)
            return(buffer)
        }
    )
}

create_domain_mapping_data <- function(domain_data_fn, region_name, buffer_dist, contour_depth) {
    if (str_detect(domain_data_fn, "roberts_regions.csv")) {
        domain <- prepare_roberts_reg_domains(domain_data_fn, region_name)
        buffer <- summarise(domain, area = mean(st_area(domain))) %>%
            st_buffer(dist = buffer_dist)
        st_crs(domain) <- 4326
        st_crs(buffer) <- 4326
    } else {
        domain <- readRDS(domain_data_fn)
        buffer <- safe_buffer(domain, buffer_dist)
    }

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

    if ((!("Class" %in% names(domain))) && (("Habitat" %in% names(domain)))) { # Check if domain data is needing a class variable and has habitat data (combined habitat and shore).
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
    } else if ((!("Class" %in% names(domain))) && (!("Habitat" %in% names(domain))) && (("Shore" %in% names(domain)))) { # Check if domain data has no habitat data, only shore data.
        domain <- domain %>%
            mutate(Class = case_when(
                Shore == "Inshore" ~ "Inshore",
                Shore == "Offshore" ~ "Offshore",
                Shore == "Overhang" ~ "Overhang"
            ))
    }

    domain <- domain[!is.na(domain$Class), ]

    return(
        list("domain" = domain, "land" = land, "contour" = line, "window" = window_extent)
    )
}

create_rock_mapping_data <- function(rock_raster_fn = "./data/roberts_rock.csv", domain) {
    rock <- read.csv(rock_raster_fn) %>%
        rast() %>%
        st_as_stars()

    tmp_domain <- domain
    st_crs(tmp_domain) <- NA

    rock <- rock[tmp_domain]

    rock[[1]][rock[[1]] == 0] <- NA

    st_crs(rock) <- st_crs(domain)

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

plot_domain_map <- function(domain_data_fn, region_name, buffer_dist, contour_depth, output_fn, legend_position, plot_type = "implementation doc", contour_txt_position, contour_on = TRUE) {
    #' Creates a plot suitable for the implementation document.
    #' `buffer_dist` and `legend_position` may need tweaking to get the correct plot layout. It is recommended to tweak these parameters while
    #' setting output_fn = "" to return a plot immediately instead of saving a .png file. Then once the correct values are
    #' found add the necessary information to the batch script.
    #'
    #' @param domain_data_fn Filename for domain/habitat input data.
    #' @param region_name Region name for domain filtering (intended for roberts_regions because domains are stored in a single file)
    #' @param buffer_dist Buffer distance around the domain for plotting, should match domain data crs units (e.g. WGS84 - degrees (1 deg buffer), projected-CRS - metres).
    #' @param contour_depth Depth that separates shallow and deep layers to draw contour line
    #' @param output_fn Filename to output file. If you do not want to output a file and instead return a plot object, set output_fn = "".
    #' @param legend_position Relative position (proportion of fig width, height) of legend. E.g. (0.5, 0.5) for centre.
    #' @param plot_type String to determine if the map should be `implementation doc` style or `app` style.

    mapping_data <- compile_mapping_data(domain_data_fn, region_name, buffer_dist, contour_depth)
    domain_data <- mapping_data$domain
    land <- mapping_data$land
    line <- mapping_data$contour
    window <- mapping_data$window

    if (!contour_on) {
        line <- NULL
    }

    # legend_position <- legend_position[[1]]

    G_Y1 <- c(Inshore = "#9097CC", Offshore = "#FFD25F", Overhang = "#b01313")
    G_Y2 <- c(
        S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
        D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd",
        DO = "#b01313"
    )

    if (all(unique(domain_data$Class) %in% names(G_Y1))) {
        plot_color_palette <- G_Y1
    } else {
        plot_color_palette <- G_Y2
    }

    if (plot_type == "implementation doc") {
        land_col <- "black"
        legend_text_col <- "white"
    } else {
        land_col <- "white"
        legend_text_col <- "black"
    }

    if (region_name %in% c("Ascension_MA", "Saint_Helena_MA")) {
        box <- st_bbox(domain_data)
        centre <- c((box$xmin + box$xmax) / 2, (box$ymin + box$ymax) / 2)
        window <- list(xmin = centre[1] - 0.3, ymin = centre[2] - 0.3, xmax = centre[1] + 0.3, ymax = centre[2] + 0.3)
    }

    contour_txt_x <- window$xmin + contour_txt_position[1] * (window$xmax - window$xmin)
    contour_txt_y <- window$ymin + (contour_txt_position[2]) * (window$ymax - window$ymin)

    hab_p <- ggplot() +
        geom_sf(data = land, fill = land_col, size = 0) +
        geom_sf(data = domain_data, aes(fill = Class), size = 0.05, colour = NA) +
        geom_sf(data = land, fill = land_col, colour = "#081a24", size = 0) +
        geom_sf(data = line, colour = "red") +
        scale_fill_manual(values = plot_color_palette) +
        coord_sf(ylim = c(window$ymin, window$ymax), xlim = c(window$xmin, window$xmax), expand = F) +
        theme_minimal() +
        theme(
            text = element_text(size = 9),
            # panel.border = element_rect(colour = "black", fill = NA, size = 1),
            legend.key = element_rect(fill = NA, colour = NA),
            legend.position = legend_position,
            legend.key.size = unit(0.15, "cm"),
            legend.background = element_rect(fill = land_col, color = NA)
        ) +
        guides(fill = guide_legend(
            ncol = 2, title.hjust = 0.5,
            title.theme = element_text(size = 4, colour = legend_text_col),
            label.theme = element_text(size = 4, colour = legend_text_col)
            # override.aes = list(colour = legend_text_col)
        )) +
        labs(
            fill = "Habitats",
            #       subtitle = "The Celtic Sea Model Domain",
            x = NULL, y = NULL
        ) +
        annotate("text",
            label = paste0("Offshore outer contour \n", abs(contour_depth), " m"), x = contour_txt_x, y = contour_txt_y,
            vjust = 0, hjust = 0, size = 2, colour = "red"
        ) +
        NULL

    if (region_name %in% roberts_regions) {
        rock_data <- mapping_data[["rock"]]
        rock_p <- ggplot() +
            geom_sf(data = land, fill = land_col, size = 0) +
            geom_sf(data = domain_data, fill = "white", colour = NA) +
            geom_stars(data = rock_data) +
            geom_sf(data = domain_data, fill = NA, colour = "#284481", size = 0.05) +
            geom_sf(data = land, fill = land_col, size = 0) +
            geom_sf(data = line, colour = "red") +
            scale_fill_gradient(low = "white", high = "#8c17ae", na.value = NA, labels = scales::percent) +
            coord_sf(ylim = c(window$ymin, window$ymax), xlim = c(window$xmin, window$xmax), expand = F) +
            theme_minimal() +
            theme(
                text = element_text(size = 9),
                legend.position = legend_position,
                legend.key.size = unit(0.15, "cm"),
                legend.key = element_rect(fill = NA, colour = NA),
                # panel.border = element_rect(colour = "black", fill = NA, size = 1),
                axis.text.y = element_blank(),
                legend.background = element_rect(fill = land_col, color = NA)
            ) +
            guides(fill = guide_colourbar(
                ncol = 2, title.hjust = 0.5,
                title.theme = element_text(size = 4, colour = legend_text_col),
                label.theme = element_text(size = 4, colour = legend_text_col),
                override.aes = list(colour = legend_text_col)
            )) +
            labs(
                fill = "Rock",
                x = NULL, y = NULL
            ) +
            annotate("text",
                label = paste0("Offshore outer contour \n", abs(contour_depth), " m"), x = contour_txt_x, y = contour_txt_y,
                vjust = 0, hjust = 0, size = 2, colour = "red"
            ) +
            NULL
        hab_p <- hab_p + rock_p

        if (output_fn == "") {
            return(hab_p)
        }
        ggsave(output_fn, hab_p, width = 16, height = 11, units = "cm", dpi = 500)
        return()
    }
    if (output_fn == "") {
        return(hab_p)
    }
    ggsave(output_fn, hab_p, width = 11, height = 11, units = "cm", dpi = 500)
    return()
}
