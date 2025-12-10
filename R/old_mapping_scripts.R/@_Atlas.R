#### Set up ####

rm(list = ls()) # Wipe the brain

Packages <- c("tidyverse", "sf", "terra", "rayshader") # List handy data packages
lapply(Packages, library, character.only = TRUE) # Load packages

base <- rast("./data/GEBCO_2020.nc") # Import bathymetry

GL <- readRDS("./data/Greenland Sea Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "GL")

WS <- readRDS("./cache/W Scotland Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "WS")

NS <- readRDS("./cache/North Sea Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "NS")

CS <- readRDS("./cache/Celtic Sea Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "CS")

IS <- readRDS("./cache/Irish Sea Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "IS")

Ch <- readRDS("./cache/Channel Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "Ch")

domain <- readRDS("./data/Barents Sea Habitats.rds") %>%
    st_transform(crs = 3035) %>%
    st_union() %>%
    st_sf(Region = "BS") %>%
    bind_rows(GL, WS, NS, CS, IS, Ch)

window <- st_buffer(domain, dist = 250000)

ggplot() +
    geom_sf(data = window) +
    geom_sf(data = domain, colour = "yellow")

window <- st_bbox(window)

#### Crop and reproject ####

clip <- as(extent(-75, 97, 40, 89), "SpatialPolygons") # Create cropping window for land
crs(clip) <- crs(base) # Match crs to bathymetry

base <- crop(base, clip) # Crop bathymetry
base <- aggregate(base, fact = 3)

# plot(base)

proj <- rgdal::make_EPSG() %>% # Get proj4 strings from epsg codes
    filter(code == 3035)

base2 <- projectRaster(base, crs = proj$prj4) %>% # Reproject to project projection
    as.data.frame(xy = T) %>% # Convert to a data frame
    drop_na() %>% # Drop NAs
    rast(crs = proj$prj4) # Back to Raster (with no empty border)

# plot(base2)                                                                   # Visual check

clip2 <- as(extent(
    window["xmin"], window["xmax"],
    window["ymin"], window["ymax"]
), "SpatialPolygons") # Create cropping window for land
crs(clip2) <- crs(base2) # Match crs to bathymetry

base3 <- crop(base2, clip2) # Crop bathymetry

plot(base3)

rm(base, base2)

#### depth colour palette ####

min <- round(raster::cellStats(base3, stat = "min") - 100, -2) # Smallest value rounded to 100 m deeper
max <- round(raster::cellStats(base3, stat = "max") + 100, -2) # Max value rounded to 100 m higher

water_length <- length(seq(min, 0, 100)) # How many colours do we want if we shade in steps of 100 m below sea level?
land_length <- length(seq(0, max, 100)) - 1 # How many colours for the same above ground? -1 for the shared 0

water.col <- colorRampPalette(c("aliceblue", "white")) # Colours for underwater
land.col <- colorRampPalette(c("snow2", "white")) # Colours for land

col <- c(water.col(water_length), land.col(land_length)) # Bind into a palette

#### rayshade ####

mat_all <- raster_to_matrix(base3) # Strip class

mat <- mat_all
# mat[mat < 0 ] = 0                                                             # flatten seafloor

overlay <- generate_polygon_overlay(domain,
    linecolor = "black", extent = attr(base3, "extent"),
    linewidth = 20, palette = "yellow", width = (nrow(mat) * 1), height = (ncol(mat) * 2.3)
)

overlay_l <- generate_line_overlay(readRDS("./cache/coast.rds"), attr(base3, "extent"),
    heightmap = base3,
    color = "black", linewidth = 15, width = (nrow(mat) * 1), height = (ncol(mat) * 2.3)
)

mat %>%
    sphere_shade(texture = create_texture(
        "#ffffff", "#ffffff", "#ffffff", # Create a pale colour scale
        "#ffffff", "#ffffff"
    )) %>%
    #  height_shade(texture = col) %>%                                              # Colour by height
    add_overlay(overlay_l) %>%
    add_overlay(overlay) %>%
    #  plot_map()
    plot_3d(
        heightmap = mat, zscale = 10,
        solidcolor = "#ffffff", solidlinecolor = "#ffffff",
        background = "#ffffff", theta = 0, phi = 89, asp = 2.3, zoom = 0.85,
        windowsize = 1000
    )

tictoc::tic()
render_highquality(
    lightaltitude = 55, lightdirection = 45, sample_method = "sobol",
    ground_material = rayrender::glossy(),
    filename = "./img/Atlas.png",
    parallel = TRUE, samples = 800, # width = 800, height = 800,
    min_variance = 0
)
tictoc::toc()
