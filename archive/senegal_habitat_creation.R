library(sf)
library(lwgeom)
library(maps)
library(mapdata)
library(raster)
library(tidyverse)

#-------------------------------
#-------------------------------

# Read in the polygon of inshore and offshore model areas

domains <- readRDS("./data/Senegal_MA Domain.rds")

# st_crs(domains) ... 4702
# convert to crs 4326
domains_ll <- st_transform(domains, 4326)
# convert to planar
domains_m <- st_transform(domains_ll, 32628)

# Read in the shape file for muddy sand sediments
sand_mud <- read_sf("./data/senegal_sediment/FONDS_SABLO_VASEUX")
mud <- read_sf("./data/senegal_sediment/FONDS_VASEUX")
sand <- read_sf("./data/senegal_sediment/FONDS_SABLEUX")
rock <- read_sf("./data/senegal_sediment/FONDS_ROCHEUX")

# convert to lat long
mud_ll <- st_transform(mud, 4326)
sand_mud_ll <- st_transform(sand_mud, 4326)
sand_ll <- st_transform(sand, 4326)
rock_ll <- st_transform(rock, 4326)

# convert to crs 32628
mud_m <- st_transform(mud, 32628)
sand_mud_m <- st_transform(sand_mud, 32628)
sand_m <- st_transform(sand, 32628)
rock_m <- st_transform(rock, 32628)


# Find the  intersecion of each sedimemnt with inshore polygon - working in planar crs
# Generates warnings but not a problem

sand_m_inshore_senegal <- st_intersection(st_make_valid(sand_m), (domains_m[1, ]))
sand_mud_m_inshore_senegal <- st_intersection(st_make_valid(sand_mud_m), (domains_m[1, ]))
mud_m_inshore_senegal <- st_intersection(st_make_valid(mud_m), (domains_m[1, ]))
rock_m_inshore_senegal <- st_intersection(st_make_valid(rock_m), (domains_m[1, ]))

# Intersecion of each sediment with offshore polygon - working in planar crs

sand_m_offshore_senegal <- st_intersection(st_make_valid(sand_m), (domains_m[2, ]))
sand_mud_m_offshore_senegal <- st_intersection(st_make_valid(sand_mud_m), (domains_m[2, ]))
mud_m_offshore_senegal <- st_intersection(st_make_valid(mud_m), (domains_m[2, ]))
rock_m_offshore_senegal <- st_intersection(st_make_valid(rock_m), (domains_m[2, ]))


###################################################################

# Now extract the area of each zone NOT covered by sediment data

# Inshore
for (kkk in 1:4) {
    if (kkk == 1) sed <- sand_m_inshore_senegal
    if (kkk == 1) sft <- domains_m[1, ]
    if (kkk == 2) sed <- sand_mud_m_inshore_senegal
    if (kkk == 3) sed <- mud_m_inshore_senegal
    if (kkk == 4) sed <- rock_m_inshore_senegal
    for (jjj in 1:(length(sed))) {
        rxxx <- st_difference(sft, sed[jjj, ])
        sft <- rxxx
    }
}
Smiss <- rxxx

# Offshore
for (kkk in 1:4) {
    if (kkk == 1) sed <- sand_m_offshore_senegal
    if (kkk == 1) sft <- domains_m[2, ]
    if (kkk == 2) sed <- sand_mud_m_offshore_senegal
    if (kkk == 3) sed <- mud_m_offshore_senegal
    if (kkk == 4) sed <- rock_m_offshore_senegal
    for (jjj in 1:(length(sed))) {
        rxxx <- st_difference(sft, sed[jjj, ])
        sft <- rxxx
    }
}
Dmiss <- rxxx

# Convert everything back to lat long coordinates

rock_ll_inshore_senegal <- st_transform(rock_m_inshore_senegal, 4326)
mud_ll_inshore_senegal <- st_transform(mud_m_inshore_senegal, 4326)
sand_mud_ll_inshore_senegal <- st_transform(sand_mud_m_inshore_senegal, 4326)
sand_ll_inshore_senegal <- st_transform(sand_m_inshore_senegal, 4326)
Smiss_ll <- st_transform(Smiss, 4326)

rock_ll_offshore_senegal <- st_transform(rock_m_offshore_senegal, 4326)
mud_ll_offshore_senegal <- st_transform(mud_m_offshore_senegal, 4326)
sand_mud_ll_offshore_senegal <- st_transform(sand_mud_m_offshore_senegal, 4326)
sand_ll_offshore_senegal <- st_transform(sand_m_offshore_senegal, 4326)
Dmiss_ll <- st_transform(Dmiss, 4326)

senegal_inshore <- rbind(
    mud_ll_inshore_senegal,
    sand_mud_ll_inshore_senegal,
    sand_ll_inshore_senegal,
    rock_ll_inshore_senegal
)
senegal_inshore$Shore <- "Inshore"

senegal_offshore <- rbind(
    mud_ll_offshore_senegal,
    sand_mud_ll_offshore_senegal,
    sand_ll_offshore_senegal,
    rock_ll_offshore_senegal
)
senegal_offshore$Shore <- "Offshore"

required_cols <- c("Read_", "Shore", "Elevation", "area")

senegal_habitats <- rbind(senegal_inshore, senegal_offshore)[, required_cols]
Smiss_ll <- Smiss_ll %>%
    mutate(Shore = "Inshore", Read_ = "Missing") %>%
    mutate(geometry = st_union(st_make_valid(st_collection_extract(geometry))))
Dmiss_ll <- Dmiss_ll %>%
    mutate(Shore = "Offshore", Read_ = "Missing") %>%
    mutate(geometry = st_union(st_make_valid(st_collection_extract(geometry))))

senegal_habitats <- rbind(senegal_habitats, Smiss_ll[, required_cols], Dmiss_ll[, required_cols])
senegal_habitats <- senegal_habitats %>%
    mutate(Class = case_when(
        Read_ == "Rocky" & Shore == "Inshore" ~ "S0",
        Read_ == "Rocky" & Shore == "Offshore" ~ "D0",
        Read_ == "muddy" & Shore == "Inshore" ~ "S1",
        Read_ == "muddy" & Shore == "Offshore" ~ "D1",
        Read_ == "sandy-muddy" & Shore == "Inshore" ~ "S2",
        Read_ == "sandy-muddy" & Shore == "Offshore" ~ "D2",
        Read_ == "Sandy" & Shore == "Inshore" ~ "S3",
        Read_ == "Sandy" & Shore == "Offshore" ~ "D3",
        Read_ == "Missing" & Shore == "Inshore" ~ "S3", # Assign Missing Inshore sediment to S3 - Inshore Sand before grouping
        Read_ == "Missing" & Shore == "Offshore" ~ "D1" # Assign Missing Offshore sediment to D1 = Offshore Mud before grouping
    )) %>%
    group_by(Class) %>%
    summarise(geometry = st_union(geometry))

habcols <- c(
    S0 = "red", S1 = "#284481", S2 = "#9097CC", S3 = "cyan3", Sunk = "white",
    D0 = "deeppink", D1 = "darkgoldenrod3", D2 = "#FFD25F", D3 = "#ffedbd", Dunk = "white"
)
ggplot() +
    geom_sf(data = senegal_habitats, aes(fill = Class)) +
    geom_sf(data = domains_ll, fill = NA, linewidth = 1) +
    scale_fill_manual(values = habcols)

saveRDS(senegal_habitats, "./data/Senegal_MA Habitats.rds")
