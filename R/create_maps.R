source("./R_scripts/mapping_functions.R")

Barents_Sea <- data.frame(
    domain_data_fn = "./data/Barents Sea Habitats projected.rds",
    region_name = "Barents Sea",
    buffer_dist = 230000,
    contour_depth = -400,
    output_fn = "./outputs/Barents-sea.png",
    legend_position = I(list(c(0.7, 0.1))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.1, 0.85))),
    contour_on = TRUE
)

Celtic_Sea <- data.frame(
    domain_data_fn = c("./data/roberts_regions.csv"),
    region_name = "Celtic Sea",
    buffer_dist = 1,
    contour_depth = -200,
    output_fn = "./outputs/Celtic_Sea_implementation.png",
    legend_position = I(list(c(0.15, 0.1))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.01, 0.9))),
    contour_on = TRUE
)

# Clyde <- data.frame(
#     domain_data_fn = c("./data/roberts_regions.csv"),
#     region_name = "Clyde and Irish Sea",
#     buffer_dist = 1,
#     contour_depth = -100,
#     output_fn = "./outputs/Clyde_and_Irish_Sea_implementation.png"
# )

# English_Channel <- data.frame(
#     domain_data_fn = c("./data/roberts_regions.csv"),
#     region_name = "English Channel",
#     buffer_dist = 1,
#     contour_depth = 0,
#     output_fn = "./outputs/English_Channel_implementation.png"
# )

# Greenland_Sea <- data.frame(
#     domain_data_fn = c("./data/Greenland Sea Habitats.rds"),
#     region_name = "Greenland Sea",
#     buffer_dist = 230000,
#     contour_depth = -400,
#     output_fn = "./outputs/Greenland_Sea_implementation.png"
# )

North_Sea <- data.frame(
    domain_data_fn = c("./data/roberts_regions.csv"),
    region_name = "North Sea",
    buffer_dist = 1,
    contour_depth = -200,
    output_fn = "./outputs/North_Sea_implementation.png",
    legend_position = I(list(c(0.8, 0.125))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.02, 0.9))),
    contour_on = TRUE
)

South_Africa <- data.frame(
    domain_data_fn = c("./data/South_Africa_MA Habitats.rds"),
    region_name = "South_Africa_MA",
    buffer_dist = 1,
    contour_depth = -800,
    output_fn = "./outputs/South_Africa_MA_implementation.png",
    legend_position = I(list(c(0.8, 0.8))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.1, 0.37))),
    contour_on = TRUE
)

# West_Scotland <- data.frame(
#     domain_data_fn = c("./data/roberts_regions.csv"),
#     region_name = "West of Scotland",
#     buffer_dist = 1,
#     contour_depth = -200,
#     output_fn = "./outputs/West_of_Scotland_implementation.png"
# )

Ascension <- data.frame(
    domain_data_fn = c("./data/Ascension_MA Domain.rds"),
    region_name = "Ascension_MA",
    buffer_dist = 0,
    contour_depth = -100,
    output_fn = "./outputs/Acension_MA_implementation.png",
    legend_position = I(list(c(0.5, 0.9))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(-10, -10))),
    contour_on = FALSE
)

Azores <- data.frame(
    domain_data_fn = "./data/Azores_MA Habitats.rds",
    region_name = "Azores_MA",
    buffer_dist = 1,
    contour_depth = -200,
    output_fn = "./outputs/Azores_implementation.png",
    legend_position = I(list(c(0.15, 0.11))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(-10, -10))),
    contour_on = FALSE
)

Norwegian_Basin <- data.frame(
    domain_data_fn = "./data/Norwegian_Basin_MA Habitats.rds",
    region_name = "Norwegian_Basin_MA",
    buffer_dist = 1,
    contour_depth = -600,
    output_fn = "./outputs/Norwegian_implementation.png",
    legend_position = I(list(c(0.825, 0.1))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.025, 0.15))),
    contour_on = TRUE
)

Saint_Helena <- data.frame(
    domain_data_fn = "./data/Saint_Helena_MA Domain.rds",
    region_name = "Saint_Helena_MA",
    buffer_dist = 1,
    contour_depth = -100,
    output_fn = "./outputs/SA.png",
    legend_position = I(list(c(0.5, 0.9))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(-10, -10))),
    contour_on = FALSE
)

Senegal <- data.frame(
    domain_data_fn = "./data/Senegal_MA Habitats.rds",
    region_name = "Senegal_MA",
    buffer_dist = 1,
    contour_depth = -900,
    output_fn = "./outputs/Senegal_MA.png",
    legend_position = I(list(c(0.75, 0.6))),
    plot_type = "implementation doc",
    contour_txt_position = I(list(c(0.05, 0.9))),
    contour_on = TRUE
)

domain_mapping_df <- rbind(Barents_Sea, Celtic_Sea, North_Sea, Azores, Ascension, Norwegian_Basin, Saint_Helena, Senegal)
domain_mapping_df_app <- domain_mapping_df
domain_mapping_df_app$plot_type <- "app"

domain_mapping_df <- rbind(domain_mapping_df, domain_mapping_df_app)
domain_mapping_df <- domain_mapping_df %>%
    mutate(output_fn = paste0("./outputs/", region_name, "_", plot_type, ".png"))

pmap(domain_mapping_df, function(domain_data_fn, region_name, buffer_dist, contour_depth, output_fn, legend_position, plot_type, contour_txt_position, contour_on) {
    plot_domain_map(
        domain_data_fn,
        region_name,
        buffer_dist,
        contour_depth,
        output_fn,
        legend_position,
        plot_type,
        contour_txt_position,
        contour_on
    )
})
