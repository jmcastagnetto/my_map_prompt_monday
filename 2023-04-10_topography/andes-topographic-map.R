library(tidyverse)
library(sf)
library(rnaturalearth)
library(stars)

# get map at the state/region level for the Andean countries
andes_2 <- ne_states(
  country = c("Peru", "Ecuador", "Colombia",
              "Bolivia", "Venezuela",
              "Chile", "Argentina"),
  returnclass = "sf"
)

# get merged polygon of Andean countries
andes <- ne_countries(
  country = c("Peru", "Ecuador", "Colombia",
              "Bolivia", "Venezuela",
              "Chile", "Argentina"),
  returnclass = "sf"
) %>%
  group_by(featurecla) %>%
  summarise()

# Using data from https://www.naturalearthdata.com/downloads/50m-raster-data/50m-manual-shaded-relief/
# extract the parts corresponding to the Andean countries
if (!file.exists("2023-04-10_topography/cropped_andes_elevation_stars.rds")) {
  world_elevation <- read_stars(
    unzip(
      zipfile = "common-data/MSR_50M.zip",
      files = "MSR_50M/MSR_50M.tif"
    )
  )
  # crop elevation data (time consuming step)
  cropped_andes_elevation <- st_crop(world_elevation, andes)
  saveRDS(
    cropped_andes_elevation,
    file = "2023-04-10_topography/cropped_andes_elevation_stars.rds"
  )
} else {
  cropped_andes_elevation <- readRDS("2023-04-10_topography/cropped_andes_elevation_stars.rds")
}

andes_map <- ggplot() +
  geom_stars(data = cropped_andes_elevation, show.legend = FALSE) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = -1,
    na.value = "transparent"
  ) +
  geom_sf(
    data = andes_2,
    color = "gray80",
    fill = "transparent"
  ) +
  theme_void(
    base_family = "Atkins Hyperlegible"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray50", size = 14, hjust = 0.5),
    plot.caption = element_text(
      family = "Inconsolata", size = 8, hjust = 0.5
    ),
    plot.margin = unit(rep(.1, 4), "cm")
  ) +
  labs(
    title = "The Andes",
    subtitle = "Data Source: Natural Earth",
    caption = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto\n#MapPromptMonday 2023-04-10"
  ) +
  coord_sf(
    xlim = c(-80, -55),
    ylim = c(-55, 12)
  )
#andes_map
ggsave(
  plot = andes_map,
  filename = "2023-04-10_topography/andes-topographic-map.png",
  height = 8,
  width = 4
)

