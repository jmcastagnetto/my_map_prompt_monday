# With ideas and code "borrowed" from:
# https://milospopovic.net/crisp-topography-map-with-r/

library(tidyverse)
library(sf)
library(terra)
library(elevatr)
library(marmap)

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

lima_metro <- readRDS("common-data/mapa_distritos.rds") %>%
  filter(provincia %in% c("LIMA", "CALLAO")) %>%
  st_transform(crs = crsLONGLAT)

lima_elev <- get_elev_raster(locations = lima_metro, z = 10, clip = "tile") %>%
  as.data.frame(xy = TRUE) %>%
  rename(
    elevation = 3
  )

lima_metro_map <- ggplot() +
  geom_tile(
    data = lima_elev,
    aes(x = x, y = y, fill = elevation),
    alpha = 0.8
  ) +
  geom_sf(data = lima_metro, fill = NA) +
  scale_fill_etopo() +
  coord_sf(crs = crsLONGLAT) +
  labs(
    title = "An elevation map of Metropolitan Lima",
    subtitle = "A place I call 🏡...",
    caption = "#MapPromptMonday 2023-05-08\nJesus M. Castagnetto @jmcastagnetto@mastodon.social"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  plot = lima_metro_map,
  filename = "2023-05-08_home/metropolitan-lima-elevation-map.png",
  width = 9,
  height = 10
)
