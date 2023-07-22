library(tidyverse)
library(sf)
library(terra)
library(ggspatial)
library(elevatr)

anp <- read_sf("common-data/ANP Nacional Definitivas/ANPNacionalDefinitivas.shp")
lachay <- anp %>%
  filter(anp_nomb == "de Lachay")
lachay_elev <- get_elev_raster(
  locations = lachay,
  z = 14,
  clip = "locations"
)
lachay_elevation_df <- as.data.frame(lachay_elev, xy = T) %>%
  na.omit() %>%
  rename(
    elevation = 3
  )
crs_lachay <- crs(lachay)

lachay_plot <- ggplot() +
  annotation_map_tile(
    type = "osm",
    zoom = 14
  ) +
  geom_tile(
    data = lachay_elevation_df,
    aes(x = x, y = y, fill = elevation),
    alpha = 1
  ) +
  marmap::scale_fill_etopo() +
  coord_sf(
    crs = crs(lachay)
  ) +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  # theme "borrowed" from: https://milospopovic.net/crisp-topography-map-with-r/
  # and modified
  theme(
    text = element_text(color = "grey10"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 24, face = "bold",
      color = "black", hjust = .99, vjust = -5
    ),
    plot.caption = element_text(
      family = "Inconsolata",
      size = 14, color = "black",
      vjust = 20, hjust = .99
    ),
    plot.margin = unit(c(t = 1, r = 0, b = 1, l = 0),"lines"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  labs(
    title = "\"Lomas de Lachay\" National Reserve (Lima, Perú)",
    caption = "Source: SENARPNP (MINAM)\n#MapPromptMonday 2023-07-24 // @jmcastagnetto@mastodon.social, Jesus M. Castagnetto"
  )

ggsave(
  plot = lachay_plot,
  filename = "2023-07-24_natural-wonders/peru-lomas-de-lachay.png",
  width = 12,
  height = 12
)
