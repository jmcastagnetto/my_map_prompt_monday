library(tidyverse)
library(tmap)
library(stars)
library(geodata)
library(rayshader)
library(rgl)

if (!file.exists("common-data/PER_elv_msk.tif")) {
  elevation_30s(country = "Peru", path = "common-data/")
}

peru_elevation <- read_stars("common-data/PER_elv_msk.tif")
peru <- gadm("PE", path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    REGIÓN = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_title()
  ) %>%
  group_by(REGIÓN) %>%
  summarise() %>%
  ungroup()

# base for map
p1 <- ggplot() +
  geom_stars(data = peru_elevation) +
  geom_sf(data = peru, fill = NA, color = "white") +
  scale_fill_viridis_c(
    na.value = "white",
    option = "inferno",
    direction = -1
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.key.height = unit(2, "cm"),
    legend.title = element_text(size = 18, hjust = 0.5),
    legend.text = element_text(size = 16)
  ) +
  labs(
    fill = "Elevation\n(meters)"
  )

# 2D map
p1a <- p1 +
  annotate(
    geom = "text",
    x = -79,
    y = -12,
    angle = -70,
    label = "Perú",
    family = "Cinzel Decorative",
    size = 28
  )

ggsave(
  plot = p1a,
  filename = "2023-03-13_earth-imagery/peru-elevation-map-2d.png",
  width = 13,
  height = 16
)

# for 3D plot
p1b <- p1 +
  annotate(
    geom = "text",
    x = -79,
    y = -12,
    angle = -70,
    label = "Perú",
    family = "Cinzel Decorative",
    size = 8
  ) +
  theme(
    legend.position = "none"
  )
#p1b

plot_gg(
  p1b,
  multicore = TRUE,
  scale = 75,
  shadow = FALSE,
  baseshape = "circle",
  basedepth = 0,
  basecolor = "white",
  solid = FALSE,
  solidcolor = "white",
  solidlinecolor = "white"
)
render_resize_window(width = 1200, height = 1000)
render_camera(
  zoom = .5,
  phi = 45,
  theta = -15
)
render_snapshot(
  filename = "2023-03-13_earth-imagery/peru-elevation-map-3d.png"
)
render_highquality(
  filename = "2023-03-13_earth-imagery/peru-elevation-map-3d-hq.png",
  ambient_light = TRUE,
  backgroundhigh = "#ffffff",
  backgroundlow = "#ffffff",
  tonemap = "uncharted",
  verbose = TRUE,
  interactive = FALSE,
  samples = 200,
  iso = 400,
  parallel = TRUE
)
