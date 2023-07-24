library(tidyverse)
library(sf)
library(terra)
library(ggspatial)
library(rosm)

peru <- geodata::gadm("PER", level = 0, path = "common-data/") %>%
  st_as_sf()

anp <- read_sf("common-data/ANP Nacional Definitivas/ANPNacionalDefinitivas.shp")

anp_converted <- st_transform(anp, crs(peru))

tkey <- yaml::yaml.load_file(".creds")$thunderforest_apikey
url_tiles <- paste0(
  c('https://tile.thunderforest.com/landscape/${z}/${x}/${y}.png?apikey='),
  tkey
)

tf_tiles <- source_from_url_format(
  url_format = c(url_tiles),
  attribution = "Thunderforest (https://www.thunderforest.com/)",
  extension = "png"
)

register_tile_source(tf_landscape = tf_tiles)

peru_rns_plot <- ggplot() +
  geom_sf(data = peru, fill = NA, color = NA) +
  annotation_map_tile(
    type = "tf_landscape",
    zoom = 8,
    zoomin = 0
  ) +
  geom_sf(data = peru, fill = NA) +
  geom_sf(
    data = anp_converted,
    aes(fill = anp_cate),
    alpha = .8
  ) +
  geom_sf_text(
    data = anp_converted %>%
      filter(anp_nomb == "Dorsal de Nasca"),
    label = "Reserva Nacional\nDorsal de Nasca",
    nudge_y = -1,
    family = "Atkinson Hyperlegible",
    fontface = "italic"
  ) +
  scale_fill_brewer(palette = "Set1") +
  coord_sf(crs = crs(peru)) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    plot.subtitle = element_text(size = 18, color = "grey20"),
    plot.caption = element_text(
      family = "Inconsolata",
      size = 14, color = "black"
    ),
    legend.text = element_text(size = 10),
    legend.position = c(.9, .6),
    plot.margin = unit(c(t = 1, r = 1, b = 1, l = 1),"lines"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  labs(
    title = "Perú: Áreas Naturales Protegidas",
    subtitle = "Fuente: SENARPNP (MINAM)",
    caption = "#MapPromptMonday 2023-07-24\n @jmcastagnetto@mastodon.social, Jesus M. Castagnetto",
    fill = ""
  )

ggsave(
  plot = peru_rns_plot,
  filename = "2023-07-24_natural-wonders/peru-reservas-naturales.png",
  width = 12,
  height = 16
)
