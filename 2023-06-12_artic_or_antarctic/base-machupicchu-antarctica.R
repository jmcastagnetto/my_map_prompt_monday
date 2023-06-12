library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(osmdata)
library(showtext)

font_add_google("Inconsolata", "Inconsolata")
showtext_auto()

world <- ne_countries(scale = 10, returnclass = "sf")

# southamerica <- world %>%
#   filter(continent == "South America")

peru <- world %>%
  filter(sov_a3 == "PER")

ata <- world %>%
  filter(sov_a3 == "ATA") # select Anctartica

machupicchu <- tribble(
  ~name, ~lon, ~lat,
  "Machu Picchu",-72.545556,-13.163333
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

machupicchu_base_osm <- getbb("Machu Picchu Base") %>%
  opq() %>%
  add_osm_feature(
    key = "place",
    value = c("isolated_dwelling")
  ) %>%
  osmdata_sf()

locations <- bind_rows(
  machupicchu,
  machupicchu_base_osm$osm_points %>%
    select(name, geometry) %>%
    mutate(name = "Machu Picchu Research Base")
)

machupicchu_and_research_base_ata <- ggplot() +
  annotation_map_tile(
    type = "stamenwatercolor",
    zoomin = 0
  ) +
  geom_sf(data = peru, fill = "peru", alpha = 0.4) +
  geom_sf(data = ata, fill = "cyan", alpha = 0.4) +
  geom_sf(data = machupicchu, size = 5, color = "blue", shape = 10) +
  geom_sf(data = machupicchu_base_osm$osm_points,
          size = 5, color = "brown", shape = 10) +
  geom_sf_label(data = locations,
                aes(label = name),
                alpha = .8,
                size = 7,
                nudge_y = 400000) +
  coord_sf(
    crs = "+proj=ups +south"
  ) +
  labs(
    title = "Machu Picchu and its namesake research base in Antarctica (South Polar Projection)",
    subtitle = "Sources: OpenStreeMaps, Natural Earth",
    caption = "#MapPromptMonday, 2023-06-12 // Jesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.title = element_text(size = 52, face = "bold"),
    plot.subtitle = element_text(size = 32, color = "grey40"),
    plot.caption = element_text(family = "Inconsolata", size = 24),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.background = element_rect(color = "white", fill = "white")
  )

ggsave(
  plot = machupicchu_and_research_base_ata,
  filename = "2023-06-12_artic_or_antarctic/peru-machupicchu-antarctic-research-base.png",
  width = 14,
  height = 7
)
