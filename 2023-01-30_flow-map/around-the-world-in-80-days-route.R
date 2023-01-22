# Info source: https://en.wikivoyage.org/wiki/Around_the_World_in_Eighty_Days

library(tidyverse)
library(tidygeocoder)
library(rnaturalearth)
library(sf)
library(ggspatial)

world <- ne_countries(scale = 110, returnclass = "sf") %>%
  filter(sov_a3 != "ATA") # remove Antartica

route <- read_csv2(
  "2023-01-30_flow-map/around-the-world-80-days-actual-travel.csv"
)


if (!file.exists("2023-01-30_flow-map/cities.csv")) {
  cities <- data.frame(city = unique(c(route$from, route$to))) %>%
    geocode(city, long = lng)
  write_csv(cities, "2023-01-30_flow-map/cities.csv")
} else {
  cities <- read_csv("2023-01-30_flow-map/cities.csv")
}

cities <- cities %>%
  rowid_to_column(var = "order") %>%
  mutate(
    label = glue::glue("({order}) {city}")
  )

route <- route %>%
  left_join(
    cities,
    by = c("from" = "city")
  ) %>%
  rename(
    from_lat = lat,
    from_lng = lng
  ) %>%
  left_join(
    cities,
    by = c("to" = "city")
  ) %>%
  rename(
    to_lat = lat,
    to_lng = lng
  )

rosm::osm.types()
set.seed(13579)

map_80days <- ggplot() +
  annotation_map_tile(
    type = "stamenwatercolor",
    zoomin = 0
  ) +
  geom_sf(data = world, fill = NA) +
  geom_spatial_segment(
    data = route,
    aes(
      x = from_lng,
      y = from_lat,
      xend = to_lng,
      yend = to_lat
    ),
    color = "darkblue",
    linewidth = 1,
    arrow = arrow(angle = 15, length = unit(4, "mm"), type = "closed"),
    crs = 4326
  ) +
  geom_spatial_point(
    data = cities,
    aes(x = lng, y = lat),
    size = 3,
    color = "red",
    crs = 4326
  ) +
  geom_spatial_label_repel(
    data = cities,
    aes(x = lng, y = lat, label = order),
    max.overlaps = 20,
    size = 3
  ) +
  geom_spatial_text_repel(
    data = cities %>%
      filter(city == "London, England") ,
    aes(x = lng + 10, y = lat, label = city),
    fontface = "bold",
    hjust = 1
  ) +
  coord_sf(
    xlim = c(-180E5, 180E5),
    ylim = c(-90E5, 90E5),
    crs = "+proj=robin"
  ) +
  annotate(
    geom = "label",
    x = -195E5,
    y = -25E5,
    label = paste(cities$label, collapse = "\n"),
    hjust = 0,
    size = 4.4
  ) +
  theme_bw(base_family = "Atkinson Hyperlegible") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  labs(
    title = "Route taken in the Jules Verne novel \"Around the world in eighty days\"",
    subtitle = "Sources: https://en.wikivoyage.org/wiki/Around_the_World_in_Eighty_Days and https://gutenberg.org/ebooks/103",
    caption = "#MapPromptMonday // 2023-01-30, Jesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  )

ggsave(
  plot = map_80days,
  filename = "2023-01-30_flow-map/around-the-world-in-80-days-route.jpg",
  width = 16,
  height = 9
)
