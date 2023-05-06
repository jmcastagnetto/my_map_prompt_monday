# Data source: Armed Conflict Location & Event Data Project (ACLED); www.acleddata.com

library(tidyverse)
library(sf)
library(geodata)
library(patchwork)
library(ggtext)

conflicts <- read_csv("2023-05-15_proportional-symbols/2022-01-01-2023-05-01-Peru.csv") %>%
  mutate(
    event_date = lubridate::dmy(event_date)
  ) %>%
  filter(
    event_type %in% c("Protests", "Riots")
  )

by_region <- conflicts %>%
  group_by(admin1, event_type) %>%
  tally() %>%
  group_by(event_type) %>%
  mutate(
    avg = mean(n, na.rm = TRUE),
    danger = (n > mean(n, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    admin1 = str_to_upper(admin1)
  )

from_date <- format(min(conflicts$event_date), "%b %Y")
to_date <- format(max(conflicts$event_date), "%b %Y")

peru <- gadm("PER", level = 1, path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    admin1 = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_upper()
  ) %>%
  group_by(admin1) %>%
  summarise() %>%
  ungroup()

peru_conflicts <- peru %>%
  left_join(
    by_region,
    by = "admin1"
  )

peru_conflicts_centroids <- st_centroid(peru) %>%
  left_join(
    by_region,
    by = "admin1"
  )


mk_map <- function(peru, peru_conflicts, peru_conflicts_centroids, type) {
  ggplot() +
    geom_sf(data = peru) +
    geom_sf(data = peru_conflicts %>% filter(event_type == type),
            aes(fill = danger), show.legend = FALSE) +
    geom_sf(data = peru_conflicts_centroids %>% filter(event_type == type),
            aes(size = n))  +
    scale_fill_manual(
      values = c("white", "red")
    ) +
    scale_size_binned_area(name = type, n.breaks = 10) +
    theme_void(base_family = "Atkinson Hyperlegible")
}


combined_map <- mk_map(peru, peru_conflicts, peru_conflicts_centroids, "Protests") +
  mk_map(peru, peru_conflicts, peru_conflicts_centroids, "Riots") +
  plot_annotation(
    title = glue::glue("Protests and Riots in Peru from {from_date} to {to_date}"),
    subtitle = "Regions with a number of events **<span style='color:red;'>above the average</span>** are highlighted",
    caption = "Data source: Armed Conflict Location & Event Data Project (ACLED); www.acleddata.com\n#MapPromptMonday 2023-05-15, Jesus M. Castagnetto @jmcastagnetto@mastodon.social"
  ) &
  theme(
    legend.title = element_text(size = 16, face = "bold.italic"),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_textbox(size = 26),
    plot.caption = element_text(family = "Inconsolata", size = 18),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave(
  plot = combined_map,
  filename = "2023-05-15_proportional-symbols/conflicts-in-peru-jan2022-apr2023.png",
  width = 16,
  height = 10
)

