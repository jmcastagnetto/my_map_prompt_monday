# Source: https://www.iucnredlist.org/species/22066/123792952
# Andean Bear (Tremarctos ornatus) - assessed 2016-02-02

library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)

# bear image
if (!file.exists("2023-03-06_favorite-animal/andean-bear-800x450.jpg")) {
  download.file(
    url = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c8/OursChaparriF_%2812%29.jpg/800px-OursChaparriF_%2812%29.jpg",
    destfile = "2023-03-06_favorite-animal/andean-bear-800x450.jpg"
  )
}

bear_img <- jpeg::readJPEG(
  source = "2023-03-06_favorite-animal/andean-bear-800x450.jpg",
  native = TRUE
)

bear_countries <- ne_countries(
  scale = 110,
  returnclass = "sf",
  continent = "South America",
  country = c("Argentina", "Bolivia", "Peru", "Ecuador", "Colombia", "Venezuela")
)

bear_countries <- ne_states(
  country = c("Argentina", "Bolivia", "Peru", "Ecuador", "Colombia", "Venezuela"),
  returnclass = "sf"
) %>%
  filter(code_hasc != "EC.GA")

andear_bear_range <- read_sf("2023-03-06_favorite-animal/redlist_species_data_aa936a17-2170-4e71-874f-d84936fd3763/data_0.shp")

andean_bear_map <- ggplot() +
  geom_sf(data = bear_countries, aes(fill = admin), show.legend = FALSE) +
  geom_sf(
    data = andear_bear_range %>% filter(LEGEND == "Extant (resident)"),
    fill = "#654321"
  ) +
  scale_fill_brewer(type = "qual", palette = "Pastel2") +
  theme_void() +
  labs(
    fill = ""
  )

combined_plot <- andean_bear_map +
  inset_element(
    p = bear_img,
    left = 0.6,
    bottom = 0.6,
    right = 1,
    top = 0.75
  ) +
  plot_annotation(
    title = "Range of Andean Bear\n(Vulnerable species)",
    subtitle = "Source: The IUCN Red List of\nThreatened Species",
    caption = "#MapPromptMonday (2023-03-06)\nJesus M. Castagnetto\n@jmcastagnetto@mastodon.social"
  ) &
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 16, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave(
  plot = combined_plot,
  filename = "2023-03-06_favorite-animal/andean-bear-range-distribution.png",
  height = 14,
  width = 5.5
)
