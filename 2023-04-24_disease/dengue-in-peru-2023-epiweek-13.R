library(tidyverse)
library(sf)
library(geodata)

dengue <- read_csv("2023-04-24_disease/Texto 9_data.csv") %>%
  janitor::clean_names() %>%
  rownames_to_column("rname") %>%
  filter(!is.na(longitude_generated)) %>%
  group_by(rname) %>%
  st_as_sf(
    coords = c("longitude_generated", "latitude_generated"),
    crs = crs("wgs84")
  )

peru <- gadm(country = "Peru", level = 1, path = "common-data/") %>%
  st_as_sf()

peru_dengue <- ggplot() +
  ggspatial::annotation_map_tile(
    type = "stamenwatercolor",
    zoomin = 0
  ) +
  geom_sf(
    data = peru,
    fill = "transparent"
  ) +
  geom_sf(
    data = dengue,
    aes(size = casos),
    color = "red",
    alpha = .5
  ) +
  scale_size_continuous(
    breaks = c(50, 100, 200, 300, 400)
  ) +
  labs(
    size = "Dengue\nCases",
    title = "Distribution of Dengue in Peru (2023)",
    subtitle = "Source: 'Situación actual de Dengue' (epiweek #13)",
    caption = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto\n#MapPromptMonday 2023-04-24"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "gray50", size = 14, hjust = 0.5),
    plot.caption = element_text(
      family = "Inconsolata", size = 12, hjust = 0.5
    ),
    plot.margin = unit(rep(.1, 4), "cm"),
    legend.position = c(.15, .3)
  )

ggsave(
  plot = peru_dengue,
  filename = "2023-04-24_disease/dengue-distribution-peru-2023-epiweek13.png",
  width = 5.5,
  height = 8
)
