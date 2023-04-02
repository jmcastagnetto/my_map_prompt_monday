library(tidyverse)
library(osmdata)
library(showtext)
library(showtextdb)

load_showtext_fonts()
font_add_google("Cinzel Decorative", "titlefont")
font_add_google("Courier Prime", "captionfont")
showtext_auto()

if (!file.exists("2023-04-03_bucket-list/kumamoto-map-sf.Rdata")) {
  kumamoto_bb <- getbb("Kumamoto-shi")
  streets <- kumamoto_bb %>%
    opq() %>%
    add_osm_feature(
      key = "highway",
      value = c(
        "turning circle",
        "turning loop",
        "mini roundabout",
        "motorway",
        "primary",
        "secondary",
        "tertiary"
      )
    ) %>%
    osmdata_sf()

  small_streets <- kumamoto_bb %>%
    opq() %>%
    add_osm_feature(
      key = "highway",
      value = c(
        "residential",
        "living_street",
        "unclassified",
        "service",
        "footway"
      )
    ) %>%
    osmdata_sf()

  trains <- kumamoto_bb %>%
    opq() %>%
    add_osm_feature(
      key = "railway",
      value = c("rail")
    ) %>%
    osmdata_sf()

  river <- kumamoto_bb %>%
    opq() %>%
    add_osm_feature(key = "waterway",
                    value = "river") %>%
    osmdata_sf()

  save(
    kumamoto_bb,
    streets,
    small_streets,
    river,
    trains,
    file = "2023-04-03_bucket-list/kumamoto-map-sf.Rdata"
  )
} else {
  load("2023-04-03_bucket-list/kumamoto-map-sf.Rdata")
}


kumamoto_map <- ggplot() +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "white",
    alpha = .8,
    linewidth = .3
  ) +
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = "lightcoral",
    alpha = .8,
    linewidth = .1
  ) +
  geom_sf(
    data = river$osm_lines,
    inherit.aes = FALSE,
    color = "lightblue",
    alpha = .8,
    linewidth = .5
  ) +
  geom_sf(
    data = trains$osm_lines,
    inherit.aes = FALSE,
    color = "yellow",
    alpha = .8,
    linewidth = .5,
    linetype = "dotdash"
  ) +
  coord_sf(
    xlim = c(kumamoto_bb["x", "min"], kumamoto_bb["x", "max"]),
    ylim = c(kumamoto_bb["y", "min"], kumamoto_bb["y", "max"]),
    expand = FALSE
  ) +
  geom_rect(
    inherit.aes = FALSE,
    aes(
      xmin = kumamoto_bb["x", "min"],
      xmax = kumamoto_bb["x", "max"],
      ymin = kumamoto_bb["y", "min"],
      ymax = kumamoto_bb["y", "max"]
    ),
    color = "yellow",
    fill = NA,
    linewidth = .7
  ) +
  labs(
    title = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto\n#MapPromptMonday 2023-04-03",
    caption = "{ Kumamoto: A city of many rivers }"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "grey15", color = "peru", size = 3
    ),
    plot.title = element_text(
      family = "captionfont",
      size = 36,
      lineheight = unit(.5, "cm"),
      hjust = 1,
      color = "white"
    ),
    plot.caption = element_text(
      family = "titlefont",
      size = 64,
      hjust = 0.5,
      color = "white"
    ),
    plot.margin = unit(c(.5, .5, .5, .5), "cm")
  )
kumamoto_map

ggsave(
  kumamoto_map,
  filename = "2023-04-03_bucket-list/kumamoto-map-a4.png",
  height = 11.7,  # A4 size in inches
  width = 8.3
)
