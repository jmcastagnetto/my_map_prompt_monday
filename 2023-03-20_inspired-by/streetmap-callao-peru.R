# Inspired by the excellent article:
# "[Streetmaps](https://ggplot2tutor.com/streetmaps/streetmaps/)",
# which uses [ggplot2](https://ggplot2.tidyverse.org/) and
# [osmdata](https://docs.ropensci.org/osmdata/) to create nice
# streetmaps of Freiburg.

library(tidyverse)
library(osmdata)
library(showtext)
library(showtextdb)

load_showtext_fonts()
font_add_google("Cinzel Decorative", "titlefont")
font_add_google("Courier Prime", "captionfont")
showtext_auto()

if (!file.exists("2023-03-20_inspired-by/callao-map-sf.Rdata")) {
  callao_bb <- getbb("Callao Perú")
  streets <- callao_bb %>%
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

  small_streets <- callao_bb %>%
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

  trains <- callao_bb %>%
    opq() %>%
    add_osm_feature(
      key = "railway",
      value = c("rail")
    ) %>%
    osmdata_sf()

  river <- callao_bb %>%
    opq() %>%
    add_osm_feature(key = "waterway",
                    value = "river") %>%
    osmdata_sf()

  save(
    callao_bb,
    streets,
    small_streets,
    river,
    trains,
    file = "2023-03-20_inspired-by/callao-map-sf.Rdata"
  )
} else {
  load("2023-03-20_inspired-by/callao-map-sf.Rdata")
}


callao_map <- ggplot() +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "white",
    alpha = .8,
    linewidth = .5
  ) +
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = "lightcoral",
    alpha = .8,
    linewidth = .3
  ) +
  geom_sf(
    data = river$osm_lines,
    inherit.aes = FALSE,
    color = "lightblue",
    alpha = .8,
    linewidth = 2
  ) +
  geom_sf(
    data = trains$osm_lines,
    inherit.aes = FALSE,
    color = "yellow",
    alpha = .8,
    linewidth = 1.2,
    linetype = "dotdash"
  ) +
  coord_sf(
    xlim = c(callao_bb["x", "min"], callao_bb["x", "max"]),
    ylim = c(callao_bb["y", "min"], callao_bb["y", "max"]),
    expand = FALSE
  ) +
  geom_rect(
    inherit.aes = FALSE,
    aes(
      xmin = callao_bb["x", "min"], #-77.090,
      xmax = callao_bb["x", "max"], #-77.000,
      ymin = callao_bb["y", "min"], #-12.08,
      ymax = callao_bb["y", "max"] #-12.03
    ),
    color = "yellow",
    fill = NA,
    linewidth = .7
  ) +
  labs(
    title = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto\n#MapPromptMonday 2023-03-20",
    caption = "{ Callao, Perú }"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "grey15", color = "peru", size = 3
    ),
    plot.title = element_text(
      family = "captionfont",
      size = 11,
      lineheight = unit(1, "cm"),
      hjust = 1,
      color = "white"
    ),
    plot.caption = element_text(
      family = "titlefont",
      size = 20,
      hjust = 0.5,
      color = "white"
    ),
    plot.margin = unit(c(.5, .5, .5, .5), "cm")
  )
callao_map

pdf(
  file = "2023-03-20_inspired-by/callao-map-a3.pdf",
  title = "Map of Callao, Peru (@jmcastagnetto@mastodon.social, 2023-03-20)",,
  width = 11.7,  # A3 size in inches
  height = 16.5
  )
print(callao_map)
dev.off()

ggsave(
  callao_map +
    theme(
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
      )
    ),
  filename = "2023-03-20_inspired-by/callao-map-a4.png",
  height = 11.7,  # A4 size in inches
  width = 8.3
)

ggsave(
  callao_map +
    theme(
      plot.title = element_text(
        family = "captionfont",
        size = 28,
        lineheight = unit(.5, "cm"),
        hjust = 1,
        color = "white"
      ),
      plot.caption = element_text(
        family = "titlefont",
        size = 44,
        hjust = 0.5,
        color = "white"
      )
    ),
  filename = "2023-03-20_inspired-by/callao-map-a5.png",
  height = 8.3,  # A5 size in inches
  width = 5.8
)

