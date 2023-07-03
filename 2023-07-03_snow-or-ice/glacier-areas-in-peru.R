library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

# Peru
peru <- geodata::gadm("PER", path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    REGION = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_title()
  ) %>%
  group_by(REGION) %>%
  summarise() %>%
  ungroup()

# raster TIFF from https://www.naturalearthdata.com/downloads/50m-raster-data/50m-cross-blend-hypso/
# ~175MB in size
hyp_ras <- rast("tmp/HYP_50M_SR_W/HYP_50M_SR_W.tif")

# crop world raster to Peru
peru_ras <- crop(hyp_ras, peru)
# glim files are over 156MB in size
glims_df <- read_sf("tmp/glims_download_83711/glims_polygons.shp") %>%
  filter(year(anlys_time) == 2021)

peru_glacier_areas <- ggplot() +
  geom_spatraster(data = subset(peru_ras, 1)) +
  geom_sf(data = peru, fill = NA, color = "grey90", linewidth = .25) +
  geom_sf_text(data = peru, aes(label = REGION), color = "yellow", size = 4) +
  geom_sf(data = glims_df, color = "white", fill = "white") +
  scale_fill_gradient(high = "peru") +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 32, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 22, color = "grey40", hjust = .5),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(rep(.5, 4), "cm"),
    legend.position = "none"
  ) +
  labs(
    title = "Areas with glaciers in Peru (2021, in white)",
    subtitle = "Source: NSIDC \"GLIMS Glacier Database, Version 1\"",
    caption = "#MapPromptMonday, 2023-07-03\nJesus M. Castagnetto (@jmcastagnetto@mastodon.social)"
  )

ggsave(
  plot = peru_glacier_areas,
  filename = "2023-07-03_snow-or-ice/glacier-areas-in-peru.png",
  width = 10.5,
  height = 14,
  dpi = 300
)
