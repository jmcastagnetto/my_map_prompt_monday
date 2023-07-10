library(tidyverse)
library(sf)
library(ggspatial)

# Windows has issues with loading fonts sometimes, unlike Linux
# (I try to test my R code in Win once in a blue moon)
if (.Platform$OS.type == "windows") {
  library(extrafont)
  loadfonts(device = "win", quiet = TRUE)
}

peru <- readRDS("common-data/peru-regions-sf-map.rds")
peru_crs <- st_crs(peru)

# Hospitals, etc. in Peru, from GeoPeru (more info: https://www.gob.pe/geoperu)
eess <- readxl::read_xlsx("common-data/GeoPeru-instituciones_salud.xlsx")
# Try to get everything in the same CRS
eess_points <- eess %>%
  filter(est_estado == "ACTIVO") %>%
  select(nom_dpto, clasificac, institucio, est_grupoo, x_gis, y_gis) %>%
  st_as_sf(coords = c("x_gis", "y_gis"), crs = peru_crs)

# Source: http://catalogo.geoidep.gob.pe/metadata/srv/eng/catalog.search#/metadata/986cc423-ff58-43c9-a15e-8fa668eb355d
# Mapa Nacional de Ecosistemas
ecoregions <- read_sf("common-data/N53_ECORREGIONES/TEM_BIOLO_ECORREGIONES_FINAL.shp") %>%
  st_transform(peru_crs)

desert <- ecoregions %>%
  filter(ecorregion == "Desierto Costanero del Pacifico Peruano")

eess_in_desert <- st_filter(eess_points, desert)

eess_in_desert_map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoomin = 0,
    alpha = .7
  ) +
#  geom_sf(data = peru, color = NA, fill = "white", alpha = .3) +
  geom_sf(
    data = desert,
    fill = "yellow",
    color = "yellow",
    alpha = .5
  ) +
  geom_sf(data = eess_in_desert, shape = "+", size = 1.5) +
  labs(
    title = "Healthcare Centers in the\nDesert Ecoregion of Peru",
    subtitle = "Data Sources: GeoPeru (hospitals), GeoIDEP (ecoregions)",
    caption = "#MapPromptMonday 2023-07-10\n@jmcastagnetto@mastodon.social\nJesus M. Castagnetto"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 16, color = "grey30"),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  plot = eess_in_desert_map,
  filename = "2023-07-10_deserts/healthcare-centers_desert-ecoregion_peru.png",
  width = 8,
  height = 10
)
