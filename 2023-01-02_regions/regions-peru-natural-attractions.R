library(tidyverse)
library(sf)
library(geodata)

peru <- gadm("PE", path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    REGIÓN = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_title()
  ) %>%
  group_by(REGIÓN) %>%
  summarise() %>%
  ungroup()


# Source: 'Inventario Nacional de Recursos Turísticos'
# https://www.datosabiertos.gob.pe/dataset/inventario-nacional-de-recursos-tur%C3%ADsticos
attractions_csv <- "common-data/Inventario_recursos_turisticos.csv"
attractions_rds <- "common-data/peru-touristic-resources.rds"

if (!file.exists(attractions_csv)) {
  download.file(
    url = "https://www.mincetur.gob.pe/Datos_abiertos/DGET/Inventario_recursos_turisticos.csv",
    destfile = attractions_csv
  )

  attractions <- read_csv2(
    attractions_csv,
    locale = locale(encoding = "WINDOWS-1252") # pick correct encoding of original data
  ) %>%
    mutate(
      FECHA_DE_CORTE = lubridate::ymd(FECHA_DE_CORTE),
      REGIÓN = iconv(REGIÓN, to = "ASCII//TRANSLIT")
    )

  saveRDS(attractions, attractions_rds)
}

attractions_region <- readRDS(attractions_rds) %>%
  group_by(REGIÓN, CATEGORÍA) %>%
  tally()

peru_attractions <- peru %>%
  left_join(
    attractions_region %>%
      filter(CATEGORÍA == "1. SITIOS NATURALES"),
    by = "REGIÓN"
  )

plot_peru_attractions <- ggplot() +
  geom_sf(
    data = peru_attractions,
    aes(fill = n)
  ) +
  coord_sf() +
  scale_fill_distiller(direction = 1, palette = "YlGnBu") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.position = c(.92,.6),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "cm")
  ) +
  labs(
    title = "Natural tourist attractions in Perú",
    subtitle = "Source: 'Inventario Nacional de Recursos Turísticos' (MINCETUR)",
    caption = "#MapPromptMonday // 2023-01-03, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social",
    fill = "Number of\nattractions"
  )

ggsave(
  plot = plot_peru_attractions,
  filename = "2023-01-02_regions/plot-peru-natural-attractions.png",
  height = 12,
  width = 8
)
