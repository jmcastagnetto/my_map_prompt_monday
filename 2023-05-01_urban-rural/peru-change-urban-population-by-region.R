# Data from: https://www.inei.gob.pe/estadisticas/indice-tematico/poblacion-y-vivienda/
# Urban: https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/pob_04_1.xls
# Rural: https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/pob_05_1.xls
# Downloaded: 2023-04-28, converted to CSV

library(tidyverse)
library(sf)
library(geodata)

urbana <- read_csv(
  "2023-05-01_urban-rural/urbana-pob_04_1.csv",
  col_types = cols(
    .default = col_number(),
    Departamento = col_character()
  ),
  na = c("", "-", NA)
) %>%
  select(-c(9:14)) %>%
  pivot_longer(
    cols = 2:8,
    names_to = "año",
    values_to = "urbana"
  ) %>%
  mutate(
    año = as.integer(año)
  )

rural <- read_csv(
  "2023-05-01_urban-rural/rural-pob_05_1.csv",
  col_types = cols(
    .default = col_number(),
    Departamento = col_character()
  ),
  na = c("", "-", NA)
) %>%
  pivot_longer(
    cols = 2:8,
    names_to = "año",
    values_to = "rural"
  ) %>%
  mutate(
    año = as.integer(año),
    rural = replace_na(rural, 0)
  )

pob_censada <- urbana %>%
  left_join(
    rural,
    by = c("Departamento", "año")
  ) %>%
  mutate(
    total = urbana + rural,
    purb = urbana / total,
    prur = rural / total,
    REGIÓN = case_when(
      Departamento == "Prov. Const. del Callao" ~ "Callao",
      Departamento == "Madre de Dios" ~ "Madre De Dios",
      TRUE ~ iconv(Departamento, to = "ASCII//TRANSLIT")
    )
  )

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

peru_df <- peru %>%
  left_join(
    pob_censada,
    by = "REGIÓN"
  )

peru_urban_pop_map <- ggplot(peru_df) +
  ggspatial::annotation_map_tile(
    type = "hotstyle",
    zoomin = 0
  ) +
  geom_sf(aes(fill = purb * 100, group = año)) +
  scale_fill_viridis_b(direction = -1, show.limits = TRUE, n.breaks = 9) +
  facet_wrap(~año, nrow = 2) +
  labs(
    fill = "% Urban\nPopulation",
    title = "Perú: Regional change in urban population over time",
    subtitle = str_wrap("Data Source: Instituto Nacional de Estadística e Informática - Censos Nacionales de Población y Vivienda, 1940, 1961, 1972, 1981, 1993, 2007 y 2017.", 80),
    caption = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto // #MapPromptMonday 2023-05-01"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    strip.text = element_text(size = 18, face = "italic", color = "white",
                              margin = margin(t = 0.5, b = 0.5, unit = "lines")),
    strip.background = element_rect(fill = "peru", color = NULL),
    legend.position = c(.9, .3),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1.5, "cm"),
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 16, color = "grey30",
                                 margin = margin(t = 1, b = 2, unit = "lines")),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.background = element_rect(fill = "white", color = "white")
  )
peru_urban_pop_map
ggsave(
  plot = peru_urban_pop_map,
  filename = "2023-05-01_urban-rural/peru-change-urban-population-by-region.png",
  width = 14,
  height = 12
)

ggsave(
  plot = peru_urban_pop_map,
  filename = "2023-05-01_urban-rural/peru-change-urban-population-by-region.pdf",
  width = 14,
  height = 12,
  dev = cairo_pdf
)
