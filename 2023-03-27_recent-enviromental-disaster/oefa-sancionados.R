# Source: https://publico.oefa.gob.pe/administrados-sancionados/#/
library(tidyverse)
library(readxl)
library(sf)
library(geodata)
library(glue)

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

get_data <- function(xlsx) {
  read_excel(
    xlsx,
    na = c("", "---"),
    skip = 1
  ) %>%
    janitor::clean_names() %>%
    select(
      administrado,
      departamento,
      inicio_supervision,
      cantidad_de_infracciones
    ) %>%
    separate_rows(
      departamento,
      sep = ","
    ) %>%
    mutate(
      año = year(dmy(inicio_supervision)),
      departamento = str_trim(departamento)
    ) %>%
    group_by(
      administrado,
      departamento
    ) %>%
    summarise(
      min_año = min(año, na.rm = TRUE),
      max_año = max(año, na.rm = TRUE),
      infracciones = sum(cantidad_de_infracciones, na.rm = TRUE),
      .groups = "drop"
    )
}


petroperu <- get_data("2023-03-27_recent-enviromental-disaster/oefa-sancionados/Reporte_1678718725460.xlsx")
pluspetrol <- get_data("2023-03-27_recent-enviromental-disaster/oefa-sancionados/Reporte_1678737295284.xlsx")
saviaperu <- get_data("2023-03-27_recent-enviromental-disaster/oefa-sancionados/Reporte_1678737331856.xlsx")

mk_map <- function(peru, df) {
  min_año <- min(df$min_año)
  max_año <- max(df$max_año)
  entity <- unique(df$administrado)
  peru_df <- peru %>%
    left_join(
      df,
      by = c("REGIÓN" = "departamento")
    )
  ggplot(peru_df) +
    geom_sf(
      aes(fill = infracciones),
      color = "white",
      show.legend = FALSE
    ) +
    geom_sf_label(
      aes(label = infracciones),
      label.size = 0,
      label.padding = unit(0.1, "lines")
    ) +
    scale_fill_gradient(
      low = "#D52800",
      high = "#190400",
      na.value = "#CCCCCC"
    ) +
    labs(
      title = glue("Enviromental Sanctions to\n{entity}\n[{min_año} - {max_año}]"),
      subtitle = "Source: OEFA - \"Administrados Sancionados\"",
      caption = "#MapPromptMonday // 2023-03-27, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social",
      fill = "Sanctions"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16, color = "gray40"),
      plot.caption = element_text(family = "Inconsolata", size = 14),
      plot.background = element_rect(fill = "white", colour = "white")
    )
}

ggsave(
  plot = mk_map(peru, petroperu),
  filename = "2023-03-27_recent-enviromental-disaster/oefa-sanciones-petroperu.png",
  width = 8,
  height = 12
)

ggsave(
  plot = mk_map(peru, pluspetrol),
  filename = "2023-03-27_recent-enviromental-disaster/oefa-sanciones-pluspetrol.png",
  width = 8,
  height = 12
)

ggsave(
  plot = mk_map(peru, saviaperu),
  filename = "2023-03-27_recent-enviromental-disaster/oefa-sanciones-saviaperu.png",
  width = 8,
  height = 12
)
 +
  mk_map(peru, pluspetrol) +
  mk_map(peru, saviaperu)


c("#D52800", "#B42200", "#931B00", "#761600", "#591000", "#3B0B00", "#190400", "#CCCCCC")


mk_map(peru, petroperu)

