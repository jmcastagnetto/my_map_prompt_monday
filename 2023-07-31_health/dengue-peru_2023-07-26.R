# Fuente: https://www.dge.gob.pe/sala-situacional-dengue/diaria/
# Al 2023-07-26

library(tidyverse)
library(sf)
library(patchwork)

if (.Platform$OS.type == "windows") {
  library(extrafont)
  loadfonts(device = "win", quiet = TRUE)
}

peru <- geodata::gadm(country = "PER", level = 2,
                      path = "common-data", resolution = 2) %>%
  st_as_sf() %>%
  mutate(
    departamento = if_else(
        NAME_1 %in% c("Lima", "Lima Province"),
        "Lima",
        NAME_1
      ) %>%
      str_to_upper() %>%
      iconv(to = "ASCII//TRANSLIT"),
    provincia = str_to_upper(NAME_2) %>%
      iconv(to = "ASCII//TRANSLIT"),
    provincia = str_replace_all(
      provincia,
      c(
        "HUENUCO" = "HUANUCO"
      )
    )
  )

dengue <- readxl::read_excel(
  "2023-07-31_health/indicadores_dengue_diario_distrito.xlsx"
) %>%
  janitor::clean_names() %>%
  filter(departamento != "Total") %>%
  mutate(
    provincia = iconv(provincia, to = "ASCII//TRANSLIT")
  ) %>%
  group_by(departamento, provincia) %>%
  summarise(
    n_casos = sum(casos, na.rm = TRUE),
    n_fallecidos = sum(fallecidos, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    cfr =  100 * n_fallecidos / n_casos
  )

peru_dengue <- peru %>%
  left_join(
    dengue,
    by = c("departamento", "provincia")
  )

mk_map <- function(data, varname) {
  df <- data %>%
    select(
      v = !!quo(varname),
      geometry
    )

  legend_title <- str_remove(varname, "n_") %>%
    str_to_title()

  ggplot() +
    geom_sf(data = df, aes(fill = v),
            color = "grey85", linewidth = .5) +
    scale_fill_viridis_c(
      name = legend_title,
      direction = -1,
      na.value = "white",
      n.breaks = 10,
      option = "rocket",
      labels = scales::label_comma()
    ) +
    theme_void(base_family = "Atkinson Hyperlegible") +
    theme(
      legend.key.height = unit(2, "cm"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 18, face = "bold")
    )
}

p1 <- mk_map(peru_dengue, "n_casos")
p2 <- mk_map(peru_dengue, "n_fallecidos")

mapa_combinado <- (p1 + p2) +
  plot_annotation(
    title = "Dengue en Provincias del Perú (al 2023-07-26)",
    subtitle = "Fuente: https://www.dge.gob.pe/sala-situacional-dengue/diaria/ (DGE/MINSA)",
    caption = "#MapPromptMonday, 2023-07-31 // Jesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title.position = "plot",
    plot.title = element_text(
      family = "Atkinson Hyperlegible",
      size = 28, face = "bold"
    ),
    plot.subtitle = element_text(
      family = "Atkinson Hyperlegible",
      size = 20, color = "grey30"
    ),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.margin = unit(rep(1, 4), "cm")
  )
mapa_combinado
ggsave(
  plot = mapa_combinado,
  file = "2023-07-31_health/dengue-peru_2023-07-26.png",
  width = 18,
  height = 12
)
