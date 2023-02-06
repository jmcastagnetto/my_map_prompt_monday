library(tidyverse)
library(sf)
library(biscale)
library(geodata)
library(patchwork)

peru_prov <- gadm(country = "PE", level = 2, path = "common-data/") %>%
  st_as_sf() %>%
  mutate(
    departamento = if_else(
        NAME_1 %in% c("Lima", "Lima Province"),
        "Lima",
        NAME_1
      ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_upper(),
    provincia = str_replace_all(
        NAME_2,
        c(
          "Huenuco" = "Huanuco"
        )
      ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_upper()
  )

prov_df <- readRDS(url("https://github.com/jmcastagnetto/ubigeo-peru-aumentado/blob/main/ubigeo_provincia.rds?raw=true")) %>%
  select(
    departamento,
    provincia,
    indice_densidad_estado,
    indice_vulnerabilidad_alimentaria,
    idh_2019,
    pct_pobreza_total
  ) %>%
  mutate(
    departamento = iconv(departamento, to = "ASCII//TRANSLIT"),
    provincia = iconv(provincia, to = "ASCII//TRANSLIT"),
    idh_2019 = as.numeric(idh_2019),
    indice_vulnerabilidad_alimentaria = as.numeric(indice_vulnerabilidad_alimentaria),
    indice_densidad_estado = as.numeric(indice_densidad_estado)
  )

bivariate_df <- prov_df %>%
  bi_class(
    x = idh_2019,
    y = indice_densidad_estado,
    style = "quantile",
    dim = 4
  )

peru_prov_df <- peru_prov %>%
  left_join(
    bivariate_df,
    by = c("departamento", "provincia")
  )

map_legend <- bi_legend(
  pal = "BlueGold",
  dim = 4,
  xlab = "Higher HDI ",
  ylab = "Higher Gov. Density ",
  size = 10)

prov_map <- ggplot(
  peru_prov_df
) +
  geom_sf(
    aes(fill = bi_class),
    color = "white", linewidth = 0.2, show.legend = FALSE
  ) +
  bi_scale_fill(pal = "BlueGold", dim = 4) +
  bi_theme()

final_plot <- prov_map +
  inset_element(
    map_legend,
    left = 0,
    bottom = 0,
    right = 0.35,
    top = 0.35
  ) +
  plot_annotation(
    title = "Government density and HDI\nat the province level in Peru",
    subtitle = "Data Source: CEPLAN & PNUD",
    caption = "#MapPromptMonday\n2023-02-13, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social"
  ) &
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 12)
  )

ggsave(
  plot = final_plot,
  filename = "2023-02-13_bivariate-maps/bivariate-map-preru-govdensity-hdi.png",
  height = 12,
  width = 7.5
)
