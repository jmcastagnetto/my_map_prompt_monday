# Source: https://www.datosabiertos.gob.pe/dataset/accidentes-de-tr%C3%A1nsito-en-carreteras

library(tidyverse)
library(sf)
library(geodata)

accidentes <- read_delim(
  "https://www.datosabiertos.gob.pe/sites/default/files/Accidentes%20de%20tr%C3%A1nsito%20en%20carreteras-2020-2021-Sutran.csv",
  delim = ";",
  locale = vroom::locale(encoding = "ISO-8859-1"),
  col_types = cols(
    .default = col_character(),
    FALLECIDOS = col_integer(),
    HERIDOS = col_integer(),
    FECHA = col_date(format = "%Y%m%d")
  ),
  na = c("", "N.I.")
)

pob2020 <- read_csv("common-data/pob2020-departamento_reunis.csv")

yr2020 <- accidentes %>%
  mutate(
    yr = year(FECHA),
    DEPARTAMENTO = str_to_upper(DEPARTAMENTO)
  ) %>%
  filter(yr == 2020) %>%
  group_by(DEPARTAMENTO, MODALIDAD) %>%
  summarise(
    deaths = sum(FALLECIDOS, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(MODALIDAD == "CHOQUE") %>%
  left_join(
    pob2020 %>% select(DEPARTAMENTO, Total),
    by = c("DEPARTAMENTO")
  ) %>%
  mutate(
    deaths_per_1M = deaths * 1e6 / Total
  )

peru <- gadm("PE", level = 1, path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    REGION = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "LIMA",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_upper()
  ) %>%
  group_by(REGION) %>%
  summarise() %>%
  ungroup()

plot_df <- peru %>%
  left_join(
    yr2020,
    by = c("REGION" = "DEPARTAMENTO")
  ) %>%
  filter(!is.na(deaths))

plot_map <- ggplot() +
  geom_sf(data = plot_df, aes(fill = deaths_per_1M)) +
  geom_sf_text(
    data = plot_df,
    aes(label = sprintf("%.1f", deaths_per_1M)),
    size = 4.5
  ) +
  scale_fill_distiller(
    palette = "Reds",
    direction = 1
  ) +
  labs(
    title = "Deaths per million by vehicle crashes in highways in Peru (2020)",
    subtitle = "Source: 'Accidentes de Tránsito en Carreteras' (SUTRAN)",
    caption = "#MapPromptMonday 2023-06-05\n@jmcastagnetto@mastodon.social\nJesus M. Castagnetto"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0.5),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave(
  plot = plot_map,
  filename = "2023-06-05_safety/death-by-crashes-highways-2020-peru.png",
  width = 10,
  height = 15
)
