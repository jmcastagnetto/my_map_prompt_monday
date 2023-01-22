library(tidyverse)
library(sf)
library(geodata)

loreto <- gadm("PE",level = 2, path = "common-data/") %>%
  st_as_sf() %>%
  filter(NAME_1 == "Loreto")


eess_url <- "https://cloud.minsa.gob.pe/s/4tfyJLoCwWYdRBR/download"
eess_csv <- "common-data/TB_EESS.csv"
eess_rds <- "common-data/TB_EESS.rds"

if (!file.exists(eess_csv)) {
  download.file(
    url = eess_url,
    destfile = eess_csv
  )
  eess <- read_csv(
    eess_csv,
    col_types = cols(
      .default = col_character(),
      longitud = col_number(),
      latitud = col_number()
    )
  )
  saveRDS(eess, eess_rds)
}

eess_loreto <- readRDS(eess_rds) %>%
  filter(diresa == "LORETO" &
           !is.na(latitud) &
           !is.na(longitud)
  )

plot_loreto <- ggplot() +
  geom_sf(data = loreto, fill = "white") +
  geom_density_2d(
    data = eess_loreto,
    aes(x = longitud, y = latitud),
    color = "grey40",
    bins = 28,
    alpha = 0.3
  ) +
  stat_density2d(
    data = eess_loreto,
    aes(x = longitud, y = latitud,
        fill = after_stat(level),
        alpha = after_stat(level)),
    size = 0.01,
    bins = 28,
    geom = "polygon") +
  scale_fill_gradient(low = "cyan", high = "peru", guide = FALSE) +
  scale_alpha(range = c(0.3, 0.6), guide = FALSE) +
  geom_text(
      data = data.frame(
      longitud = c(-73.25383, -76.12234),
      latitud = c(-3.74912, -5.90181),
      label = c("Iquitos", "Yurimaguas")
    ),
    aes(x = longitud, y = latitud, label = label),
    color = "black",
    size = 6
  ) +
  geom_point(
    data = eess_loreto,
    aes(x = longitud, y = latitud),
    color = "grey40",
    size = .5
  ) +
  annotate(
    geom = "text",
    x = -73.5,
    y = -.5,
    label = "Health establishments\nare clustered near the\nbiggest cities in Loreto",
    size = 6,
    hjust = 0
  ) +
  labs(
    fill = "Density\nof Locations",
    title = "Health establishments in Loreto, Peru",
    subtitle = "Source: 'Establecimientos de Salud' (MINSA)",
    caption = "#MapPromptMonday // 2023-01-09, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social"
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 23, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave(
  plot = plot_loreto,
  filename = "2023-01-09_heatmap/health-establishments-loreto-peru.png",
  width = 7.5,
  height = 9
)
