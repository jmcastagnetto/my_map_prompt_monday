library(tidyverse)
library(sf)
library(geodata)

# Original data from: https://www.datosabiertos.gob.pe/dataset/poblaci%C3%B3n-identificada-con-dni-registro-nacional-de-identificaci%C3%B3n-y-estado-civil-reniec

rds_fn <- "common-data/reniec-dni-2022Q4.rds"
if (!file.exists(rds_fn)) {
  reniec <- read_csv(
    "common-data/16_OPP_2022_Dic.csv.gz",
    col_types = cols(
      .default = col_character(),
      Edad = col_integer(),
      Cantidad = col_integer()
    )
  ) %>%
    mutate(
      across(where(is.character), as.factor)
    )
  saveRDS(
    reniec,
    file = rds_fn
  )
}

# get world map from "Natural Earth"
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

peruvians_abroad <- readRDS(rds_fn) %>%
  filter(Residencia == "Extranjero") %>%
  group_by(Pais) %>%
  summarise(
    total = sum(Cantidad),
    .groups = "drop"
  ) %>%
  mutate(  # mappings to put RENIEC data into the map
    Pais = str_replace_all(
      Pais,
      c(
        "Azerbaiyan" = "Azerbaiyán",
        "Bahamas, Islas" = "Bahamas",
        "Bangladesh" = "Bangladés",
        "Bosnia Y Herzegovina" = "Bosnia y Herzegovina",
        "Brunei" = "Brunéi",
        "Egipto, República Árabe" = "Egipto",
        "Estados Unidos de América" = "Estados Unidos",
        "Gran Ducado de Luxemburgo" = "Luxemburgo",
        "Gran Bretaña" = "Reino Unido",
        "Etiopia" = "Etiopía",
        "Guayana Francesa" = "Guyana",
        "Holanda" = "Países Bajos",
        "Reino de los Paises Bajos" = "Países Bajos",
        "Irlanda Del Norte" = "Irlanda",
        "Kazajstán" = "Kazajistán",
        "Macedonia" = "República de Macedonia",
        "Rep. Democrática del Congo" = "República Democrática del Congo",
        "República de Corea del Sur" = "Corea del Sur",
        "República Islámica de Mauritania" = "Mauritania",
        "Trinidad Y Tobago" = "Trinidad y Tobago",
        "Sudan" = "Sudán",
        "Palau" = "Palaos",
        "Principado de Mónaco" = "Mónaco",
        "República Unida de Tanzania" = "Tanzania",
        "Bahréin" = "Baréin",
        "República de Fiyi" = "Fiyi",
        "Principado de Andorra" = "Andorra",
        "Suazilandia" = "eSwatini",
        "Islas Bermudas" = "Bermudas",
        "República de Armenia" = "Armenia",
        "República de San Marino" = "San Marino",
        "República de República de Macedonia" = "República de Macedonia",
        "República Togolesa" = "Togo",
        "República de Cabo Verde" = "Cabo Verde",
        "Isla de Jersey" = "Jersey",
        "Santa Lucia" = "Santa Lucía",
        "Principado de Liechtenstein" = "Liechtenstein",
        "República de las Seychelles" = "Seychelles",
        "Samoa Americana" = "Samoa Estadounidense",
        "República Democrática de Timor Oriental" = "Timor Oriental",
        "Antillas Holandesas" = "Curazao"
      )
    )
  )


world_peruvians <- world %>%
  left_join(
    peruvians_abroad,
    by = c("name_es" = "Pais")
  ) %>%
  select(
    adm0_a3,
    name,
    name_es,
    total,
    geometry
  ) %>%
  mutate(
    log10total = log10(total) # use amounts in log10 scale
  )

map_peruvian_abroad <- ggplot() +
  geom_sf(
    data = world_peruvians,
    aes(fill = log10total),
    color = "grey50"
  ) +
  geom_sf(
    data = world_peruvians %>% filter(name == "Peru"),
    fill = "peru",
    color = "white",
    linewidth = .5
  ) +
  scale_fill_distiller(
    palette = "YlGnBu", # Color blind friendly
    n.breaks = 6,
    direction = 1,
    na.value = "white",
    label = function(x) {scales::comma(10^x)}
  ) +
  coord_sf(crs = "+proj=moll") +
  theme_bw() +
  theme(
    legend.position = c(.2,.4),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.height = unit(1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  labs(
    fill = "Peruvians\nabroad",
    title = "Distribution of peruvians living abroad",
    subtitle = "Source: \"Población identificada con DNI\" (RENIEC)",
    caption = "2023-01-16, Jesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  )

ggsave(
  plot = map_peruvian_abroad,
  filename = "2023-01-16_colorblind-friendly/map-peruvians-abroad.png",
  width = 14,
  height = 9
)
