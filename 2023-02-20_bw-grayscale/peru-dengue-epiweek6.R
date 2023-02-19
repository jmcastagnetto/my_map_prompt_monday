# Source: https://www.dge.gob.pe/sala-situacional-dengue/#grafico01
library(tidyverse)
library(sf)
library(geodata)
library(readxl)

dengue_w6 <- read_excel(
  "2023-02-20_bw-grayscale/Nacional.xlsx",
  skip = 2
) %>%
  select(1,14:15) %>%
  pivot_longer(
    cols = 2:3,
    names_to = "year",
    values_to = "cases"
  ) %>%
  mutate(
    year = str_remove_all(year, fixed("*")) %>%
      as.numeric()
  ) %>%
  filter(Departamentos != "TOTAL") %>%
  bind_rows(  # Departamentos sin datos
    data.frame(
      Departamentos = rep(c("APURIMAC", "HUANCAVELICA", "TACNA"), each = 2),
      year = rep(2022:2023, 3),
      cases = NA_real_
    )
  )

peru <- gadm("PE", path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    Departamentos = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_upper()
  ) %>%
  group_by(Departamentos) %>%
  summarise() %>%
  ungroup()

peru_df <- peru %>%
  left_join(
    dengue_w6,
    by = "Departamentos",
    multiple = "all"
  )

dengue_map <- ggplot(peru_df) +
  geom_sf(aes(fill = cases)) +
  scale_fill_distiller(palette = "Greys", direction = 1, na.value = "white") +
  facet_wrap(~year) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(size = 20, face = "bold"),
    strip.background = element_blank(),
    legend.title = element_text(size = 16, face = "italic"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(2, "cm")
  ) +
  labs(
    title = "Increment of Dengue cases in Perú: 2022 ⇢ 2023\n(up to epiweek #6 of each year)",
    subtitle = "Source: 'Sala Situacional de Dengue' (MINSA)",
    caption = "#MapPromptMonday // 2023-02-20, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social",
    fill = "Dengue\nCases"
  )

ggsave(
  plot = dengue_map,
  filename = "2023-02-20_bw-grayscale/peru-dengue-epiweek6-2022-2023.png",
  height = 12,
  width = 16
)

