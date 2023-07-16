library(tidyverse)
library(sf)
library(rcartocolor)
library(ggspatial)
library(patchwork)

# pull ISO3 codes for South American countries
southam <- countrycode::codelist %>%
  filter(region23 == "South America" & !is.na(cow.name)) %>%
  select(
    iso3c,
    iso.name.en,
    country.name.en
  )

# raw data on cocoa bean production from FAO
fao_cocoa_raw <- read_csv("2023-07-17_desserts/FAOSTAT_data_en_7-14-2023.csv") %>%
  inner_join(
    southam,
    by = c("Area" = "iso.name.en")
  )

# pick the 1961 year as comparison base for each country
fao_cocoa_base <- fao_cocoa_raw %>%
  filter(Year == 1961 & Element == "Area harvested") %>%
  select(Area, base_value = Value)

fao_cocoa_df <- fao_cocoa_raw %>%
  filter(Element == "Area harvested") %>%
  left_join(
    fao_cocoa_base,
    by = c("Area")
  ) %>%
  mutate(
    change = Value / base_value
  )

# get all the countries that produce cocoa beans
cocoa_map <- rnaturalearth::ne_countries(continent = "South America",
                                         returnclass = "sf") %>%
  inner_join(
    fao_cocoa_df,
    by = c("iso_a3" = "iso3c")
  )

p1 <- ggplot(data = cocoa_map) +
  annotation_map_tile(
    type = "stamenwatercolor"
  ) +
  geom_sf(
    aes(fill = change),
    #alpha = .7,
    show.legend = FALSE
  ) +
  geom_sf_label(
    aes(label = sprintf("%.1fx", change)),
    size = 3,
    label.size = 0,
    fill = rgb(1, 1, 1, .7)
  ) +
  facet_wrap(~Year, nrow = 1) +
  scale_fill_carto_c(palette = "BrwnYl", trans = scales::log10_trans()) +
  theme_void(
    base_family = "Atkinson Hyperlegible",
    base_size = 24
  )

# Okabe-Ito colors
color_map <- c(
  "Peru" = "#000000",
  "Bolivia" = "#E69F00",
  "Brazil" = "#56B4E9",
  "Colombia" = "#D55E00",
  "Ecuador" = "#0072B2",
  "Guyana" = "#009E73",
  "Suriname" = "#CC79A7",
  "Venezuela" = "#999999"
)

p2 <- ggplot() +
  geom_line(
    data = fao_cocoa_df,
    aes(
      x = Year, y = Value * 0.01,
      group = country.name.en,
      color = country.name.en
    ),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_line(
    data = fao_cocoa_df %>% filter(country.name.en == "Peru"),
    aes(
      x = Year, y = Value * 0.01
    ),
    linewidth = 1.5,
    show.legend = FALSE
  ) +
  geom_point(
    data = fao_cocoa_df %>% filter(Year == 1961),
    aes(x = Year, y = Value * 0.01, color = country.name.en),
    size = 3,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = fao_cocoa_df %>% filter(Year == 1961),
    aes(x = Year, y = Value * 0.01,
        color = country.name.en, label = country.name.en),
    hjust = 0,
    nudge_x = -2,
    size = 5,
    seed = 1357,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = color_map
  ) +
  scale_y_log10(
    labels = scales:::label_log()
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0, add = c(10, 10)),
    n.breaks = 9
  ) +
  labs(
    y = "Harvested Area (Km²)",
    x = ""
  ) +
  ggthemes::theme_tufte(
    base_family = "Atkinson Hyperlegible",
    base_size = 16
  )
plot_combined <- (p1 / p2) +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title = "Growth in harvested areas dedicated to cocoa beans in South America",
    subtitle = "Source: FAO Data, comparison year: 1961",
    caption = "#MapPromptMonday, 2023-07-17 // Jesus M. Castagnetto (@jmcastagnetto@mastodon.social)"
  ) &
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title = element_text(family = "Atkinson Hyperlegible", size = 32),
    plot.subtitle = element_text(family = "Atkinson Hyperlegible", size = 22,
                              color = "grey40"),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  plot = plot_combined,
  filename = "2023-07-17_desserts/cocoa-harvested-area-south-america-1961-2021.png",
  width = 16,
  height = 9
)
