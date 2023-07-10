library(tidyverse)
library(rnaturalearth)
library(sf)

# Windows has issues with loading fonts sometimes, unlike Linux
# (I try to test my R code in Win once in a blue moon)
if (.Platform$OS.type == "windows") {
  library(extrafont)
  loadfonts(device = "win", quiet = TRUE)
}

world <- ne_countries(scale = 110, returnclass = "sf")

tcl <- read_csv(
  "2023-06-26_plants/TCL_ind_na.csv",
  col_types = cols(
    .default = col_number(),
    iso = col_character(),
    code = col_character(),
    country = col_character()
  )
) %>%
  pivot_longer(
    cols = 4:31,
    names_to = "year",
    values_to = "tcl",
    names_prefix = "TCL.ind."
  ) %>%
  mutate(
    year = as.integer(year)
  )

sam <- world %>%
  filter(continent == "South America" & iso_a3 != "FLK")

tcl_2000_2022 <- tcl %>%
  filter(year == 2000 | year == 2022) %>%
  filter(iso %in% sam$iso_a3)

plot_df <- sam %>%
  left_join(
    tcl_2000_2022,
    by = c("iso_a3" = "iso")
  )

tcl_map <- ggplot(plot_df) +
  geom_sf(aes(fill = tcl), color = "grey80") +
  scale_fill_continuous(
    name = "% Loss\nof Tree\nCoverage",
    low = "#FF7276",
    high = "black",
    n.breaks = 9
  ) +
  labs(
    title = "Loss of tree coverage in South America in 2000 and 2022",
    subtitle = "Source: 2022 Environmental Performance Index (Component: TCL index)",
    caption = "#MapPromptMonday, 2023-06-26, Jesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.margin = unit(rep(1, 4), "cm"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 20, color = "grey30",
                                 margin = margin(t = 5, b = 10)),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    strip.text = element_text(size = 20, face = "bold", color = "white",
                              margin = margin(t = 5, b = 5)),
    strip.background = element_rect(fill = "black"),
    legend.key.height = unit(2, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(~year)

ggsave(
  plot = tcl_map,
  filename = "2023-06-26_plants/tree-coverage-loss-south-america-2000-2022.png",
  width = 11,
  height = 9
)
