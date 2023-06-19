library(tidyverse)
library(sf)
library(geodata)
library(extrafont)

loadfonts(device = "win", quiet = TRUE)

load_raw <- function(fname) {
  period <- str_extract(fname, "\\d{4}-\\d{4}")
  read_csv(
    file = fname,
    skip = 2,
    col_names = c("location","annual","djf","mam","jja","son"),
    col_types = cols(
      .default = col_number(),
      location = col_character()
    )
  ) %>%
  add_column(period = period, .before = 1)
}

p1_min <- load_raw("2023-06-19_solstice-seasons/tasmin_climatology_annual-seasonal_cru_1901-1930_PER.csv")
p1_max <- load_raw("2023-06-19_solstice-seasons/tasmax_climatology_annual-seasonal_cru_1901-1930_PER.csv")
p2_min <- load_raw("2023-06-19_solstice-seasons/tasmin_climatology_annual-seasonal_cru_1991-2020_PER.csv")
p2_max <- load_raw("2023-06-19_solstice-seasons/tasmax_climatology_annual-seasonal_cru_1991-2020_PER.csv")

save(p1_min, p1_max, p2_min, p2_max,
     file = "2023-06-19_solstice-seasons/temps-1901_1930-1991_2020.Rdata")

# Summer months: Dec, Jan, Feb

comb_min <- p1_min %>%
  select(location, djf_1 = djf) %>%
  left_join(
    p2_min %>%
      select(location, djf_2 = djf),
    by = "location"
  ) %>%
  mutate(
    diff_min = djf_2 - djf_1
  )

comb_max <- p1_max %>%
  select(location, djf_1 = djf) %>%
  left_join(
    p2_max %>%
      select(location, djf_2 = djf),
    by = "location"
  ) %>%
  mutate(
    diff_max = djf_2 - djf_1
  )

plot_df <- comb_min %>%
  select(location, diff_min) %>%
  left_join(
    comb_max %>%
    select(location, diff_max),
    by = "location"
  ) %>%
  pivot_longer(
    cols = c(diff_min, diff_max),
    names_to = "type",
    values_to = "temp_diff"
  ) %>%
  filter(location != "Peru")%>%
  mutate(
    location = str_to_upper(location) %>%
      iconv(to = "ASCII//TRANSLIT"),
    type = str_replace_all(
      type,
      c(
        "diff_min" = "Change in Min. Temperature",
        "diff_max" = "Change in Max. Temperature"
      )
    )
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
saveRDS(peru, "common-data/peru-regions-sf-map.rds")

map_df <- peru %>%
  left_join(plot_df, by = c("REGION" = "location"))

temp_diff_map <- ggplot(map_df) +
  geom_sf(aes(fill = temp_diff)) +
  scale_fill_steps2(
    name = "Temperature\nDifference (°C)",
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    show.limits = TRUE,
    limits = c(-0.1, 0.5)
  ) +
  theme_void(
    base_family = "Atkinson Hyperlegible",
    base_size = 18
  ) +
  labs(
    title = "Change in Max. and Min. Temperatures\nduring Summer in Regions of Peru",
    subtitle = "Comparing periods [1901-1930] and [1991-2020]\n(Source: Climate Knowledge Portal, World Bank)",
    caption = "#MapPromptMonday, 2023-06-19\nJesus M. Castagnetto, @jmcastagnetto@mastodon.social"
  ) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20, color = "grey30",
                                  margin = margin(t = 5, b = 10)),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    strip.text = element_text(size = 20, face = "italic", color = "white",
                              margin = margin(rep(10, 4))),
    strip.background = element_rect(fill = "black"),
    plot.margin = unit(rep(1, 4), "cm"),
    legend.key.height = unit(1.5, "cm")
  ) +
  facet_wrap(~type)

ggsave(
  plot = temp_diff_map,
  filename = "2023-06-19_solstice-seasons/peru-regional-min-seasonal-temp-change.png",
  width = 11,
  height = 10
)
