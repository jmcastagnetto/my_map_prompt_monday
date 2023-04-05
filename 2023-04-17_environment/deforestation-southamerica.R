# Data source: https://fra-data.fao.org/assessments/fra/2020
library(tidyverse)
library(geodata)
library(sf)
library(patchwork)

cc <- country_codes() %>%
  select(
    country = NAME_FAO,
    iso3 = ISO3
  )

total_area <- read_csv(
  file = "2023-04-17_environment/fra-forestCharacteristics.csv",
  col_names = c("country", "total")
) %>%
  slice(3:16) %>%
  mutate(
    total = as.numeric(total)
  ) %>%
  left_join(
    cc,
    by = "country"
  ) %>%
  filter(!is.na(iso3)) %>%
  select(-country)

loss_area <- read_csv(
  "2023-04-17_environment/fra-forestAreaChange.csv"
) %>%
  janitor::clean_names()
yrs <- as.character(loss_area[1, ])
yrs[1] <- "country"
loss_area <- slice(loss_area, 2:15)
colnames(loss_area) <- yrs
loss_area <- loss_area %>%
  mutate(
    across(2:5, as.numeric)
  ) %>%
  left_join(
    cc,
    by = "country"
  ) %>%
  filter(!is.na(iso3)) %>%
  left_join(
    total_area,
    by = "iso3"
  ) %>%
  pivot_longer(
    cols = 2:5,
    names_to = "range",
    values_to = "loss"
  ) %>%
  mutate(
    pct = 100 * loss / total
  )

sam_map <- world(path = "common-data/") %>%
  st_as_sf() %>%
  filter(GID_0 %in% as.character(loss_area$iso3)) %>%
  left_join(
    loss_area %>% select(-country),
    by = join_by(GID_0 == iso3)
  )

p1 <- ggplot(sam_map) +
  geom_sf(aes(fill = loss)) +
  scale_fill_fermenter(direction = 1, palette = "Reds", show.limits = TRUE) +
  coord_sf(xlim = c(-85, -30)) +
  facet_wrap(~range, nrow = 1, strip.position = "left") +
  labs(
    fill = "Area loss (x 1000ha) "
  ) +
  theme_void(base_family = "Atkins Hyperlegible") +
  theme(
    #plot.background = element_rect(color = "black", fill = "white")
  )

p2 <- ggplot(sam_map) +
  geom_sf(aes(fill = pct)) +
  scale_fill_fermenter(direction = 1, palette = "Blues", show.limits = TRUE) +
  coord_sf(xlim = c(-85, -30)) +
  facet_wrap(~range, nrow = 1, strip.position = "left") +
  labs(
    fill = "% Area loss "
  ) +
  theme_void(base_family = "Atkins Hyperlegible") +
  theme(
    #plot.background = element_rect(color = "black", fill = "white")
  )

p_12 <- (p1 / p2) +
  plot_annotation(
    title = "Forest area loss in South America over the years",
    subtitle = "Source: 'Global Forest Resources Assessment 2020' (FAO)",
    caption = "@jmcastagnetto@mastodon.social - Jesus M. Castagnetto // #MapPromptMonday 2023-04-17"
  ) &
  theme(
    legend.title = element_text(size = 16, face = "italic"),
    legend.position = "top",
    legend.key.width = unit(2, "cm"),
    strip.text = element_text(angle = 90, size = 14),
    #strip.background = element_rect(color = "grey80", fill = "gray80"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, colour = "gray50"),
    plot.caption = element_text(family = "Inconsalata")
  )

ggsave(
  plot = p_12,
  filename = "2023-04-17_environment/deforestation-southamerica.png",
  height = 8,
  width = 11
)
