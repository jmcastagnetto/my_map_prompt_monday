# Source: https://data.humdata.org/dataset/ukraine-hno

library(tidyverse)
library(sf)
library(geodata)

url <- "https://data.humdata.org/dataset/019de7ad-8043-4755-b1fb-f84ce4b961a5/resource/9865aa90-d0a4-4d78-845d-3fffa67be0be/download/ukraine-2023-hno-pin-and-severity-for-hdx-20230215.xlsx"

fn <- glue::glue("2023-02-27_ngo-data/{basename(url)}")

if (!file.exists(fn)) {
  download.file(url = url, destfile = fn)
}

affected_by_oblast <- readxl::read_excel(
  path = fn,
  sheet = 3,
  range = "A5:F33"
)

affected_by_oblast <- affected_by_oblast %>%
  slice(2:nrow(affected_by_oblast)) %>%
  janitor::clean_names() %>%
  mutate(
    across(4:6, as.integer),
    pct_in_need = intersectoral / population_estimate,
    oblast_uk = if_else(oblast_p_code == "UA80", "Київ", oblast_uk)
  )

ukraine <- gadm("UA", path = "common-data/") %>%
  st_as_sf() %>%
  mutate(
    NL_NAME_1 = case_match(
      GID_1,
      "UKR.14_1" ~ "Львівська",
      "UKR.5_1" ~ "Дніпропетровська",
      "UKR.6_1" ~ "Донецька",
      "UKR.20_1" ~ "Севастопільська",
      .default = NL_NAME_1
    )
  )

ukr_df <- ukraine %>%
  left_join(
    affected_by_oblast,
    by = c("NL_NAME_1" = "oblast_uk"),
    #multiple = "all"
  ) %>%
  mutate(
    pct_in_need = if_else(is.nan(pct_in_need), NA_real_, pct_in_need)
  )

ukraine_pin_hno_map <- ggplot(ukr_df) +
  geom_sf(aes(fill = pct_in_need), color = "white") +
  geom_sf_text(
    data = ukr_df %>% filter(!is.na(pct_in_need)),
    aes(label = sprintf("%.1f%%", 100 * pct_in_need)),
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#56B1F7",
    high = "#132B43",
    labels = scales::percent,
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(size = 20, face = "bold"),
    strip.background = element_blank(),
    legend.position = "none",
    # legend.position = c(0.05, 0.25),
    # legend.title = element_blank(),
    # legend.text = element_text(size = 12),
    # legend.key.height = unit(1.5, "cm")
  ) +
  labs(
    title = "Ukraine: Percentage of people in need per Oblast (2023-02-15)",
    subtitle = "Source: 'Ukraine: Humanitarian Needs Overview' (OCHA, https://data.humdata.org/dataset/ukraine-hno)",
    caption = "#MapPromptMonday // 2023-02-27, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social",
    fill = ""
  )

ggsave(
  plot = ukraine_pin_hno_map,
  filename = "2023-02-27_ngo-data/ukraine-people-in-need-humanitarian-needs-overview.png",
  width = 16,
  height = 12
)
