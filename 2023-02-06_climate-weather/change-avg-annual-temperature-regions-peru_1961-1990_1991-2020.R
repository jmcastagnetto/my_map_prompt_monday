# Source: https://climateknowledgeportal.worldbank.org/country/peru/climate-data-historical

library(tidyverse)
library(sf)
library(geodata)

peru <- gadm("PE", path = "common-data/") %>%
  st_as_sf() %>%
  mutate( # to prepare for merging Lima and Lima Province
    REGIÓN = if_else(
      NAME_1 %in% c("Lima", "Lima Province"),
      "Lima",
      NAME_1
    ) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_to_title()
  ) %>%
  group_by(REGIÓN) %>%
  summarise() %>%
  ungroup()

data_rds <- "2023-02-06_climate-weather/cru-obs-avgtemp-periods-peru.rds"

if (!file.exists(data_rds)) {
  get_annual_data <- function(fn) {
    period <- str_extract(fn, "\\d{4}-\\d{4}")
    df <- read_csv(
      file = fn,
      skip = 2,
      col_names = c("location", "temp"),
      col_types = cols(
        .default = col_skip(),
        location = col_character(),
        temp = col_number()
      )
    ) %>%
      add_column(period = period)
    return(df)
  }
  t_csvs <- fs::dir_ls(path = "2023-02-06_climate-weather",
                       glob = "*.csv")

  t_peru <- tibble()
  for (fn in t_csvs) {
    tmp <- get_annual_data(fn)
    t_peru <- bind_rows(t_peru, tmp)
  }
  t_peru <- t_peru %>%
    arrange(location, period) %>%
    group_by(location) %>%
    mutate(
      temp_diff = temp - lag(temp)
    )
  saveRDS(t_peru, data_rds)


}

t_peru_last <- readRDS(data_rds) %>%
  filter(period == "1991-2020") %>%
  select(REGIÓN = location, temp_diff) %>%
  mutate(
    REGIÓN = iconv(REGIÓN, to = "ASCII//TRANSLIT") %>%
      str_replace("Madre de Dios", "Madre De Dios")
  )

peru_df <- peru %>%
  left_join(
    t_peru_last,
    by = "REGIÓN"
  )

change_temp_peru <- ggplot(
  data = peru_df,
  aes(fill = temp_diff)
) +
  geom_sf(color = "white", linewidth = .5) +
  scale_fill_distiller(
    direction = 1,
    palette = "Reds"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 17, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.title = element_text(size = 14, vjust = 1),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1.5, "cm"),
    legend.position = c(.1,.3)
  ) +
  labs(
    title = "Perú: Change in average\nannual temperature between\n1961-1990 and 1991-2020",
    subtitle = "Source: World Bank's historical climate data",
    caption = "#MapPromptMonday\n2023-02-06, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social",
    fill = "Change (°C)"
  )

ggsave(
  plot = change_temp_peru,
  filename = "2023-02-06_climate-weather/change-avg-annual-temperature-regions-peru_1961-1990_1991-2020.png",
  height = 11,
  width = 6.5
)
