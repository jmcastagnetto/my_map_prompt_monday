library(rio)
library(rvest)
library(tidyverse)
library(tidygeocoder)
library(rnaturalearth)
library(sf)

# get world cities dataset from simplemaps.com
if (!file.exists("common-data/simplemaps_worldcities_basicv1.75.zip")) {
  download.file(
    url = "https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.75.zip",
    destfile = "common-data/simplemaps_worldcities_basicv1.75.zip"
  )
  cities <- import_list(
    file = "common-data/simplemaps_worldcities_basicv1.75.zip",
    which = "worldcities.csv"
  )[[1]]
  saveRDS(cities, "common-data/world_cities.rds")
}

if (!file.exists("2023-01-23_film-tv/simpson-cities.csv")) {
  doc <- read_html("https://simpsons.fandom.com/wiki/List_of_towns_and_cities")
  elements <- html_elements(doc, "li")
  simpson_cities <- data.frame(location = html_text2(elements[263:409])) %>%
    rowwise() %>%
    mutate(
      city = str_split(location, ",", simplify = TRUE)[1] %>% str_trim(),
      country = str_split(location, ",", simplify = TRUE)[2] %>% str_trim()
    )
  # fix/normalize some cities and countries
  simpson_cities <- simpson_cities %>%
    mutate(
      city = case_when(
        city == "Abu Gharib" ~ "Abu Ghraib",
        city == "Acapulco" ~ "Acapulco de Juárez",
        city == "Basra" ~ "Al Başrah",
        city == "Chamonix" ~ "Chamonix-Mont-Blanc",
        city == "Da Nang" ~ "Đà Nẵng",
        city == "El Giza" ~ "Giza",
        city == "Geneva" & country == "Italy" ~ "Genoa",
        city == "Kiev" ~ "Kyiv",
        city == "Kolkata (Calcutta)" ~ "Kolkāta",
        city == "Krakow" ~ "Kraków",
        city == "Mumbai (Bombay)" ~ "Mumbai",
        city == "Osaka" ~ "Ōsaka",
        city == "Seville" ~ "Sevilla",
        city == "Toulose" ~ "Toulouse",
        TRUE ~ city
      ),
      country = case_when(
        city == "Aberdeen" & country == "Scotland" ~ "United Kingdom",
        city == "Belfast" & country == "Northern Ireland" ~ "United Kingdom",
        city == "Edinburgh" & country == "Scotland" ~ "United Kingdom",
        city == "Glasgow" & country == "Scotland" ~ "United Kingdom",
        city == "Inverness" & country == "Scotland" ~ "United Kingdom",
        city == "Kirkwall" & country == "Scotland" ~ "United Kingdom",
        city == "Liverpool" & country == "England" ~ "United Kingdom",
        city == "London" & country == "England" ~ "United Kingdom",
        city == "Plymouth" & country == "England" ~ "United Kingdom",
        city == "Salzburg" & country == "Germany" ~ "Austria",
        city == "Sarajevo" ~ "Bosnia And Herzegovina",
        city == "Tripoli" ~ "Libya",
        TRUE ~ country
      )
    )
  write_csv(simpson_cities, "2023-01-23_film-tv/simpson-cities.csv")
}

tmp <- simpson_cities %>%
  left_join(
    cities %>%
      select(city, country, lat, lng),
    by = c("city", "country")
  )

unloc <- tmp %>%
  filter(is.na(lat)) %>%
  select(location, city, country)
loc <- tmp %>%
  filter(!is.na(lat))

loc2 <- unloc %>%
  geocode(location, long = lng)

cities_loc <- bind_rows(loc, loc2) %>%
  arrange(location)

world <- ne_countries(scale = 110, returnclass = "sf") %>%
  filter(sov_a3 != "ATA") # remove Antartica

simpsons_cities_map <- ggplot() +
  geom_sf(data = world, aes(fill = income_grp),
          alpha = .35, color = "black") +
  geom_point(data = cities_loc, aes(x = lng, y = lat),
             shape = 10, size = 5, color = "brown") +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  theme_bw(base_family = "Atkinson Hyperlegible") +
  theme(
    legend.position = c(.15,.3),
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
    fill = "World Bank\nIncome Group",
    title = "Cities outside USA mentioned in \"The Simpsons\" (TV series)",
    subtitle = "Sources: https://simpsons.fandom.com and https://simplemaps.com/data/world-cities",
    caption = "2023-01-23, Jesus M. Castagnetto\n@jmcastagnetto@mastodon.social"
  )

ggsave(
  plot = simpsons_cities_map,
  filename = "2023-01-23_film-tv/cities-outside-usa-mentioned-in-the-simpsons.png",
  width = 16,
  height = 9
)
