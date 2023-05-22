library(tidyverse)
# rds files is about 815MB in size
peru_elev_df <- readRDS("tmp/peru-elev-df.rds")

peru_map <- ggplot(
  peru_elev_df,
  aes(x = x, y = y, fill = elevation)
) +
  geom_tile() +
  scale_fill_gradient2(low = "grey70",
                       mid = "papayawhip",
                       high = "peru") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "none"
  )

ggsave(
  plot = peru_map,
  filename = "2023-05-23_book-cover/peru-elevation-map.png",
  width = 7,
  height = 9,
  dpi = 600
)
