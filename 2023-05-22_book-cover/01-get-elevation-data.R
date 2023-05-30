library(tidyverse)
library(rnaturalearth)
library(elevatr)

peru <- ne_countries(country = "PERU", scale = 10, returnclass = "sf")
peru_elev <- get_elev_raster(
  locations = peru,
  z = 9,
  clip = "locations"
)
peru_elev_df <- peru_elev %>%
  terra::as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  rename(
    elevation = 3
  )
# rds files is about 815MB in size
saveRDS(peru_elev_df, file = "tmp/peru-elev-df.rds")
