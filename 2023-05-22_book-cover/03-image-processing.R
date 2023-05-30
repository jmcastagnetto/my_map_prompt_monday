library(magick)

img <- image_read("2023-05-23_book-cover/peru-elevation-map.png")

img %>%
  image_trim() %>%
  image_emboss(5) %>%
  image_write(
    "2023-05-23_book-cover/peru-trimmed-embossed.png"
  )
