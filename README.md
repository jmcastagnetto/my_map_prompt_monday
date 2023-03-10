# Maps following the #MapPromptMonday weekly mapping project

Ref: [https://github.com/MapPromptMonday/MapPromptMonday](https://github.com/MapPromptMonday/MapPromptMonday)

- Region(s) [[2023-01-02_regions](2023-01-02_regions)]: A simple map showing the number of natural tourist attractions in each region of Perú.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-01-02
  - Data source: https://www.datosabiertos.gob.pe/dataset/inventario-nacional-de-recursos-tur%C3%ADsticos

- Heatmap [[2023-01-09_heatmap](2023-01-09_heatmap)]: A simple heatmap showing the locations of health establishments in Loreto, Perú. Most are closer to the biggest cities in the region, as expected.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-01-09
  - Data source: https://www.datosabiertos.gob.pe/dataset/establecimientos-de-salud

- Colorblind friendly [[2023-01-16_colorblind-friendly](2023-01-16_colorblind-friendly)]: A map using colorblind friendly colors, showing the distribution of peruvians abroad.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-01-16
  - Data source: https://www.datosabiertos.gob.pe/dataset/poblaci%C3%B3n-identificada-con-dni-registro-nacional-de-identificaci%C3%B3n-y-estado-civil-reniec

- Film/TV [[2023-01-23_film-tv](2023-01-23_film-tv)]: A map of the cities outside USA that have been mentioned in "The Simpsons" (TV Series).
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-01-23
  - Data Sources:
    - https://simpsons.fandom.com/wiki/List_of_towns_and_cities
    - https://simplemaps.com/data/world-cities

- Flow Map [[2023-01-30_flow-map](2023-01-30_flow-map)]: A map of the locations visited in the book ["Around the World in Eighty Days" by Jules Verne](https://www.gutenberg.org/ebooks/103) (link to ebook at the Gutenberg project)
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-01-30
  - Data Source:
    - https://en.wikivoyage.org/wiki/Around_the_World_in_Eighty_Days

- Climate/Weather [2023-02-06_climate-weather](2023-02-06_climate-weather): A map showing the differences in average annual temperature in the regions in Peru, using the periods from 1961-1990 and 1991-2020.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-02-06
  - Data Sources:
    - https://climateknowledgeportal.worldbank.org/country/peru/climate-data-historical
    - https://climateknowledgeportal.worldbank.org/download-data 

- Bivariate Map [2023-02-13_bivariate-maps](2023-02-13_bivariate-maps): A map of Peru using the information on Government Density (a measure of institutional government presence), and the HDI (Human Development Index, 2019) vary in Peru at the level of provinces. It is clear that locations in the Coast of Peru tend to have both more government presence and better HDI.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-02-13
  - Data Sources: 
    - Originally from: https://www.ceplan.gob.pe/informacion-sobre-zonas-y-departamentos-del-peru/
    - Data pre-processed at: https://github.com/jmcastagnetto/ubigeo-peru-aumentado/

- Black and White / Grayscale [2023-02-20_bw-grayscale](2023-02-20_bw-grayscale): Maps showing the increment in cases of Dengue in Peru from 2022 to 2023, using data up to epidemiological week #6 of each year.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-02-20
  - Data Source:
    - "Sala Situacional de Dengue": https://www.dge.gob.pe/sala-situacional-dengue/ (data downloaded on 2023-02-19)

- NGO Data [2023-02-27_ngo-data](2023-02-27_ngo-data): A map showing the percentage of population per Oblast in Ukraine, considered as "people in need" according to the Humanitarian Needs Overview of OCHA (as of Feb 15, 2023).
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-02-27
  - Data Source:
    - "Ukraine: Humanitarian Needs Overview": https://data.humdata.org/dataset/ukraine-hno

- Favorite Animal [2023-03-06_favorite-animal](2023-03-06_favorite-animal): A map showing the geographical range for the Andean Bear (*Tremarctos ornatus*).
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-03-06
  - Data Sources:
    - "Andean Bear": https://www.iucnredlist.org/species/22066/123792952
    - Bear image: https://commons.wikimedia.org/wiki/File:OursChaparriF_(12).jpg

- Earth Imagery [2023-03-13_earth-imagery](2023-03-13_earth-imagery): An elevation map of Perú as a 2D and 3D representation
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-03-13
  - Data Source:
    - Elevation at 30 arcseconds from [geodata](https://github.com/rspatial/geodata)

- Inspired by... [2023-03-20_inspired-by](2023-03-20_inspired-by): A streetmap of Callao, Perú, inspired by the excellent article: "[Streetmaps](https://ggplot2tutor.com/streetmaps/streetmaps/)", which uses [ggplot2](https://ggplot2.tidyverse.org/) and [osmdata](https://docs.ropensci.org/osmdata/) to create nice streetmaps of Freiburg. The original article retouched the map afterwards, here I'm trying to use only R.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-03-20
  - Data Source: [osmdata](https://docs.ropensci.org/osmdata/)
  
- Recent Environmental Disaster [2023-03-27_recent-environmental-disaster](2023-03-27_recent-environmental-disaster): Maps of affected regions in Peru for the top three institutions sanctioned for environmental violations, using the information from OEFA.
  - Prompt: https://github.com/MapPromptMonday/MapPromptMonday/tree/main/Prompts/2023/2023-03-27
  - Data Source: OEFA - Administrados Sancionados](https://publico.oefa.gob.pe/administrados-sancionados/#/)