# Libraries
library(tidyverse)
library(here)
library(sf)
library(osmdata)
library(ggimage)
library(ggtext)
library(ggfx)

# Data
# GBIF.org (11 April 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.zdnb2d
salamander = read_tsv(here("data/gbif_fire_salamander.csv"))
sbgland = getbb("Salzburg, AT", format_out = "sf_polygon")
sbgcity = getbb("Salzburg, AT", featuretype = "city", format_out = "sf_polygon")
flachgau = getbb("Flachgau, AT", format_out = "sf_polygon")
tennengau = getbb("Tennengau, AT", format_out = "sf_polygon")

region = rbind(sbgcity, flachgau, tennengau) %>% 
  mutate(region = c("Salzburg", "Salzburg-Umgebung", "Tennengau"))
sal_img = "https://citizen-conservation.org/wp-content/uploads/2019/05/4_1_2_cc_arten_art_illustration.png"

# Wrangle
salamander_sf = salamander %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

img_sf = tibble(img = sal_img, lat = 47.55, lon = 13.4) 

# Plot
plot_family = "sans"
color_bg = "#fcf3bd"
color_font = "black"
color_panel = "grey50"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = color_bg, color = "transparent"),
  panel.background = element_rect(fill = color_bg, color = "transparent"),
  legend.position = "none",
  plot.margin = margin(rep(0, 4)),
  plot.caption = element_text(
    hjust = 0.5, color = color_panel,
    family = plot_family, size = 4, 
    margin = margin(t = -15) 
  )
)

ggplot(salamander_sf) +
  with_outer_glow(
    geom_sf(data = sbgland, fill = NA, color = "black", size = 0.5)
  ) +
  geom_sf(data = region, fill = "black", color = "black") +
  stat_density_2d(
    mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
    geom = 'raster',
    contour = FALSE, n = 300, adjust = 0.8
  ) +
  annotate(
    geom = "text",
    y = 47.493,
    x = 12.921,
    label = "Germany\nAustria",
    size = 1.2,
    color = "grey70",
    angle = 330, 
    alpha = 0.7
  ) +
  scale_fill_gradientn(
    colors = c("transparent", "#f7d926", "#ad7a0d")
  ) +
  geom_image(data = img_sf, aes(image = img, x = lon, y = lat), size = 0.5) +
  annotate(
    geom = "richtext",
    x = 13.45, y = 47.9, size = 2.7,
    label = "Observations of the<br>fire salamander<br><span style='color:grey50'>*Salamandra salamandra*</span>",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  annotate(
    geom = "richtext",
    x = 13.3, y = 47.63, size = 2, color = "#f7d926",
    label = "Hotspot map of observations from 2019<br>collected from observation.org around<br>**Salzburg**, Austria",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  annotate(
    geom = "richtext",
    x = 12.95, y = 47.65, size = 1.5,
    label = '*"The fire salmander has*<br>*a vulnerable status according to*<br>*the IUCN Red List in Salzburg.*<br>*Deforestation is its main threat.*"',
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  coord_sf(xlim = c(12.80, 13.56), ylim = c(47.45, 47.98)) +
  labs(
    caption = paste0(
      "Data: GBIF Occurrence & OpenStreetMap | Salamander image: citizen-conservation.org | Text: Ankel (2020), Haus der Natur", 
      "\nVisualization: @loreabad6 | ", 
      "Challenge: #30DayChartChallenge | ",
      "Day 8: animals | Week 2: distributions"
    )
  ) 

ggsave(
  filename = "charts/day_8.png",
  width = 12, height = 10, device = "png",
  units = "cm", dpi = 320
)

knitr::plot_crop("charts/day_8.png")
