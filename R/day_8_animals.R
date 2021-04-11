# Libraries
library(tidyverse)
library(here)
library(sf)
library(osmdata)
library(ggimage)

# Data
# GBIF.org (11 April 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.zdnb2d
salamander = read_tsv(here("data/gbif_fire_salamander.csv"))
sbgland = getbb("Salzburg, AT", format_out = "sf_polygon")
sbgcity = getbb("Salzburg, AT", featuretype = "city", format_out = "sf_polygon")
flachgau = getbb("Flachgau, AT", format_out = "sf_polygon")
tennengau = getbb("Tennengau, AT", format_out = "sf_polygon")

region = rbind(sbgcity, flachgau, tennengau)
sal_img = "https://citizen-conservation.org/wp-content/uploads/2019/05/4_1_2_cc_arten_art_illustration.png"

# Wrangle
salamander_sf = salamander %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

img_sf = tibble(img = sal_img, lat = 47.55, lon = 13.4) 

# Plot
sysfonts::font_add("FuturaLT", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/FuturaLT.ttf")
sysfonts::font_add("Handwritten", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/NothingYouCouldDo-Regular.ttf")
showtext::showtext_auto()
plot_family = "sans"
color_bg = "grey80"
color_font = "grey50"
color_panel = "grey50"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = color_bg, color = "transparent"),
  panel.background = element_rect(fill = color_bg, color = "transparent"),
  legend.position = "none",
  plot.margin = margin(rep(4, 4)),
  plot.caption = element_text(
    hjust = 0.5, color = color_panel,
    family = plot_family, size = 12
  ),
  plot.title = element_text(
    hjust = 0.5, size = 32,
    family = plot_family,
    face = "bold", color = color_font
  ),
  plot.subtitle = element_text(
    hjust = 0.5, size = 28,
    family = plot_family,
    face = "bold", color = color_font
  )
  
)

ggplot(salamander_sf) +
  geom_sf(data = sbg, fill = NA, color = "grey20") +
  geom_sf(data = region, fill = "black", color = NA) +
  stat_density_2d(
    mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
    geom = 'raster',
    contour = FALSE
    # alpha = 0.5
  ) +
  scale_fill_gradientn(
    colors = c("transparent","#f7d926", "#ad7a0d")
    # low = "transparent", mid = "#f6ce1e", high = "#ad7a0d"
    # values = c(NA, "#fffdeb", "#fef8c1", "#fcf396", "#f9ee68", "#f6e82a")
    # breaks = c(0, 25, 45, 65, 85, 100)
  ) +
  geom_image(data = img_sf, aes(image = img, x = lon, y = lat), size = 0.5) +
  coord_sf(xlim = c(12.80, 13.56), ylim = c(47.45, 48.05)) +
  labs(
    # title = "My top 12 book genres evolution",
    # subtitle = "from my <span style = 'color:purple;font-family:Handwritten'>teenage years</span>  to my <span style = 'color:darkgreen;font-family:Handwritten'>adult years</span>",
    caption = paste0(
      "Data: GBIF Occurrence | OpenStreetMap | Salamander image: citizen-conservation.org - ", 
      "\nVisualization: @loreabad6 - ", 
      "Challenge: #30DayChartChallenge | ",
      "Day 8: animals | Week 1: distribution"
    )
  ) 

ggsave(
  filename = "charts/day_8.png",
  width = 12, height = 10, device = "png",
  units = "cm", dpi = 320
)
