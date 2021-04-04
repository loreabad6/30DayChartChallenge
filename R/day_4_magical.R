# Libraries
library(tidyverse)
library(here)
library(ggforce)
library(ggfx)
library(gganimate)
library(ggimage)
library(harrypotter)
library(emojifont)
library(ggtext)

# Wrangle
test = data.frame(
  book = c(1:7), 
  words = sample(1000:3000, 7)
) %>% 
  mutate(label = emoji("footprints"))

# Plot
# extrafont::font_import("C:/Windows/Fonts")
extrafont::loadfonts("win")
plot_family = "Harry P"
color_bg = "#D4BD84FF"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = color_bg, color = "transparent"),
  panel.background = element_rect(fill = color_bg, color = "transparent"),
  legend.position = "none",
  plot.margin = margin(4,4,4,4),
  plot.caption = element_text(hjust = 0),
  plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
)

ggplot(test) +
  aes(x = book, y = words, label = label) +
  geom_line(linetype = "dashed") +
  geom_text(
    family = "EmojiOne", size = 10, angle = 300,
    fontface = "bold", color = "#761919FF"
  ) +
  labs(
    title = "",
    subtitle = "Word count for every ",
    caption = paste0(
      "Data: {harrypotter} R package - ", 
      "Visualization: @loreabad6\n", 
      "Challenge: #30DayChartChallenge - ",
      "Day 3: magical - Week 1: comparisons"
    )
  )

ggsave(
  filename = "charts/day_4.png",
  width = 35, height = 10, device = "png",
  units = "cm", dpi = 300
)
