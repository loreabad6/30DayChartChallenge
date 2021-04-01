# Learning how to do a donut plot.
# From: https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

library(tidyverse)
library(ggfx)
library(ragg)

goodreads = read_csv("data/goodreads_library_export.csv")
exclusive_shelves = goodreads %>% 
  group_by(`Exclusive Shelf`) %>% 
  summarise(count = n()) %>% 
  rename(shelf = `Exclusive Shelf`) %>% 
  filter(shelf %in% c("read", "to-read")) %>% 
  mutate(
    fraction = count/sum(count),
    ymax = cumsum(fraction), 
    ymin = c(0, head(ymax, n=-1))
  )

extrafont::loadfonts("win")
plot_family = "Lydian"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = "grey90", color = "transparent"),
  panel.background = element_rect(fill = "grey90", color = "transparent"),
  legend.position = "none"
)

plot = ggplot(
    exclusive_shelves,
    aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = shelf)
  ) +
  with_inner_glow(
    geom_rect()
  ) +
  geom_text(
    x = 5, family = plot_family,
    aes(y = (ymin+ymax)/2, label = paste0(shelf, ":\n", count))
  ) +
  annotate(
    "text", x = -1, y = 0.5,
    label = "My reading shelves",
    family = plot_family, size = 7
  ) +
  scale_fill_manual(values = c("deepskyblue3", "grey70")) +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  labs(caption = "Data: My Goodreads library export - Visualization: @loreabad6\nChallenge: #30DayChartChallenge - Day 1: part-to-whole - Week 1: comparisons")

agg_png(
  filename = "charts/day_1.png",
  width = 10, height = 12,
  units = "cm", res = 300
)
plot
invisible(dev.off())
knitr::plot_crop("charts/day_1.png")
