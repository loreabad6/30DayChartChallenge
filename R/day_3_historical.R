# Scrapped data from GoodReads
# Libraries
library(tidyverse)
library(here)
library(ggforce)
library(ggfx)

# Data: scrapped from Gooodreads
# Code from: https://maraaverick.rbind.io/2017/10/goodreads-part-2/
# See: data/goodreads_scrap.R
# Thank you Mara!
load(here("data", "goodreads_scrapped.rda"))

# Wrangle
historical_books = goodreads %>% 
  filter(map_lgl(book_genres, ~"Historical Fiction" %in% .)) %>% 
  mutate(years_probably = str_extract(book_description, "\\d{4}+")) %>% 
  filter(
    map_lgl(book_genres, ~"War" %in% .) |
    str_detect(str_to_lower(book_description), "war") |
    years_probably > 1700
  ) %>%
  mutate(
    years_probably = ifelse(
      str_detect(author, "Hemingway"), "1917",
      ifelse(
        str_detect(author, "Boyne"), "1941",
        years_probably)),
    title = str_trim(str_extract(title,"[^(]+")),
    event = case_when(
      years_probably %in% c(1939:1941) ~ "WWII",
      str_detect(author, "Hemingway") ~ "WWI",
      years_probably %in% c(1918) ~ "Spanish Flu",
      
    ),
  ) %>% 
  filter(years_probably > 1000 & years_probably < 2000) %>% 
  arrange(years_probably) 


# Plot
extrafont::loadfonts("win")
plot_family = "Tempus Sans ITC"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = "steelblue", color = "transparent"),
  panel.background = element_rect(fill = "steelblue", color = "transparent"),
  legend.position = "none",
  plot.margin = margin(4,4,4,4),
  plot.caption = element_text(hjust = 0),
  plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
)

ggplot(historical_books) +
  aes(x = years_probably, label = years_probably) +
  geom_hline(yintercept = 0, color = "black", size = 2) + 
  geom_mark_circle(aes(label = paste0(title, "\n", author), y = 0),
                 label.family = plot_family, label.fill = "transparent", label.fontsize = 8) +
  with_blur(
    geom_point(y = 0, shape = 20, size = 25, color = "darkorange"), sigma = 2
  ) +
  geom_text(y = 0.015, size = 4, family = plot_family, fontface = "bold") + 
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "When were the historical fiction books I read set?",
    caption = paste0(
      "Data: My webscrapped Goodreads data - ", 
      "Visualization: @loreabad6\n", 
      "Challenge: #30DayChartChallenge - ",
      "Day 3: historical - Week 1: comparisons"
    )
  )

ggsave(
  filename = "charts/day_3.png",
  width = 35, height = 10, device = "png",
  units = "cm", dpi = 300
)
