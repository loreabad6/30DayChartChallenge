# Libraries
library(tidyverse)
library(ggtext)
library(scico)
library(here)

# Data
load("data/goodreads_scrapped.rda")
goodreads_export = read_csv("data/goodreads_library_export.csv")

# Wrangle
export = goodreads_export %>% 
  filter(`Exclusive Shelf`=="read")

genres = goodreads %>% 
  mutate(title = str_squish(title)) %>% 
  rowwise() %>% 
  mutate(
    first_genre = book_genres[1],
    second_genre = book_genres[2]
  ) %>%
  left_join(export, by = c("title" = "Title")) %>% 
  mutate(date_read = case_when(
    is.na(`Date Read`) ~ `Date Added`,
    TRUE ~ `Date Read`
  )) %>% 
  select(title, Author, first_genre, second_genre, date_read) %>% 
  mutate(periods = as.factor(case_when(
    format(date_read, "%Y") <= 2010 ~ "2000-2010",
    format(date_read, "%Y") <= 2021 ~ "2011-2021"
    ))
  ) %>% 
  pivot_longer(c(first_genre, second_genre), values_to = "genre") %>% 
  mutate(periods = factor(periods, levels = c("2000-2010", "2011-2021")))

genres_sum = genres %>% 
  count(genre, periods) %>% 
  drop_na() %>% 
  # filter(n > 3) %>% 
  group_by(periods) %>% 
  arrange(desc(n)) %>% 
  mutate(
    rank = row_number(),
    nudge_x = case_when(
      periods == "2000-2010" ~ -0.05,
      periods == "2011-2021" ~ 0.05
    ),
    hjust = case_when(
      periods == "2000-2010" ~ 1,
      periods == "2011-2021" ~ 0
    )
  ) %>% 
  filter(rank <= 12)


# Plot
sysfonts::font_add("FuturaLT", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/FuturaLT.ttf")
sysfonts::font_add("Handwritten", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/NothingYouCouldDo-Regular.ttf")
showtext::showtext_auto()
plot_family = "FuturaLT"
color_bg = "grey80"
color_font = "grey20"
color_panel = "grey50"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = color_bg, color = "transparent"),
  panel.background = element_rect(fill = color_bg, color = "transparent"),
  legend.position = "none",
  plot.margin = margin(rep(6, 4)),
  plot.caption = element_text(
    hjust = 0.5, color = color_panel,
    family = plot_family
  ),
  plot.title = element_text(
    # halign = 0.5,
    hjust = 0.5, size = 18,
    family = plot_family,
    face = "bold", color = color_font
  ),
  plot.subtitle = element_textbox_simple(
    halign = 0.5,
    hjust = 0.5, size = 16,
    family = plot_family,
    face = "bold", color = color_font
  )
  
)

ggplot(genres_sum) + 
  aes(x = periods, y = rank, group = genre, color = rank) +
  geom_line(color = "grey30") +
  geom_point(
    aes(fill = periods),
    shape = 21, color = "black", stroke = 1, size = 3
  ) +
  geom_text(
    aes(label = genre, hjust = hjust),
    nudge_x = genres_sum$nudge_x,
    family = plot_family
  ) +
  scale_color_scico(
    direction = -1, palette = "grayC",
    begin = 0.5, end = 0.8,
  ) +
  scale_fill_manual(values = c("purple", "darkgreen")) +
  scale_y_reverse() +
  labs(
    title = "My top 12 book genres evolution",
    subtitle = "from my <span style = 'color:purple;font-family:Handwritten'>teenage years</span>  to my <span style = 'color:darkgreen;font-family:Handwritten'>adult years</span>",
    caption = paste0(
      "Data: My webscrapped Goodreads data - ", 
      "Visualization: @loreabad6\n", 
      "Challenge: #30DayChartChallenge - ",
      "Day 5: slope - Week 1: comparisons"
    )
  ) 
