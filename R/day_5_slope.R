# Libraries
library(tidyverse)
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
  mutate(first_genre = book_genres[1]) %>%
  left_join(export, by = c("title" = "Title")) %>% 
  mutate(date_read = case_when(
    is.na(`Date Read`) ~ `Date Added`,
    TRUE ~ `Date Read`
  )) %>% 
  select(title, Author, first_genre, date_read) %>% 
  mutate(periods = factor(case_when(
    format(date_read, "%Y") <= 2010 ~ "2000-2010",
    format(date_read, "%Y") <= 2017 ~ "2011-2017",
    format(date_read, "%Y") <= 2021 ~ "2011-2021"
    ), ordered = T)
  )

genres_sum = genres %>% 
  count(first_genre, periods) %>% 
  drop_na() %>% 
  filter(n > 1) %>% 
  group_by(periods) %>% 
  arrange(n) %>% 
  mutate(rank = row_number())


# Plot
sysfonts::font_add("FuturaLT", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/FuturaLT.ttf")
showtext::showtext_auto()
font_panel = "FuturaLT"
color_bg = "#e1d9c4"
color_font = "#5b1012"
color_panel = "#6c523d"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = color_bg, color = "transparent"),
  panel.background = element_rect(fill = color_bg, color = "transparent"),
  legend.position = "none",
  plot.margin = margin(rep(6, 4)),
  plot.caption = element_text(
    hjust = 0, color = color_panel,
    family = font_panel
  ),
  plot.title = element_textbox_simple(
    hjust = 0, size = 20,
    family = plot_family,
    face = "bold", color = color_font
  )
)

ggplot(genres_sum) + 
  aes(x = periods, y = rank, group = first_genre) +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = first_genre), nudge_x = 0.25
  )
