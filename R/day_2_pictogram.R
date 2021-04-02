#Libraries
library(tidyverse)
library(ragg)
library(waffle)

# Data
goodreads = read_csv("data/goodreads_library_export.csv")

# Wrangle
read_books = goodreads %>% 
  filter(`Exclusive Shelf` == "read") %>%
  mutate(date_read = case_when(
    is.na(`Date Read`) ~ `Date Added`,
    TRUE ~ `Date Read`
  )) %>% 
  select(Title, Author, date_read) %>% 
  mutate(groups_years = case_when(
    format(date_read, "%Y") <= 2005 ~ "2001-2005",
    format(date_read, "%Y") <= 2010 ~ "2006-2010",
    format(date_read, "%Y") <= 2015 ~ "2011-2015",
    format(date_read, "%Y") <= 2021 ~ "2016-2021"
  ))

read_years_count = read_books %>% 
  count(groups_years)

# Themes & settings
extrafont::loadfonts("win")
plot_family = "Futura LT"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = "grey90", color = "transparent"),
  panel.background = element_rect(fill = "grey90", color = "transparent"),
  legend.position = "none"
)

# Plot
ggplot(read_books, aes(y = ..count.., x = groups_years)) +
  # geom_dotplot(binwidth = 1) +
  geom_text(label = "A") +
  theme_bw()
  