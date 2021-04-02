#Libraries
library(tidyverse)
library(ragg)
library(waffle)
library(extrafont)

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
chart_family = "Amatic SC"
plot_family = "Futura LT"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = "cornsilk", color = "transparent"),
  panel.background = element_rect(fill = "cornsilk", color = "transparent"),
  legend.position = "none",
  plot.margin = margin(2,2,2,2)
)


# Plot
ggplot(read_years_count, aes(label = 1, values = n)) + 
  geom_pictogram(
    color = "saddlebrown",
    size = 4, 
    n_rows = 5,
    flip = TRUE, 
    family = "Font Awesome 5 Free Solid"
  ) +
  scale_label_pictogram(values = c("book")) +
  facet_wrap(~groups_years, nrow = 1, strip.position = "left") +
  theme(
    text = element_text(color = "saddlebrown"),
    strip.text = element_text(family = chart_family, size = 25, face = "bold", 
                              vjust = 0.5, hjust = 0.05),
    plot.title = element_text(family = chart_family, size = 25, face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  coord_equal() +
  labs(
    title = "Not really a book worm... or am I?",
    subtitle = str_wrap(
      paste(
        "Interestingly, my book records go back to 2003.",
        "Considering I am 27 yo, I must admit I am very proud of my young self by keeping it up.",
        "Here is how many books I read over the years."
      ),
      65
    ),
    caption = paste0(
      "Data: My Goodreads library export - ", 
      "Visualization: @loreabad6\n", 
      "Challenge: #30DayChartChallenge - ",
      "Day 2: pictogram - Week 1: comparisons"
    )
  )

ggsave(
  filename = "charts/day_2.png",
  width = 15, height = 15, device = "png",
  units = "cm", dpi = 300
)
knitr::plot_crop("charts/day_2.png")
