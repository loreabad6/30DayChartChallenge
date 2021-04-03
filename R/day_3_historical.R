# Scrapped data from GoodReads
# Libraries
library(tidyverse)
library(here)
library(ggforce)

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
      str_detect(author, "Hemingway"), "1918",
      ifelse(
        str_detect(author, "Boyne"), "1941",
        years_probably)),
    title = str_trim(str_extract(title,"[^(]+")),
    years = as.Date(paste0(years_probably, "/01/01")),
  ) %>% 
  filter(years_probably > 1700 & years_probably < 2000) %>% 
  arrange(years) 


# Plot
extrafont::loadfonts("win")
plot_family = "Papyrus"
theme_set(theme_void(base_family = plot_family))
theme_update(
  plot.background = element_rect(fill = "steelblue", color = "transparent"),
  panel.background = element_rect(fill = "steelblue", color = "transparent"),
  legend.position = "none"
)

zoom_dates = as.Date(paste0(c(1900, 2020),  "/01/01"))
ggplot(historical_books) +
  aes(x = years_probably, label = years_probably) +
  geom_hline(yintercept = 0, color = "black", size = 2) + 
  geom_point(y = 0, shape = 20, size = 25, color = "darkorange") + 
  geom_text(y = 0, size = 3) + 
  # geom_text(aes(y = 0.5, label = title), angle = 90) +
  scale_y_continuous(limits = c(-2, 2)) #+
  facet_zoom(x = years > zoom_dates[1])
