# Scrapped data from GoodReads
# Code from: https://maraaverick.rbind.io/2017/10/goodreads-part-2/
# See: data/goodreads_scrap.R
# Thank you Mara!

library(tidyverse)
library(here)
load(here("data", "goodreads_scrapped.rda"))

goodreads %>% 
  filter(book_genres)