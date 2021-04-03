# Scrapping data from GoodReads
# Code from: https://maraaverick.rbind.io/2017/10/goodreads-part-2/
# Thank you Mara!

library(tidyverse)
library(here)
library(rvest)
startUrl = "https://www.goodreads.com/review/list/7703473-lore"


# function to get book descriptions
getBookDescription <- function(bookLink) {
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_node("#descriptionContainer") %>%
    html_text() %>%
    trimws()
}

# function to get book genres
get_genres <- function(bookLink){
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text(trim = TRUE)
}

# function to get books
getBooks <- function(i) {
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i, "&shelf=read")
  
  html <- read_html(url)
  
  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE) #%>%
  #discard(!str_detect(., "[A-Z0-9]"))
  
  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))
  
  bookLinks <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(!str_detect(., "^/book/show")) %>%
    na.omit() %>%
    unique()
  
  bookDescription <- bookLinks %>%
    map_chr(getBookDescription)
  
  bookGenre <- bookLinks %>%
    map(get_genres)
  
  return(tibble(
    title = title,
    author = author,
    book_description = bookDescription,
    book_genres = bookGenre
  ))
}

# get books (30 per page, 207 books -> 7 pages)
goodreads <- c(1:7) %>%
  map_dfr(getBooks)

# save the output
save(goodreads,file = here("data", "goodreads_scrapped.rda"))
