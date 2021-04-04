# Libraries
library(tidyverse)
library(here)
library(gganimate)
library(harrypotter)
library(emojifont)
library(ggtext)
library(stringi)

# Wrangle
books = bind_rows(
  harrypotter::philosophers_stone %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 1,
      chapter = row_number(),
      book = "the Philosophers Stone", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::chamber_of_secrets %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 2,
      chapter = row_number(),
      book = "the Chamber of Secrets", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::prisoner_of_azkaban %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 3,
      chapter = row_number(),
      book = "the Prisoner of Azkaban", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::goblet_of_fire %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 4,
      chapter = row_number(),
      book = "the Goblet of Fire", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::order_of_the_phoenix %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 5,
      chapter = row_number(),
      book = "the Order of the Phoenix", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::half_blood_prince %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 6,
      chapter = row_number(),
      book = "the Half Blood Prince", 
      word_count = stringi::stri_count_words(text)
    ),
  harrypotter::deathly_hallows %>% 
    as_tibble() %>% 
    rename(text = value) %>% 
    mutate(
      book_no = 7,
      chapter = row_number(),
      book = "the Deathly Hollows", 
      word_count = stringi::stri_count_words(text)
    )
) %>% 
  mutate(
    book = fct_reorder(factor(book), book_no, min),
    label = emoji("footprints")
  )

books_words = books %>% 
  group_by(book_no, book) %>% 
  summarise(
    word_count = sum(word_count),
    label = first(label)
  )

# Plot
# extrafont::font_import("C:/Windows/Fonts")
# https://www.dafont.com/search.php?q=HARRY+POTTER

# extrafont::loadfonts(device = "win")
sysfonts::font_add("AquilineTwo", "C:/Users/Lore/Downloads/aquiline/AquilineTwo.ttf")
sysfonts::font_add("HarryP", "C:/Users/Lore/Downloads/harry_p/HARRYP__.TTF")
sysfonts::font_add("FontAwesome", "C:/Users/Lore/Downloads/fontawesome-free-5.15.3-desktop/otfs/FontAwesome5Free-Solid-900.otf")
sysfonts::font_add("FuturaLT", "C:/Users/Lore/AppData/Local/Microsoft/Windows/Fonts/FuturaLT.ttf")
showtext::showtext_auto()
plot_family = "AquilineTwo"
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

plot =
  ggplot(books_words) +
  aes(x = book, y = word_count, label = label) +
  geom_text(
    family = "EmojiOne",
    size = 10, angle = c(300,310,320,350,270,250,300),
    fontface = "bold", color = color_font
  ) +
  geom_text(
    aes(label = str_wrap(book, 16)),
    family = plot_family,
    size = 5, nudge_y = -35000,
    # nudge_x = -0.2,
    color = color_font
  ) +
  scale_y_continuous(
    limits = c(0, NA), n.breaks = 6, label = scales::unit_format(unit = "K", scale = 1e-3, sep = "")
  ) +
  labs(
    title = "Word  count  in  <span style = 'font-family:HarryP'>Harry Potter</span>and...",
    caption = paste0(
      "Data: harrypotter R package - ", 
      "Visualization: @loreabad6\n", 
      "Challenge: #30DayChartChallenge - ",
      "Day 4: magical - Week 1: comparisons"
    )
  ) +
  theme(
      panel.grid.major.y = element_line(
        color = "#FEF5E7", linetype = "dashed", size = 0.01
      ),
      axis.text.y = element_text(
        family = font_panel,
        color = color_panel, size = 8
      )
    ) +
  transition_states(
    book,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes(y = "bounce-in") +
  shadow_mark() +
  enter_fade() + 
  exit_shrink()

anim_save("charts/day_4.gif", plot,
          width = 800, height = 400)  
