# Explore different visualisations of what genres I listen to


# Requirements ------------------------------------------------------------

source("code/data_import.R")

plays_jan2023

# Packages

library(ggsci)
library(shadowtext)
library(ggpubr)
library(ggthemes)
library(glue)
library(ggforce)


# Exploration -------------------------------------------------------------


plays_jan2023 %>% 
  select(genre, plays) %>% 
  dplyr::count(genre) %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(genre)) %>% 
  print(n = Inf)

# We likely want to combine some of these genres. E.g., we have multiple 'rock''related categories
# Let's first save a copy of our genre data to work with

genres_jan2023 <- plays_jan2023 %>% 
  select(genre, plays) %>% 
  dplyr::count(genre) %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(genre))

# Now let's combine some categories

(
  clean_genres_jan2023 <- genres_jan2023 %>% 
  mutate(
    genre = case_when(
      str_detect(genre, "Rock") ~ "Rock Genres",
      str_detect(genre, "Folk") ~ "Folk Genres",
      str_detect(genre, "Rap") ~ "Hip-Hop/Rap",
      str_detect(genre, "Religious") ~ "Christian",
      str_detect(genre, "Gospel") ~ "Christian",
      str_detect(genre, "CCM") ~ "Christian",
      TRUE ~ genre
      ),
    plays = n
    ) %>% 
  select(-n) %>% 
  group_by(genre) %>% 
  summarise(total_plays = sum(plays)) %>% 
  filter(total_plays > 10) %>% 
  arrange(-total_plays)
)



# Visualisation -----------------------------------------------------------

### Total plays by genre copy to add as data source for labelling

genres_labels <- clean_genres_jan2023 %>% 
  mutate(y = (total_plays + 13))

### Extract most-played song for top 3 genres for extra detail

top_song_label <- plays_jan2023 %>% 
  filter(genre == "Alternative" | genre == "Pop" | genre == "Singer/Songwriter" |
         genre == "Christian") %>% 
  group_by(genre) %>% 
  arrange(desc(plays), .by_group = T) %>% 
  slice_head(n = 1) %>% 
  select(title, artist, genre, plays) %>% 
  # Use a join to get total plays by genre
  left_join(genres_labels, by = "genre") %>% 
  mutate(percent_of_genre_plays = (plays/total_plays) * 100) %>% 
  select(-c(total_plays, y))


### Plot

clean_genres_jan2023 %>% 
  ggplot() +
    geom_col(aes(x = reorder(genre, total_plays), y = total_plays, fill = total_plays),
             colour = "black",
             lwd = 0.1,
             show.legend = F,
             width = 0.6,
             alpha = 0.65) +
    geom_col(data = top_song_label,
             aes(x = genre, y = plays),
             alpha = 0.1,
             colour = "black",
             lwd = 0.4,
             width = 0.6) +
    ggforce::geom_mark_ellipse(
      data = top_song_label,
      aes(x = genre, y = plays, label = paste0("'", title, "', ", artist)),
      expand = unit(0, "mm"),
      label.margin = margin(b =0, l = 0, r = 0, t = 1.5, "mm"),
      label.buffer = unit(1, "mm"),
      label.hjust = 1,
      label.lineheight = 1,
      label.fill = NA,
      label.fontsize = 9,
      label.family = "sans",
      label.fontface = "plain",
      con.size = 0.25,
      con.type = "straight",
      con.border = "none"
    ) +
    coord_flip() +
    labs(title = "Total Plays by Genre as of Jan 2023",
         subtitle = "Apple Music Song Library",
         y = "",
         x = "",
         caption = glue(R.version.string, ", Matteo Zucchi")) +
    geom_hline(yintercept = 0, size = 0.2) +
    scale_y_continuous(expand = c(0, 10), limits = c(0, 900)) +
    geom_text(data = genres_labels,
              aes(x = reorder(genre, total_plays), y = y, label = total_plays),
              nudge_y = 25,
              colour = "black",
              size = 3) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    theme_pubr() +
    theme(
      text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, margin = margin(b = 6, unit = "pt")),
      plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10, unit = "pt")),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank()
    )
  

# Save

ggsave("figures/genre_barchart.png", width = 15, height = 20, units = "cm")

