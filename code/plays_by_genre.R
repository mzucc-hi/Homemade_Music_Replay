# Explore different visualisations of what genres I listen to

# Requirements
source("code/data_import.R")

plays_jan2023

# Packages

library(ggsci)
library(shadowtext)
library(ggpubr)
library(ggthemes)
library(glue)

# Exploration

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


# Visualisation

## Total plays by genre copy to add as data source for labelling

genres_labels <- clean_genres_jan2023 %>% 
  mutate(y = (total_plays + 13))

clean_genres_jan2023 %>% 
  ggplot() +
    geom_col(aes(x = reorder(genre, total_plays), y = total_plays, fill = total_plays),
             colour = "black",
             lwd = 0.1,
             show.legend = F,
             width = 0.6,
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Total Plays by Genre — Jan 2023",
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
  
