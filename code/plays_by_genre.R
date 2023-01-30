# Explore different visualisations of what genres I listen to

# Requirements
source("code/data_import.R")

plays_jan2023

# Packages

library(ggsci)
library(shadowtext)
library(ggpubr)
library(ggthemes)

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

clean_genres_jan2023 %>% 
  ggplot() +
    geom_col(aes(x = reorder(genre, -total_plays), y = total_plays, fill = genre),
             show.legend = F,
             width = 0.6) +
    theme_pubr() +
    theme(
      text = element_text(size = 9),
      plot.subtitle = element_text(size = 11, margin = margin(t = 3, b = 6, unit = "pt")),
      plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10, unit = "pt")),
    )
  
