# Explore different visualisations of what genres I listen to

# Requirements
source("code/data_import.R")

plays_jan2023

# Exploration

plays_jan2023 %>% 
  select(genre, plays) %>% 
  dplyr::count(genre) %>% 
  arrange(desc(n))
