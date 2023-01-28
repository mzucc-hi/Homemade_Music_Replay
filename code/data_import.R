# Data import and any cleaning


## Packages
library(tidyverse)
library(scales)
library(janitor)

## Data Import from our data folder
plays_jan2023 <- read_csv("data/play_history_28012023.csv") %>% 
  mutate(
    plays = as.numeric(plays), # convert play count to numeric from chr
    index = seq(1, nrow(.), 1) # add index
         ) %>% 
  filter(plays != 0) # get rid of any songs registered with 0 plays

plays_jan2023


