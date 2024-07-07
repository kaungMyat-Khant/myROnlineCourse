library(tidyverse)
library(sf)
gapconti <- gapminder::gapminder %>% select(-1)
gis_conti <- read_sf("c:/docs/R_teaching/data/World_Continents.json")
gapconti$continent <- as.character(gapconti$continent)
gis_data <- left_join(gis_conti,gapconti, by = join_by(CONTINENT==continent)) 
gis_data
