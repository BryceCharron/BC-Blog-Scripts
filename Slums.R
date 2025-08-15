### Slums 6/11/25

# libraries
library(`readr`)
library(dplyr)
library(tidyverse)

# goal: to visualize a comparison b/t ssa and rest of world (area graph or slope chart)

library(readr)
slums <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Urban Slums Blog Post/slums.csv") %>% 
  filter(GeoAreaName == "Sub-Saharan Africa")

# urban population
years <- as.character(2000:2023)

urbanpop <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/API_SP.URB.TOTL_DS2_en_csv_v2_7768.csv", skip = 3) %>% 
  select(`Country Name`, `Country Code`, years)

# merge datasets
slum_totals <- slums %>% 
  left_join(urbanpop, by = c("GeoAreaName" = "Country Name")) %>% 
  filter(!GeoAreaName %in% c("Central and Southern Asia", "Eastern and South-Eastern Asia",
                             "Europe and Northern America", ""))
