### World Refugee Day 6/13/25

# libraries
library(`readr`)
library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages('ggalluvial')
library(ggalluvial)
library(tidyr)
library(readxl)
library(readr)
library(reshape2)
library(stringr)
library(forcats)
library(wbstats)
library(wbgcharts)
library(wbggeo)
library(scales)
library(ragg)
library(ggbeeswarm)

setwd("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post")

# read in UNRWA dfs
ref_asylum <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/Refugee Asylum Data/API_SM.POP.RRWA.EA_DS2_en_csv_v2_7687.csv", 
                    skip = 3) %>% 
  select(`Country Name`, `Country Code`, `2023`) %>% 
  na.omit()
ref_origin <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/Refugee Origin Data/API_SM.POP.RRWA.EO_DS2_en_csv_v2_7689.csv",
                       skip = 3) %>% 
  select(`Country Name`, `Country Code`, `2023`) %>% 
  na.omit()

# read in UNHCR df
ref_total <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/Refugee Data/persons_of_concern.csv") %>% 
  filter(Year == "2023") %>% 
  select(Year, `Country of Asylum`, `Country of Origin`, `Country of Asylum ISO`, `Country of Origin ISO`,
         Refugees)

# read in UNHCR income classification df
ref_class <- read_excel("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/CLASS.xlsx")

# create UNHCR df - count of refugees moving b/t income groups
ref_unhcr <- ref_total %>%
  left_join(ref_class, by = c("Country of Asylum ISO" = "Code")) %>%
  left_join(ref_class, by = c("Country of Origin ISO" = "Code")) %>%
  select(Year, 
    `Country of Asylum`, 
    `Country of Origin`, 
    `Country of Asylum ISO`, 
    `Country of Origin ISO`, 
    Refugees, 
    `Income group.x`, 
    `Income group.y`) %>%
  rename(origin_income_group = `Income group.y`,
    asylum_income_group = `Income group.x`) %>%
  na.omit() %>%
  group_by(origin_income_group, asylum_income_group) %>%
  summarise(total_refugees = sum(Refugees), .groups = "drop") 

# write .csv
write.csv(ref_unhcr, "ref_unhcr.csv", row.names = F)

# create UNRWA df - count of refugees moving from West Bank & Gaza to income classifications
ref_unrwa <- ref_asylum %>% 
  left_join(ref_origin, by = "Country Name") %>% 
  rename(total_refugees = `2023.x`,
         asylum_income_group = "Country Name") %>% 
  mutate(origin_income_group = "West Bank and Gaza",
         origin_total = 5936247) %>% 
  select(asylum_income_group, origin_income_group, total_refugees, origin_total) %>% 
  na.omit() %>% 
  filter(asylum_income_group %in% c("Lower middle income", "Low income"))

# write .csv
write.csv(ref_unrwa, "ref_unrwa.csv", row.names = F)

# combine UNHCR and UNRWA
ref_combined <- bind_rows(ref_unhcr, ref_unrwa) 

ref_dest <- ref_combined %>%
  group_by(asylum_income_group) %>%
  summarise(dest_total = sum(total_refugees), .groups = "drop") %>%
  mutate(dest_total_pct = 100 * dest_total / sum(dest_total))

# form final df with UNHCR and UNRWA data with flows and percentages (for sankey)
ref_final <- ref_combined %>%
  left_join(ref_dest, by = "asylum_income_group") %>%
  group_by(origin_income_group) %>%
  mutate(origin_total = sum(total_refugees)) %>%
  ungroup() %>%
  mutate(origin_total_pct = 100 * origin_total / sum(total_refugees)) %>%
  arrange(desc(asylum_income_group))

# write .csv
write.csv(ref_final, "ref_final.csv", row.names = F)

# let's form a df for a stacked line area chart in flourish
years <- as.character(1970:2023)
incomeGroups <- c("Low income", "Lower middle income",
                  "Upper middle income",
                  "High income")

ref_area_data <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/Refugee Stacked Area Data/API_SM.POP.RHCR.EO_DS2_en_csv_v2_6571.csv",
                          skip = 3) %>% 
  select(`Country Name`, all_of(years)) %>% 
  pivot_longer(cols = all_of(years),
               names_to = "Year",
               values_to = "Value") %>% 
  filter(`Country Name` %in% incomeGroups) %>% 
  pivot_wider(names_from = `Country Name`,
              values_from = Value)

# write.csv
write.csv(ref_area_data, "ref_area_df.csv", row.names = F)
