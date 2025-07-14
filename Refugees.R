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
ref_class <- read_excel("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/CLASS.xlsx")

# clean UNHCR df, calculate percentages for total refugee flows
ref_total_clean <- ref_total %>%
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
write.csv(ref_total_clean, "ref_total_clean.csv", row.names = F)

# create UNRWA flow df
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

# combine dfs
ref_all <- bind_rows(ref_total_clean, ref_unrwa)

dest_totals <- ref_all %>%
  group_by(asylum_income_group) %>%
  summarise(dest_total = sum(total_refugees), .groups = "drop") %>%
  mutate(dest_total_pct = 100 * dest_total / sum(dest_total))

ref_final <- ref_all %>%
  left_join(dest_totals, by = "asylum_income_group") %>%
  group_by(origin_income_group) %>%
  mutate(origin_total = sum(total_refugees)) %>%
  ungroup() %>%
  mutate(origin_total_pct = 100 * origin_total / sum(total_refugees)) %>%
  arrange(desc(asylum_income_group))

# write .csv
write.csv(ref_final, "ref_final.csv", row.names = F)


# sankey plot per code from Sinae
stratum_fill <- c(rev(c("HIC","UMC","LMC","LIC")),
  rev(c("HIC","UMC","LMC","LIC")))

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source("C:/Users/wb647336/OneDrive - WBG/Documents/Refugee Blog Post/styles.R")
style <- style_atlas()

ref_final %>% 
  ggplot(aes(axis1 = origin_income_group, axis2 = asylum_income_group, y = total_refugees)) +
  scale_x_discrete(limits = c("origin_income_group", "asylum_income_group"), labels=c("Origin", "Destination"),
                   expand = c(0.17, 0.17), position = "top") +
  geom_alluvium(aes(fill = origin_income_group), width=1/10) +
  scale_y_continuous(expand=c(0,1))+
  scale_fill_manual(values=style$colors$incomes.alt) +
  geom_stratum(width=1/10, fill = style$colors$incomes.alt[stratum_fill], color=NA) +
  geom_text(aes(label=str_wrap_lines(style$labels$income.lb[origin_income_group], 2)),
    stat="stratum",
    hjust = 1,
    vjust=0.8,
    nudge_x = -0.07,
    family = style$theme()$text$family,
    size = style$gg_text_size*0.8,
    lineheight = 0.75) +
  geom_text(aes(label=str_wrap_lines(style$labels$income.lb[asylum_income_group], 2)),
    stat="stratum",
    hjust = 0,
    nudge_x = 0.07,
    family = style$theme()$text$family,
    size = style$gg_text_size*0.8,
    lineheight = 0.75) +
  style$theme() +
  theme(axis.text.y=element_blank(),
        panel.grid = element_blank())

# stacked area chart
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


write.csv(ref_area_data, "ref_area_df.csv", row.names = F)
