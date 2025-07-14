### Remittances 6/11/25

# libraries
library(`readr`)
library(dplyr)
library(tidyverse)
library(ggplot2)

# load in data
remit_gdp <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Remittances Blog Post/Remittance % GDP/API_BX.TRF.PWKR.DT.GD.ZS_DS2_en_csv_v2_6708.csv", skip =3)
remit_cost <- read.csv("C:/Users/wb647336/OneDrive - WBG/Documents/Remittances Blog Post/Remittance Transaction Cost %/API_SI.RMT.COST.IB.ZS_DS2_en_csv_v2_7573.csv", skip = 3) 
remit_class <- read.csv("C:/Users/wb647336/OneDrive - WBG/Documents/Remittances Blog Post/Remittance Transaction Cost %/Metadata_Country_API_SI.RMT.COST.IB.ZS_DS2_en_csv_v2_7573.csv")

### remittance gdp stacked bar chart
# clean remit_gdp with percent greater than 10
remit_gdp_clean <- remit_gdp %>%
  left_join(remit_class, by = c("Country Code" = "Country.Code")) %>%
  select(`Country Name`, `Country Code`, `2023`, `Region`, `IncomeGroup`) %>%
  filter(`2023` > 10) %>%
  na.omit() %>%
  filter(if_all(everything(), ~ str_trim(as.character(.)) != "")) %>% 
  mutate(Color = case_when(
    IncomeGroup == "High income" ~ paste0(`Country Name`, ": #1B5B6E"),
    IncomeGroup == "Upper middle income" ~ paste0(`Country Name`, ": #7BB3C0"),
    IncomeGroup == "Lower middle income" ~ paste0(`Country Name`, ": #CF6682"),
    IncomeGroup == "Low income" ~ paste0(`Country Name`, ": #A22C2D"),
    TRUE ~ paste0(`Country Name`, ": Unknown"))) %>%
  arrange(desc(`2023`))

# write .csv
write.csv(remit_gdp_clean, "remit_gdp_clean.csv", row.names = F)

### remittance gdp bubble chart
# remit_gdp w/all countries 
remit_gdp_clean_all <- remit_gdp %>%
  left_join(remit_class, by = c("Country Code" = "Country.Code")) %>%
  select(`Country Name`, `Country Code`, `2023`, `Region`, `IncomeGroup`) %>%
  na.omit() %>%
  filter(if_all(everything(), ~ str_trim(as.character(.)) != "")) %>% 
  mutate(Color = case_when(
    IncomeGroup == "High income" ~ paste0(`Country Name`, ": #08306B"),
    IncomeGroup == "Upper middle income" ~ paste0(`Country Name`, ": #2171B5"),
    IncomeGroup == "Lower middle income" ~ paste0(`Country Name`, ": #6BAED6"),
    IncomeGroup == "Low income" ~ paste0(`Country Name`, ": #DEEBF7"),
    TRUE ~ paste0(`Country Name`, ": Unknown"))) %>%
  arrange(desc(`2023`))

# write.csv
write.csv(remit_gdp_clean_all, "remit_gdp_clean_all.csv", row.names = F)

# form remittance points layer df for flourish
remit_points <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Remittances Blog Post/remit_points.csv") %>% 
  left_join(remit_gdp_clean_all, by = c("Name" = "Country Name")) %>%
  select(Region.x, Name, Longitude, Latitude, `Population (2019)`, IncomeGroup.x, Color.x, `2023.x`) %>% 
  na.omit()

# which are top 5 countries w/highest gdp contribution by remittance flows?
remit_points %>% 
  filter(`2023.x` > 25) %>% 
  arrange(desc(`2023.x`)) 

# write .csv
write.csv(remit_points, "remit_points.csv", row.names = F)

### remittance cost slope chart (as a %)
years <- paste0("X", 2016:2023)

# merge regional df w/classifications and identify changes since 2016
remit_cost_clean <- remit_cost %>% 
  left_join(remit_class, by = "Country.Code") %>% 
  select(Country.Name, Country.Code, years, IncomeGroup, Region) %>% 
  na.omit() %>% 
  mutate(diff = X2023 - X2016,
         change = case_when(diff > 1 ~ "Increase",
                            diff < -1 & X2023 <= 3.5 ~ "Decrease, Target Met",
                            diff < -1 & X2023 > 3.5 ~ "Decrease, Target Not Met",
                            TRUE ~ "No Change")) %>% 
  arrange(desc(change))

# how many countries still have costs greater than 7%?
remit_cost_clean %>% 
  filter(X2023 >= 7) %>% 
  count(Country.Name) %>% 
  arrange(desc(Country.Name)) 

# write .csv
write.csv(remit_cost_clean, "remit_cost_clean.csv", row.names = F)

