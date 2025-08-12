### Rural Blog Post Day 7/6/2025

# load libraries
library(`readr`)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)

# set wd
setwd("C:/Users/wb647336/OneDrive - WBG/Documents/Rural Blog Post")

### let's look at access to water: urban vs. rural
# read in rural and urban dfs
years <- as.character(2000:2022)
rural_water <- read_csv("C:/Users/wb647336/Downloads/API_SH.H2O.SMDW.RU.ZS_DS2_en_csv_v2_31137/API_SH.H2O.SMDW.RU.ZS_DS2_en_csv_v2_31137.csv",
                        skip = 3) %>%
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit() %>% 
  mutate(location = "Rural")

urban_water <- read_csv("C:/Users/wb647336/Downloads/API_SH.H2O.SMDW.UR.ZS_DS2_en_csv_v2_31139/API_SH.H2O.SMDW.UR.ZS_DS2_en_csv_v2_31139.csv",
                         skip = 3) %>% 
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit() %>% 
  mutate(location = "Urban")

# form clean water df
water_clean <- bind_rows(urban_water, rural_water) %>% 
  group_by(`Country Name`) %>% 
  filter(n_distinct(location) == 2) %>% 
  pivot_longer(cols = years,
               names_to = "Year",
               values_to = "value") %>% 
  pivot_wider(names_from = location,
            values_from = value) %>% 
  mutate(diff = Urban - Rural) %>% 
  pivot_longer(cols = c(Urban, Rural),
               names_to = "location",
               values_to = "value")

### let's look at water access at the world, regional, and income group level
# world water 
years <- as.character(2000:2022)
rural_water_world <- water_clean %>% 
  filter(`Country Name` == "World") %>% 
  pivot_wider(names_from = "location",
              values_from = "value")

# region water, flourish line chart form
rural_water_region <- water_clean %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  select(-`Country Code`) %>% 
  pivot_wider(names_from = `location`,
              values_from = "value") 

# let's look at the diff for ssf - have urban areas improved access more than rural since 2000?
diffdf <- water_clean %>% 
  filter(`Country Code` == 'SSF', 
         location == "Urban") 

# plot
diffdf %>% 
  ggplot(aes(x = as.numeric(Year), y = diff))+
  geom_line()

# income water, flourish line chart form
rural_water_income <- water_clean %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(income_group) %>% 
  select(-`Country Code`) %>% 
  pivot_wider(names_from = `location`,
              values_from = "value")

# let's look at the relationship b/t gdp/cap and access to water
# read in GDP/cap df: 2015, constant
gdp <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/SDG 11/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_80947.csv",
                skip = 3) %>% 
  pivot_longer(cols = `1960`: `2024`,
               names_to = "Year",
               values_to = "gdp_per_cap") %>% 
  mutate(Year = as.character(Year))

# form water access, gdp df
water_gdp <- water_clean %>% 
  inner_join(gdp, by  = c("Country Name", "Year")) %>% 
  select(`Country Name`, diff, Year, location, value, gdp_per_cap) %>% 
  filter(Year == "2022")

# wealthier countries have a smaller difference in urban-rural water access
water_gdp %>% 
  ggplot(aes(x = log(gdp_per_cap), y = diff))+
  geom_point()+
  geom_smooth(method = "lm")

# form fuel gdp .csv
write.csv(water_gdp, "water_gdp.csv", row.names = F)

# form the .csvs for flourish
write.csv(rural_water_world, "rural_water_world.csv", row.names = F)
write.csv(rural_water_region, "rural_water_region.csv", row.names = F)
write.csv(rural_water_income, "rural_water_income.csv", row.names = F)

### let's look at population trends: urban vs. rural
# load in urban and rural pop dfs
Years <- as.character(1960:2024)
pop_prop <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Rural Blog Post/API_SP.RUR.TOTL.ZS_DS2_en_csv_v2_23429/API_SP.RUR.TOTL.ZS_DS2_en_csv_v2_23429.csv",
         skip = 3)

# world pop df - suitable for flourish area chart
world_pop_prop_df <- pop_prop %>% 
  pivot_longer(cols = Years,
               names_to = "Year",
               values_to = "Value") %>% 
  select(`Country Name`, `Country Code`, Year, Value) %>% 
  mutate("urban value" = 100 - Value) %>% 
  filter(`Country Name` == "World") 

# income pop df - suitable for flourish area chart
income_pop_prop_df <- pop_prop %>% 
  pivot_longer(cols = Years,
               names_to = "Year",
               values_to = "Value") %>% 
  select(`Country Name`, `Country Code`, Year, Value) %>% 
  mutate("urban Value" = 100 - Value) %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                   "Upper middle income", "High income"))) %>% 
  arrange(income_group)

# region pop df - suitable for flourish area chart
region_pop_prop_df <- pop_prop %>% 
  pivot_longer(cols = Years,
               names_to = "Year",
               values_to = "Value") %>% 
  select(`Country Name`, `Country Code`, Year, Value) %>% 
  mutate("urban value" = 100 - Value) %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  group_by(`Country Name`) %>% 
  arrange(Value, `Country Name`)  


# form the .csvs
write.csv(world_pop_prop_df, "world_pop_df.csv", row.names = F)
write.csv(income_pop_prop_df, "income_pop_df.csv", row.names = F)
write.csv(region_pop_prop_df, "region_pop_df.csv", row.names = F)

### now let's do the same for electricity: urban vs. rural
# read in elect dfs
years <- as.character(2000:2023)
elect_rural <- read_csv("C:/Users/wb647336/Downloads/API_EG.ELC.ACCS.RU.ZS_DS2_en_csv_v2_30467/API_EG.ELC.ACCS.RU.ZS_DS2_en_csv_v2_30467.csv",
                  skip = 3) %>% 
  select(years, `Country Name`, `Country Code`) %>%
  mutate(location = "Rural")

elect_urban <- read_csv("C:/Users/wb647336/Downloads/API_EG.ELC.ACCS.UR.ZS_DS2_en_csv_v2_30469/API_EG.ELC.ACCS.UR.ZS_DS2_en_csv_v2_30469.csv",
                        skip = 3) %>% 
  select(years, `Country Code`, `Country Name`) %>% 
  mutate(location = "Urban")

# read in rural and urban pop dfs
rural_pop <- read_csv("C:/Users/wb647336/Downloads/API_SP.RUR.TOTL_DS2_en_csv_v2_23587/API_SP.RUR.TOTL_DS2_en_csv_v2_23587.csv",
                      skip = 3) %>% 
  select(years, `Country Name`, `Country Code`) %>% 
  mutate(location = "rural")

urban_pop <- read_csv("C:/Users/wb647336/Downloads/API_SP.URB.TOTL_DS2_en_csv_v2_23181/API_SP.URB.TOTL_DS2_en_csv_v2_23181.csv",
                      skip = 3) %>% 
  select(years, `Country Name`, `Country Code`) %>% 
  mutate(location = "urban")

# merge both urban and rural pop dfs together
rural_urban_pop <- bind_rows(rural_pop, urban_pop) %>% 
  pivot_longer(cols = years,
               names_to = "Year",
               values_to = "value") %>% 
  pivot_wider(names_from = location,
              values_from = value) %>% 
  select(-`Country Code`) 
  
# form main elect df
elect <- bind_rows(elect_rural, elect_urban) %>% 
  group_by(`Country Name`) %>% 
  filter(n_distinct(location) == 2) %>% 
  pivot_longer(cols = years,
               names_to = "Year",
               values_to = "value") %>% 
  na.omit() %>% 
  mutate(pct_noelect = 100-value)

# world elect
years <- as.character(1960:2024)
elect_world <- elect %>% 
  filter(`Country Name` == "World") %>% 
  select(`Country Name`, Year, location, value, pct_noelect)

# world elect plot
elect_world %>%
  ggplot(aes(x = as.factor(Year), y = value, group = location, color = location)) + 
  geom_line()

# region elect
elect_region <- elect %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  select(`Country Name`, Year, value, pct_noelect, location)

# region elect plot
elect_region %>%
  ggplot(aes(x = as.factor(Year), y = value, color = location, group = location)) +
  geom_line() +
  facet_wrap(~ `Country Name`)

# make regional elect df for flourish - grid by urban, rural
elect_region_df <- elect_region %>% 
  select(-value) %>% 
  pivot_wider(names_from = location,
              values_from = pct_noelect) %>% 
  inner_join(rural_urban_pop, by = c("Country Name", "Year")) %>% 
  mutate(urb_noelect = Urban/100*urban,
         rur_noelect = Rural/100*rural) %>% 
  pivot_longer(cols = c(urb_noelect, rur_noelect),
               names_to = "location",
              values_to = "value") %>% 
  select(Year, `Country Name`, location, value) %>% 
  pivot_wider(values_from = value,
              names_from = `Country Name`) %>% 
  mutate(location = case_when(location == "urb_noelect" ~ "Urban",
    location == "rur_noelect" ~ "Rural",
    TRUE ~ location))

# regional elect df for flourish - grid by region
elect_region_df2 <- elect_region %>% 
  pivot_wider(names_from = location,
              values_from = value)

# make world elect df for flourish
# world urban and rural pop df
world_pop <- rural_urban_pop %>% 
  filter(`Country Name` == "World") 

# world electricity df for flourish - count of ppl w/o electricity
elect_world_df <- elect_world %>% 
  select(-value) %>% 
  pivot_wider(names_from = location,
              values_from = pct_noelect) %>% 
  inner_join(world_pop, by = c("Country Name", "Year")) %>% 
  mutate(urb_noelect = Urban/100*urban,
         rur_noelect = Rural/100*rural)

# write.csv
write.csv(elect_region_df, "elect_region_df.csv", row.names = F)
write.csv(elect_region_df2, "elect_region_df2.csv", row.names = F)
write.csv(elect_world_df, "elect_world_df.csv", row.names = F)

# income elect
elect_income <- elect %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(income_group) %>% 
  select(`Country Name`, `Country Code`, Year, value, location)

# income elect plot
elect_income %>% 
  ggplot(aes(x = as.factor(Year), y = value, 
             color = location, group = location))+
  geom_line()+
  facet_wrap(~`Country Name`)

# let's look at the relationship b/t gdp and access to electricity
elect_gdp <- elect %>% 
  inner_join(gdp, by  = c("Country Name", "Year")) %>% 
  select(`Country Name`, Year, location, value, gdp_per_cap)  %>% 
  filter(Year == "2023")

# gdp vs. electricity access - wealthier countries have higher access in both rural and urban areas
elect_gdp %>% 
  ggplot(aes(x = log(gdp_per_cap), y = value))+
  geom_point()+
  facet_grid(~location)

# calculations
# which countries have the lowest rural electricity access? 
elect %>%
  filter(location == "Rural") %>%
  summarise(min_value = min(value, na.rm = TRUE)) %>% 
  arrange(min_value)

# what was the highest electricity access recorded for SSF? 
elect_region %>% 
  filter(`Country Name` == "Sub-Saharan Africa", location == "Rural") %>% 
  summarise(max_value = max(value, na.rm = T)) %>% 
  arrange(desc(max_value))

# form elect gdp .csv
write.csv(elect_gdp, "elect_gdp.csv", row.names = F)

### let's now look at access to clean fuel/technologies for cooking %: urban vs. rural
# read in fuel dfs
years <- as.character(2000:2022)
fuel_rural <- read_csv("C:/Users/wb647336/Downloads/API_EG.CFT.ACCS.RU.ZS_DS2_en_csv_v2_44096/API_EG.CFT.ACCS.RU.ZS_DS2_en_csv_v2_44096.csv",
                       skip = 3) %>% 
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit() %>% 
  mutate(location = "Rural")

fuel_urban <- read_csv("C:/Users/wb647336/Downloads/API_EG.ELC.ACCS.UR.ZS_DS2_en_csv_v2_30469/API_EG.ELC.ACCS.UR.ZS_DS2_en_csv_v2_30469.csv",
                       skip = 3) %>% 
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit() %>% 
  mutate(location = "Urban")

# form main fuel df
fuel <- bind_rows(fuel_rural, fuel_urban) %>% 
  group_by(`Country Name`) %>% 
  filter(n_distinct(location) == 2) %>% 
  pivot_longer(cols = years,
               names_to = "Year",
               values_to = "value") %>% 
  pivot_wider(names_from = location,
              values_from = value) %>% 
  mutate(diff = Urban - Rural) %>% 
  pivot_longer(cols = c(Urban, Rural),
               names_to = "location",
               values_to = "value") %>% 
  mutate(Year = as.numeric(Year))

# world fuel
fuel_world <- fuel %>% 
  filter(`Country Name` == "World") 

# world fuel plot
fuel_world %>% 
  ggplot(aes(x = Year, y = value, group = location, 
             color = location))+
  geom_line()

# world fuel df for flourish
fuel_world_df <- fuel_world %>% 
  pivot_wider(names_from = location,
              values_from = value)
# write.csv
write.csv(fuel_world_df, "fuel_world_df.csv", row.names = F)

# region fuel
fuel_region <- fuel %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  select(`Country Name`, `Country Code`, Year, value, location) 

# region fuel plot
fuel_region %>% 
  ggplot(aes(x = Year, y = value, 
             color = location, group = location))+
  geom_line() +
  facet_wrap(~`Country Name`)

# region fuel df for flourish
fuel_region_df <- fuel_region %>% 
  pivot_wider(names_from = location,
              values_from = value)
# write.csv
write.csv(fuel_region_df, "fuel_region_df.csv", row.names = F)

# income fuel 
fuel_income <- fuel %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  select(`Country Name`, `Country Code`, Year, value, location, income_group) %>% 
  arrange(income_group)

# income fuel plot
fuel_income %>% 
  ggplot(aes(x = Year, y = value, 
             color = location, group = location))+
  geom_line() +
  facet_wrap(~`Country Name`)

# income fuel df for flourish
fuel_income_df <- fuel_income %>% 
  pivot_wider(names_from = location,
              values_from = value)

# write.csv
write.csv(fuel_income_df, "fuel_income_df.csv", row.names = F)

# let's look at the relationship b/t gdp and access to fuel and cooking tech
# first, read in world pop figures so we can size dots by population
years <- as.character(2000:2023)
pop <- read_csv("C:/Users/wb647336/Downloads/API_SP.POP.TOTL_DS2_en_csv_v2_38144/API_SP.POP.TOTL_DS2_en_csv_v2_38144.csv",
                skip = 3) %>% 
  select(years, `Country Name`, `Country Code`) %>% 
  pivot_longer(cols = years,
               names_to = "Year",
               values_to = "pop")

# then read in metadata
meta <- read_csv("C:/Users/wb647336/Downloads/API_EG.CFT.ACCS.RU.ZS_DS2_en_csv_v2_44096/Metadata_Country_API_EG.CFT.ACCS.RU.ZS_DS2_en_csv_v2_44096.csv") %>% 
  filter(!is.na(IncomeGroup))

# form fuel gdp df, use gdp df from earlier
fuel_gdp <- fuel %>% 
  mutate(Year = as.character(Year)) %>% 
  inner_join(meta, by = "Country Code") %>%
  inner_join(gdp, by = c("Country Name", "Year")) %>% 
  inner_join(pop, by = c("Country Name", "Year")) %>% 
  select(`Country Name`, Year, Region, IncomeGroup, 
         gdp_per_cap, location, diff, pop, value) 

# inc in gdp, dec in inequality of access to fuel and cooking tech (urban-rural)
fuel_gdp %>% 
  ggplot(aes(x = log(gdp_per_cap), y = diff))+
  geom_point()+
  geom_smooth(method = "lm")

# gdp vs. fuel tech access: inc in gdp assoc. w/increase in access to clean tech
fuel_gdp %>% 
  ggplot(aes(x = log(gdp_per_cap), y = value))+
  geom_point()+
  facet_grid(~location)

# form fuel gdp .csv for flourish - filter to most recent yr, urban
# because we're only concerned with the diff column
fuel_gdp_df<- fuel_gdp %>% 
  filter(Year == 2022, location == "Urban") %>% 
  mutate(log_gdp = log(gdp_per_cap))

write.csv(fuel_gdp_df, "fuel_gdp_df.csv", row.names = F)

### now let's do the same for completeness of birth: urban vs. rural
# read in birth df, most recent data of birth reg across countries
birth <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Rural Blog Post/fusion_PT_UNICEF_1.0_.PT_CHLD_Y0T4_REG.....R+U..csv") %>% 
  filter(`TIME_PERIOD:Time period` > 2016) 

# form birth .csv for connected dot plot, flourish
write.csv(birth, "birth.csv", row.names = F)

# let's look at birth completeness w/income class using a bee swarm plot
# library
library(ggbeeswarm)

# bee swarm plot - use meta data from above
birth %>% 
  separate(`REF_AREA:Geographic area`, into = c("Country Code", "Country Name"), sep = ":") %>% 
  left_join(meta, by = "Country Code") %>% 
  select("Country Code", "Country Name", "Region", "IncomeGroup", `RESIDENCE:Residence`,
         `OBS_VALUE:Observation Value`, `TIME_PERIOD:Time period`) %>% 
  ggplot(aes(x = `IncomeGroup`, y = `OBS_VALUE:Observation Value`, color = IncomeGroup)) +
  geom_beeswarm() +
  facet_wrap(~`RESIDENCE:Residence`)

# bee swarm plot df for flourish
birth_df <- birth %>% 
  separate(`REF_AREA:Geographic area`, into = c("Country Code", "Country Name"), sep = ":") %>% 
  left_join(meta, by = "Country Code") %>% 
  select("Country Code", "Country Name", "Region", "IncomeGroup", `RESIDENCE:Residence`,
         `OBS_VALUE:Observation Value`, `TIME_PERIOD:Time period`) %>% 
  mutate(Location = sub(".*:\\s*", "", `RESIDENCE:Residence`)) %>% 
  mutate(income_group = factor(IncomeGroup, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(income_group)

# write.csv
write.csv(birth_df, "birth_df.csv", row.names = F)

# let's look at birth reg and gdp data
birth_gdp <- birth %>%
  separate(`REF_AREA:Geographic area`, 
           into = c("Country Code", "Country Name"), sep = ": ") %>%
  rename(Location = `RESIDENCE:Residence`,
         Value = `OBS_VALUE:Observation Value`,
         Year = `TIME_PERIOD:Time period`) %>% 
  mutate(Year = as.character(Year)) %>% 
  inner_join(gdp, by = c("Country Name", "Year")) %>%
  select(`Country Code.x`, `Country Name`, Location,
         Year, Value, gdp_per_cap) %>% 
  arrange(Year) %>% 
  mutate(Location = sub(".*:\\s*", "", Location))

# write.csv connected dot plot flourish
write.csv(birth_gdp, "birth_gdp.csv", row.names = F)

# calculation
# let's look at the birth completion rates among upper middle income economies
birth_upper <- birth %>% 
  separate(`REF_AREA:Geographic area`, into = c("Country Code", "Country Name"), sep = ":") %>% 
  left_join(meta, by = "Country Code") %>% 
  select("Country Code", "Country Name", "Region", "IncomeGroup", `RESIDENCE:Residence`,
         `OBS_VALUE:Observation Value`, `TIME_PERIOD:Time period`) %>% 
  filter(IncomeGroup == "Upper middle income", `RESIDENCE:Residence` == "R: Rural") %>% 
  arrange(`OBS_VALUE:Observation Value`) %>% 
  mutate(count = row_number())

# what is the avg. diff in birth compl. rate in LIC and LMIC economies? 
birth_df %>% 
  select(`OBS_VALUE:Observation Value`, `Country Name`, `TIME_PERIOD:Time period`,
         Location, income_group) %>% 
  pivot_wider(names_from = Location,
              values_from = `OBS_VALUE:Observation Value`) %>% 
  filter(income_group %in% c("Low income", "Lower middle income")) %>% 
  group_by(`Country Name`) %>% 
  mutate(urban_rural_gap = Urban - Rural) %>% 
  group_by(income_group) %>% 
  mutate(mean_value = mean((urban_rural_gap)), na.rm = T) %>% 
  select(mean_value) 

