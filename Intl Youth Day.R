### International Youth Day
# August 12, 2025

# set wd
setwd("C:/Users/wb647336/OneDrive - WBG/Documents/International Youth Day")

## read in dfs
# literacy rate of youth as a proportion of youth total
lit <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/International Youth Day/API_SE.ADT.1524.LT.ZS_DS2_en_csv_v2_22827/API_SE.ADT.1524.LT.ZS_DS2_en_csv_v2_22827.csv",
                skip = 3)

# let's make a clean lit df w recent data for a points map viz
# first let's read in the incomegroup and points dfs
meta <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/International Youth Day/API_SE.ADT.1524.LT.ZS_DS2_en_csv_v2_22827/Metadata_Country_API_SE.ADT.1524.LT.ZS_DS2_en_csv_v2_22827.csv")
points <- read_csv("C:/Users/wb647336/Downloads/Points.1753210713961.csv")

# now let's clean, retrieving most recent data and merging dfs
years <- as.character(2015:2024)
lit_clean_df <- lit %>% 
  pivot_longer(cols = years,
               values_to = "lit_rate",
                 names_to = "Year") %>% 
  drop_na(lit_rate) %>% 
  group_by(`Country Name`) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year == max(Year)) %>% 
  inner_join(points, by = c("Country Name" = "Name")) %>% 
  inner_join(meta, by = "Country Code") %>% 
  select(`Country Name`, `Country Code`, Year,
         Region, IncomeGroup, Latitude, 
         Longitude, lit_rate) 

# write.csv
write.csv(lit_clean_df, "lit_clean_df.csv", row.names = F)

### let's look at literacy rates at the world, regional, and income group level
# world lit over time -> lit rates increasing worldwide
lit %>% 
  filter(`Country Name` == "World") %>% 
  pivot_longer(cols = years,
               names_to = "year",
               values_to = "lit_rate") %>% 
  ggplot(aes(x = as.numeric(year), y = lit_rate))+
  geom_line()

# regional lit rates over time -> increasing in every region especially South Asia
lit %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', 
                                "ECS", "LCN", "MEA", "NAC")) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value") %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()

# lit region df for flourish
lit_region <-  lit %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', 
                                "ECS", "LCN", "MEA", "NAC")) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value") %>% 
  select(`Country Name`, 
         year, value) %>% 
  pivot_wider(names_from = `Country Name`,
              values_from = value)

# write.csv
write.csv(lit_region, "lit_region.csv", row.names = F)

# income lit rates over time -> inc. in every economy, steep catching up from LMIC, missing HIC
lit %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(income_group) %>% 
  select(-`Country Code`) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value") %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()


### youth unemployment rate
unemploy <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/International Youth Day/API_SL.UEM.1524.ZS_DS2_en_csv_v2_31226/API_SL.UEM.1524.ZS_DS2_en_csv_v2_31226.csv",
                     skip = 3)

# form a clean df
years <- as.character(1991:2024)
unemploy_clean <- unemploy %>% 
  select(`Country Code`, `Country Name`, years) %>%
  na.omit()

## let's look at unemploy rates at the world, regional, and income group level
# world unemploy 
unemploy_world <- unemploy_clean %>% 
  filter(`Country Name` == "World") %>% 
  pivot_longer(cols = years,
               names_to = "year",
               values_to = "value")

# world unemploy over time -> generally inc up until covid (2020) then strong correction downwards
unemploy_world %>% 
  ggplot(aes(x = as.numeric(year), y = value))+
  geom_line()

# region unemploy
unemploy_region <- unemploy_clean %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value") 

# regional unemploy rates over time -> steady seasonality, SSA consistent rates
# EAP gradual increase, MEA has highest now, South Asia, Europe, LAC
unemploy_region %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()

# unemploy region df flourish
unemploy_region_df <- unemploy_region %>% 
  select(-`Country Code`) %>% 
  pivot_wider(names_from = `Country Name`, 
              values_from = "value")

# write.csv
write.csv(unemploy_region_df, "unemploy_region_df.csv", row.names = F)

# income unemploy
unemploy_income <- unemploy_clean %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(income_group) %>% 
  select(-`Country Code`) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value")

# unemployment df flourish
unemploy_income_df <- unemploy_income %>% 
  select(-income_group) %>% 
  pivot_wider(names_from = `Country Name`, 
              values_from = "value")

# write.csv
write.csv(unemploy_income_df, "unemploy_income_df.csv", row.names = F)

# income unemploy rates over time -> UMC highest, increased substantially since '91
unemploy_income %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()

### youth NEET
youth_pop <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Health_Nutrition_and_Population_Statistics/489b582f-ef7b-4e67-beff-fa1f775f5e0e_Data.csv") %>%
  filter(`Series Code`!= "") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "pop") %>%
  mutate(Year = sub(" .*", "", Year),
         Year = as.numeric(Year),
         pop = as.numeric(pop)) %>% 
  select(-`Series Name`) %>% 
  pivot_wider(names_from = `Series Code`,
              values_from = pop) %>% 
  mutate(pop = (SP.POP.2024.FE + SP.POP.2024.MA))

# youth neet rates, wb df
youth_neet <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Gender_Statistics (3)/0c980a75-667a-4841-9b6a-ab842e259b8d_Data.csv") %>% 
  mutate(across(everything(), ~ na_if(.x, ".."))) %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "neet") %>% 
  mutate(Year = sub(" .*", "", Year),
         Year = as.numeric(Year),
         neet = as.numeric(neet)) %>% 
  drop_na(neet) %>% 
  group_by(`Country Name`) %>%
  filter(Year == max(Year)) %>% 
  inner_join(meta, by = "Country Code") %>% 
  inner_join(youth_pop, by = c("Country Name", "Country Code", "Year")) %>% 
  select(`Country Name`, `Country Code`, Region,
         IncomeGroup, Year, neet, pop) %>% 
  mutate(color = case_when(
    Region == "South Asia" ~ paste0(`Country Name`, ": #4ec2c0"),
    Region == "Europe & Central Asia" ~ paste0(`Country Name`, ": #AA0000"),
    Region == "Middle East, North Africa, Afghanistan & Pakistan" ~ paste0(`Country Name`, ": #664ab6"),
    Region == "Sub-Saharan Africa" ~ paste0(`Country Name`, ": #ff9800"),
    Region == "Latin America & Caribbean" ~ paste0(`Country Name`,": #0c7c68"),
    Region == "East Asia & Pacific" ~ paste0(`Country Name`, ": #f3578e"),
    Region == "North America" ~ paste0(`Country Name`, ": #34a7f2")),
    neet_pop = neet/100*pop,
    neet_pop = as.character(neet_pop))

# let's look at gender differences in NEET - read in abs numbers of NEETs (ILO) df
neet_gender <- read_csv("C:/Users/wb647336/Downloads/EIP_2EET_SEX_NB_A-20250729T1641.csv") 

# filter to world
world_neet <- neet_gender %>% 
  filter(ref_area.label == "World") %>% 
  pivot_wider(names_from = sex.label,
              values_from = obs_value) %>% 
  mutate(male_pct = Male/Total * 100,
         female_pct = Female/Total * 100)

# let's now look at gender differences across gdp
# read in GDP/cap df: 2015, constant
gdp <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/SDG 11/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_80947.csv",
                skip = 3) %>% 
  pivot_longer(cols = `1960`: `2024`,
               names_to = "Year",
               values_to = "gdp_per_cap") %>% 
  mutate(Year = as.numeric(Year))

# country gender diff in NEET prevalence vs. gdp df
neet_gender_gdp <- read_csv("C:/Users/wb647336/Downloads/EIP_2EET_SEX_RT_A-20250729T1617.csv") %>% 
  filter(time == 2023, sex.label != "Total")  %>% 
  pivot_wider(names_from = sex.label,
              values_from = obs_value) %>% 
  mutate(diff = Female - Male) %>% 
  inner_join(gdp, by  = c("time" = "Year",
                          "ref_area.label" = "Country Name")) %>% 
  inner_join(meta, by = c("ref_area.label" = "TableName")) %>% 
  inner_join(neet, by  = c("time", "ref_area.label")) %>% 
  filter(sex.label == "Total") %>% 
  select(ref_area.label, time, Region,
         IncomeGroup, gdp_per_cap, 
         Male, Female, diff, obs_value) %>% 
  mutate(log_gdp = log(gdp_per_cap),
         obs_value = obs_value * 1000)

# write.csv
write.csv(youth_neet, "youth_neet.csv", row.names = F)  
write.csv(neet_gender, "neet_gender.csv", row.names = F)
write.csv(neet_gender_gdp, "neet_gender_gdp.csv", row.names = F)  
