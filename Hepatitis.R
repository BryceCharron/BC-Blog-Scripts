### World Hepatitis Day
# 7/28/25

# hep b3 wdi data
hepb3 <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Hepatitis Day Blog Post/API_SH.IMM.HEPB_DS2_en_csv_v2_31146/API_SH.IMM.HEPB_DS2_en_csv_v2_31146.csv",
                  skip = 3)

years <- as.character(2000:2023)
hep_clean <- hepb3 %>% 
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit()
  
### let's look at hep immunizations at the world, regional, and income group level
# world hep 
hep_world <- hep_clean %>% 
  filter(`Country Name` == "World") %>% 
  pivot_longer(cols = years,
               names_to = "year",
               values_to = "value")

# world hep over time -> immunization rates increasing worldwide
hep_world %>% 
  ggplot(aes(x = as.numeric(year), y = value))+
  geom_line()


# region hep
hep_region <- hep_clean %>% 
  filter(`Country Code` %in% c ("EAS", 'SAS', 'SSF', "ECS", "LCN", "MEA", "NAC")) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value")

# regional hep immunizations over time -> increasing in every region especially until 2012 and has plateaued since
hep_region %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()

# income hep
hep_income <- hep_clean %>% 
  filter(`Country Code` %in% c("HIC", "LIC", "LMC", "UMC")) %>% 
  mutate(income_group = factor(`Country Name`, 
                               levels = c("Low income", 
                                          "Lower middle income", 
                                          "Upper middle income",
                                          "High income"))) %>% 
  arrange(income_group) %>% 
  select(-`Country Code`) %>% 
  pivot_longer(cols = years,
               names_to = "year", 
               values_to = "value")

# income hep immunizations over time -> inc. in every economy, steep catching up from LIC and LMIC
hep_income %>% 
  ggplot(aes(x = as.numeric(year), y = value, color = `Country Name`))+
  geom_line()

# let's convert them all to .csvs for flourish
hep_world_df <- hep_world %>% 
  select(-`Country Code`) %>% 
  pivot_wider(names_from = `Country Name`, 
              values_from = "value")

hep_region_df <- hep_region %>% 
  select(-`Country Code`) %>% 
  pivot_wider(names_from = `Country Name`, 
              values_from = "value")

hep_income_df <- hep_income %>% 
  select(-income_group) %>% 
  pivot_wider(names_from = `Country Name`, 
              values_from = "value")

# let's reshape the data for a slope chart format and to be used for gdp plot
# first let's read in the incomegroup df
meta <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/Hepatitis Day Blog Post/API_SH.IMM.HEPB_DS2_en_csv_v2_31146/Metadata_Country_API_SH.IMM.HEPB_DS2_en_csv_v2_31146.csv") %>% 
    filter(!is.na(IncomeGroup))

# read in gdp df, 2015, constant
gdp <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/SDG 11/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_80947.csv",
                  skip = 3) %>% 
  select(`Country Name`,
           `2023`) %>% 
  rename(gdp_2023 = `2023`)
  
# read in world pop figures, select 2023
pop <- read_csv("C:/Users/wb647336/Downloads/API_SP.POP.TOTL_DS2_en_csv_v2_38144/API_SP.POP.TOTL_DS2_en_csv_v2_38144.csv",
                  skip = 3) %>% 
  select(`2023`, `Country Name`) %>% 
  pivot_longer(cols = `2023`,
                 names_to = "Year",
                 values_to = "pop")

# FCS countries list 2023
fcs_countries <- c("Afghanistan", "Burkina Faso", "Cameroon", "Central African Republic",
    "Congo, Democratic Republic of", "Ethiopia", "Iraq", "Mali", "Mozambique",
    "Myanmar", "Niger", "Nigeria", "Somalia", "South Sudan", "Syrian Arab Republic",
    "Ukraine", "Yemen, Rep.", "Burundi", "Chad", "Comoros",
    "Congo, Republic of", "Eritrea", "Guinea-Bissau", "Haiti", "Kosovo",
    "Lebanon", "Libya", "Marshall Islands", "Micronesia, Fed Sts.",
    "Papua New Guinea", "Solomon Islands", "Sudan", "Timor-Leste", "Tuvalu",
    "Venezuela, RB", "West Bank and Gaza", "Zimbabwe")
  
# let's look at a more recent window of time  
years <- as.character(2010:2023)
hep_clean_new <- hepb3 %>% 
  select(`Country Name`, `Country Code`, years) %>% 
  na.omit()
  
# slope chart df  
hep_slope <- hep_clean_new %>%
  mutate(Change = case_when(`2023` - `2010` > 2 ~ "Economies with increasing immunization",
                            `2023` - `2010` < -2 ~ "Economies with decreasing immunization",
                            T ~ "No change")) %>% 
  left_join(meta, by = "Country Code") %>% 
  inner_join(gdp, by  = "Country Name") %>%
  inner_join(pop, by  = "Country Name") %>% 
  select(years, Change, 
         `Country Code`, `Country Name`, 
         IncomeGroup, Region, 
         gdp_2023, pop) %>% 
  mutate(IncomeGroup = factor(IncomeGroup, 
                              levels = c("Low income", 
                                         "Lower middle income", 
                                         "Upper middle income", 
                                         "High income"))) %>%
  mutate(fcs_status = if_else(`Country Name` %in% fcs_countries, 
                              "FCS", 
                              "Non-FCS"),
         log_gdp = log(gdp_2023)) %>%
  arrange(IncomeGroup) %>% 
  na.omit()

# are fcs areas more commonly areas of "decreasing immunization"?  
hep_slope %>% 
  mutate(greatest_drop = `2023` - `2019`) %>% 
  select(`Country Name`, greatest_drop) %>% 
  arrange((greatest_drop))
  count(fcs_status, Change)
  
# let's look at fcs areas - make a line chart in flourish
years <- as.character(2000:2023)
hep_fcs <- hep_clean %>% 
  filter(`Country Name` == "Fragile and conflict affected situations") %>% 
  pivot_longer(cols = years,
               values_to = "fcs_rate",
               names_to = "year")

# write .csvs for flourish
write.csv(hep_fcs, "hep_fcs.csv", row.names = F)  
write.csv(hep_clean_dec, "hep_clean_dec.csv", row.names = F)
write.csv(hep_world_df, "hep_world.csv", row.names = F)
write.csv(hep_income_df, "hep_income.csv", row.names = F)
write.csv(hep_region_df, "hep_region.csv", row.names = F)

# let's look at count of ppl w/chronic hep b
hep_count <- read_csv("C:/Users/wb647336/Downloads/hep_data.csv") %>% 
  filter(Indicator == "People living with chronic hepatitis B (HBV) (number)") %>% 
  select(Location, Period, SpatialDimValueCode, FactValueNumeric)

# read in points df from flourish
hep_points <- read_csv("C:/Users/wb647336/Downloads/Points.1753210713961.csv")

# form clean df for flourish map
hep_count_clean <- hep_count %>% 
  inner_join(hep_points, by  = c("Location" = "Name")) %>% 
  inner_join(gdp, by  = c("Location" = "Country Name")) %>%
  inner_join(meta, by  = c("SpatialDimValueCode"  = "Country Code")) %>% 
  select(Location, Period, SpatialDimValueCode, Longitude,
         Latitude, Region, 
         gdp_2023, IncomeGroup, FactValueNumeric) %>% 
  mutate(log_gdp = log(gdp_2023),
         IncomeGroup = factor(IncomeGroup, 
                              levels = c("Low income", 
                                         "Lower middle income",
                                         "Upper middle income", 
                                         "High income"))) %>% 
  arrange(IncomeGroup)

# which countries have the most people living with chronic hep? 
hep_count_clean %>%
  filter(FactValueNumeric > 2e6) %>%
  select(Location, FactValueNumeric,IncomeGroup) %>% 
  arrange(desc(FactValueNumeric))

# write.csv
write.csv(hep_count_clean, "hep_count_clean.csv", row.names = F)
  