### Child Marriage Story

## treemap gender (2024) - data from childmarriagedata.org, mar_pop
married_women <- (.19*299950640)
married_men <- (.03*318305258)
unmarried_women <- 299950640 - married_women
unmarried_men <- 318305258 - married_men

## treemap of absolute number
# read in marriage prevalence among youth df from unicef
years <- as.character(2016:2023)
mar_pct <- read_excel("C:/Users/wb647336/Downloads/GLOBAL_DATAFLOW_2016-2023.xlsx",
                  skip = 1) %>% 
  slice(-1) %>% 
  pivot_longer(cols = years,
               names_to = "year",
               values_to = "pct") %>% 
  mutate(pct = as.numeric(pct),
         `Geographic area` = str_replace(`Geographic area`, "Türkiye", "Turkiye"))

# filter to female
mar_pct_female <- mar_pct %>%
  filter(Indicator == "Percentage of women (aged 20-24 years) married or in union before age 18")

# read in pop df from wb health nutrition and pop stats
mar_pop <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Health_Nutrition_and_Population_Statistics/489b582f-ef7b-4e67-beff-fa1f775f5e0e_Data.csv") %>%
  filter(`Series Code`!= "") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
    names_to = "Year",
    values_to = "pop") %>%
  mutate(Year = sub(" .*", "", Year),
         pop = as.numeric(pop))

# filter to female
mar_pop_female <- mar_pop %>% 
  filter(`Series Name` == "Population ages 20-24, female") 

### form clean marriage df
# first read in meta df for new region and income group classifications 
meta <- read_excel("C:/Users/wb647336/Downloads/P_Data_Extract_From_Health_Nutrition_and_Population_Statistics_Metadata.xlsx")

# now merge marriage dfs, adding income and region groups
mar_clean_female <- mar_pct_female %>%
  inner_join(mar_pop_female, by = c("Geographic area" = "Country Name",
                             "year" = "Year")) %>% 
  na.omit() %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  mutate(total_mar = pop*pct/100,
         color = case_when(
           Region == "South Asia" ~ paste0(`Geographic area`, ": #4ec2c0"),
           Region == "Europe & Central Asia" ~ paste0(`Geographic area`, ": #AA0000"),
           Region == "Middle East, North Africa, Afghanistan & Pakistan" ~ paste0(`Geographic area`, ": #664ab6"),
           Region == "Sub-Saharan Africa" ~ paste0(`Geographic area`, ": #ff9800"),
           Region == "Latin America & Caribbean" ~ paste0(`Geographic area`,": #0c7c68"),
           Region == "East Asia & Pacific" ~ paste0(`Geographic area`, ": #f3578e"))) %>% 
    select(`Geographic area`, year, `Country Code`, Region, `Income Group`, pop, 
         pct, total_mar, color) %>% 
  arrange(desc(total_mar))

mar_clean_female %>% 
  arrange(desc(pct))
    
# write.csv for flourish
write.csv(mar_clean_female, "mar_clean_female.csv", row.names = F)

# scatterplot of gdp and CM
# read in GDP/cap df: 2015, constant
gdp <- read_csv("C:/Users/wb647336/OneDrive - WBG/Documents/SDG 11/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_80947.csv",
                skip = 3) %>% 
  pivot_longer(cols = `1960`: `2024`,
               names_to = "Year",
               values_to = "gdp_per_cap") 

# make clean marriage df with gdp
mar_clean_gdp <- mar_clean_female %>% 
  inner_join(gdp, by = c("year" = "Year",
                         "Geographic area" = "Country Name")) %>% 
  mutate(IncomeGroup = factor(`Income Group`, 
                              levels = c("Low income", "Lower middle income", 
                                         "Upper middle income", "High income"))) %>% 
  arrange(IncomeGroup)

# write.csv for flourish
write.csv(mar_clean_gdp, "mar_clean_gdp.csv", row.names = F)

# scatterplot literacy & CM, female labor force participation
# read in female lit and labor force participation dfs from wdi
lit_fem <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_World_Development_Indicators (1)/1a3a9dde-e493-441c-8b9f-21927cd30ed8_Data.csv") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "lit_rate")  %>%
  mutate(Year = sub(" .*", "", Year),
         lit_rate = as.numeric(lit_rate)) %>% 
  select(`Country Name`, Year, lit_rate)

years <- as.character(2016:2023)
labor_fem <- read_csv("C:/Users/wb647336/Downloads/API_SL.TLF.CACT.FE.ZS_DS2_en_csv_v2_20497/API_SL.TLF.CACT.FE.ZS_DS2_en_csv_v2_20497.csv", skip = 3) %>% 
  pivot_longer(cols = years,
               names_to = "year",
               values_to = "labor_force") %>% 
  select(`Country Name`, year, labor_force)

# make clean marriage df with lit rates
mar_clean_lit <- mar_clean_female %>% 
  inner_join(lit_fem, by = c("year" = "Year",
                             "Geographic area" = "Country Name")) %>%
  na.omit()

# write.csv for flourish
write.csv(mar_clean_lit, "mar_clean_lit.csv", row.names = F)

# make clean marriage df with labor participation rates
mar_clean_labor <- mar_clean_female %>% 
  inner_join(labor_fem, by = c("year" = "year",
                               "Geographic area" = "Country Name")) %>% 
  na.omit() %>% 
  mutate(IncomeGroup = factor(`Income Group`, 
                               levels = c("Low income", "Lower middle income", 
                                          "Upper middle income", "High income"))) %>% 
  arrange(IncomeGroup)

# write.csv for flourish
write.csv(mar_clean_labor, "mar_clean_labor.csv", row.names = F)

# treemap of gender diff
# pivot mar_pct
mar_pct_wide <- mar_pct %>% 
  filter(Indicator != "") %>% 
  pivot_wider(names_from = Indicator,
              values_from = pct) %>% 
  rename(`pct_women` = `Percentage of women (aged 20-24 years) married or in union before age 18`,
    `pct_men` = `Percentage of men (aged 20-24 years) married or in union before age 18`)

# pivot mar_pop
mar_pop_wide <- mar_pop %>%
  select(`Country Name`, `Country Code`, `Series Code`, pop, Year) %>%
  pivot_wider(names_from = `Series Code`,
    values_from = pop)

# now merge marriage dfs, add incomegroup
mar_pop_region <- mar_pop_wide %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  filter(Year == "2024") %>% 
  group_by(Region) %>% 
  summarise(total_region_fem_pop = sum(SP.POP.2024.FE),
            total_region_men_pop = sum(SP.POP.2024.MA))

# clean treemap of gender diff df
mar_clean_region <- mar_pct_wide %>%
  inner_join(mar_pop_wide, by = c("Geographic area" = "Country Name",
                             "year" = "Year")) %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  mutate(women_mar = pct_women/100*SP.POP.2024.FE,
         men_mar = pct_men/100 *SP.POP.2024.MA) %>% 
  select(`Geographic area`, year, `Country Code`, Region, `Income Group`, 
         SP.POP.2024.FE, SP.POP.2024.MA, pct_women, pct_men, 
         women_mar, men_mar) %>% 
  na.omit() %>%
  group_by(Region) %>% 
  summarise(region_fem_mar = sum(women_mar),
            region_men_mar = sum(men_mar),
            region_fem_pop = sum(SP.POP.2024.FE),
            region_men_pop = sum(SP.POP.2024.MA)) %>% 
  mutate(region_fem_pct = 100*region_fem_mar/region_fem_pop,
         region_men_pct = 100*region_men_mar/region_men_pop) %>% 
  inner_join(mar_pop_region, by = "Region") %>% 
  mutate(fem_pct_check = 100*region_fem_pop/total_region_fem_pop,
         men_pct_check = 100*region_men_pop/total_region_men_pop)

# write.csv for flourish
write.csv(mar_clean_region, "mar_clean_region.csv", row.names = F)

## let's use the WDI data to make a slope chart
# make a 1995-2005 df
mar_pct_1995 <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Health_Nutrition_and_Population_Statistics (1)/57a24891-b39a-436d-b7a3-dfb4c00b35c6_Data.csv") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "value") %>% 
  mutate(Year = sub(" .*", "", Year),
         Year = as.numeric(Year),
         value = as.numeric(value)) %>% 
  drop_na(value) %>%
  filter(Year < 2006) %>% 
  group_by(`Country Name`) %>% 
  filter(Year == max(Year)) %>% 
  mutate(era = case_when(
    Year < 2006 ~ "era_1995_2005"))%>% 
  pivot_wider(names_from = era, 
              values_from = value)

# make a 2015-2023 df
mar_pct_2015 <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Health_Nutrition_and_Population_Statistics (1)/57a24891-b39a-436d-b7a3-dfb4c00b35c6_Data.csv") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "value") %>% 
  mutate(Year = sub(" .*", "", Year),
         Year = as.numeric(Year),
         value = as.numeric(value)) %>% 
  drop_na(value) %>%
  filter(Year > 2014) %>% 
  group_by(`Country Name`) %>% 
  filter(Year == max(Year)) %>% 
  mutate(era = case_when(
    Year > 2014 ~ "era_2015_2023"))%>% 
  pivot_wider(names_from = era, 
              values_from = value)

# form slope chart df
mar_pct_slope <- mar_pct_1995 %>% 
  inner_join(mar_pct_2015, by = c("Country Name", "Country Code")) %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  select(`Country Name`, `Income Group`, Region,
         era_1995_2005, era_2015_2023) %>% 
  mutate(Change = case_when(era_2015_2023 - era_1995_2005 > 1 ~ "Increasing Child Marriage",
                            era_2015_2023 - era_1995_2005 < -1 ~ "Decreasing Child Marriage",
                           T ~ "No change")) %>% 
  mutate(`Income Group` = factor(`Income Group`, 
                              levels = c("Low income", "Lower middle income", 
                                         "Upper middle income", "High income"))) %>% 
  arrange(`Income Group`)

# write .csv for flourish
write.csv(mar_pct_slope, "mar_pct_slope.csv", row.names = F)
  
## marriage indicator vs. marriage rate scatter plot
marriage_scale <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Gender_Statistics/d61ea855-130c-4ce0-ab3e-979ef170a963_Data.csv") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "mar_scale") %>% 
  mutate(Year = sub(" .*", "", Year))

mar_indicator <- marriage_scale %>% 
  inner_join(mar_pct_female, by = c("Year" = "year",
                             "Country Name" = "Geographic area")) %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  select(`Country Name`, `Country Code`, Region, `Income Group`,
         Year, mar_scale, pct) %>% 
  na.omit() 

# write.csv
write.csv(mar_indicator, "mar_indicator.csv", row.names = F)

## female years of schooling vs. marriage rate scatter plot
fem_school <- read_csv("C:/Users/wb647336/Downloads/P_Data_Extract_From_Gender_Statistics (4)/4bd1674a-1ac9-43e3-b1fe-b441a181e976_Data.csv") %>% 
  pivot_longer(cols = matches("^\\d{4} \\[YR\\d{4}\\]"),
               names_to = "Year",
               values_to = "yrs_school") %>% 
  mutate(Year = sub(" .*", "", Year),
         yrs_school = as.numeric(yrs_school))

fem_sch_mar <- fem_school %>% 
  inner_join(mar_pct_female, by  = c("Country Name" = "Geographic area",
                                     "Year" = "year")) %>% 
  inner_join(meta, by = c("Country Code" = "Code")) %>% 
  select(`Country Name`, `Income Group`, 
         Region, Year, yrs_school, pct) %>% 
  na.omit() %>% 
  mutate(`Income Group` = factor(`Income Group`, 
                                 levels = c("Low income", "Lower middle income", 
                                            "Upper middle income", "High income"))) %>% 
  arrange(`Income Group`)

# write.csv for flourish
write.csv(fem_sch_mar, "fem_sch_mar.csv", row.names = F)
