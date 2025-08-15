### Youth Data Availability
# August, 2025


# set wd
setwd("C:/Users/wb647336/OneDrive - WBG/Documents/Youth Data Avail")

# packages/libraries
install.packages("countrycode")
library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(stringr)

# read in sdg df with indicators to identify youth, young, age indicators
sdg_2 <- read_excel("C:/Users/wb647336/Downloads/Tier Classification of SDG Indicators_ 10 Apr 2025_web.xlsx",
                  sheet = 3,
                  skip = 1) %>% 
  select("Indicator...3") %>% 
  na.omit() %>% 
  rename(indicator = "Indicator...3") %>% 
  mutate(youth = ifelse(str_detect(indicator, 
                                   regex("youth", ignore_case = T)), 1, 0),
    young = ifelse(str_detect(indicator, 
                              regex("young", ignore_case = T)), 1, 0),
    age   = ifelse(str_detect(indicator, 
                              regex("\\bage\\b", ignore_case = TRUE)), 1, 0)) %>% 
  filter(youth == 1 |
         young == 1|
         age == 1)

# let's make this into an excel file that shows our filtering process
write.csv(sdg_2, "sdg_2.csv", row.names = F)

### after excluding non-youth-relevant indicators, let's read in the remaining indicators by series code
# initialize df "df"
df <- NULL

# create vector of series codes, 23*3 + 1 = 70 total series codes
seriescodes <- c("SE_ADT_EDUCTRN", "SH_HIV_INCD", "SE_ADT_ACTS",
                 "SI_POV_DAY1", "SI_POV_EMP1", "SI_POV_NAHC",
                "SP_DYN_ADKL",  "SE_ADT_FUNS", "VC_VAW_MARR",
                "SP_DYN_MRBF18", "SP_DYN_MRBF15", "SH_STA_FGMS",
                "SL_DOM_TSPDCW", "SL_DOM_TSPDDC", "SL_DOM_TSPD", 
               "SL_EMP_EARN", "SL_TLF_UEM", "SL_TLF_UEMDIS",
               "SL_TLF_UEM_19ICLS", "SL_TLF_UEMDIS_19ICLS", "SL_TLF_NEET", 
               "SL_TLF_NEET_19ICLS", "SL_TLF_CHLDEC", "SL_TLF_CHLDEA",
               "SL_CPA_YEMP", "SI_POV_50MI", "SP_TRN_PUBL",
               "EN_URB_OPENSP", "EN_ACS_URB_OPENSP", "VC_VOH_SXPH",
               "VC_IHR_PSRC", "VC_IHR_PSRCN", "VC_DTH_TOCVN",
               "VC_DTH_TOTR", "VC_DTH_TOTN", "VC_DTH_TOUNN",
               "VC_DTH_TONCVN", "VC_DTH_TOTPT", "VC_HTF_DETVFL",
               "VC_HTF_DETVOP", "VC_HTF_DETVOG", "VC_HTF_DETVSX",
               "VC_HTF_DETV", "VC_HTF_DETVFLR", "VC_HTF_DETVOPR",
               "VC_HTF_DETVOGR", "VC_HTF_DETVSXR", "VC_HTF_DETVR",
               "VC_VAW_SXVLN", "SG_DMK_PARLYN_LC", "SG_DMK_PARLYR_UC",
               "SG_DMK_PARLYP_UC", "SG_DMK_PARLYN_UC", "SG_DMK_PSRVC",
               "SG_DMK_JDC", "SG_DMK_JDC_HGR", "SG_DMK_JDC_LWR",
               "SG_DMK_JDC_CNS", "SG_DMK_PARLMP_LC", "SG_DMK_PARLSP_LC", 
               "SG_DMK_PARLCC_LC", "SG_DMK_PARLMP_UC", "SG_DMK_PARLSP_UC",
               "SG_DMK_PARLCC_UC", "SG_DMK_PARLYR_LC", "SG_DMK_PARLYP_LC", 
               "SG_DMK_PARLCC_JC", "IU_DMK_ICRS", "IU_DMK_INCL",
               "IU_DMK_RESP") 

# write for loop to create main database of all relevant youth indicators
for (series in seriescodes) {
  df1 <- fromJSON(
    paste0('https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=', 
           series, '&pageSize=10000'),
    flatten = TRUE)$data %>%
    as_tibble()
  
  if (is.null(df)) {df <- df1} else {df <- bind_rows(df, df1)}
  } 

df %>% 
  filter(dimensions.Age == "<30Y") %>% 
  mutate(indicator = unlist(indicator)) %>% 
  distinct(indicator)
  filter(indicator == "5.3.2") %>% 
  distinct(dimensions.Age)

# let's look at the different age groups available in the df 
unique(df$dimensions.Age)

# let's clean up the df - filter for age groups we define as "youth"
# can include the 10-17 age group to check data coverage for 8.7.1 too
df_clean <- df %>% 
  mutate(iso3 = countrycode(geoAreaName, 
                      origin = "country.name", 
                      destination = "wb")) %>% 
  filter(dimensions.Age %in% c("15-24", "15-19", "20-24", "18-24", 
                               "15-29", "18-29", "<=45Y", "<35Y", 
                               "<30Y", "<45Y", "<=40Y","<25Y")) %>% 
  drop_na(iso3)
  
# which indicators fit the "youth" relevant age groups? - 5.3.2 is filtered out
# when removing regional aggregates
unique(df_clean$indicator)

# number of countries that have at least one data point in a series since 2015 and 2020
df_clean_a <- df_clean %>% 
  mutate(value = as.numeric(value),
         avail_youth_2020 = case_when(timePeriodStart > 2019 & !is.na(value) ~ 1,
                                T ~ 0),
         avail_youth_2015 = case_when(timePeriodStart > 2014 & !is.na(value) ~ 1,
                                T ~ 0)) %>%
  group_by(geoAreaName, series) %>% 
  summarise(avail_youth_2020 = max(avail_youth_2020),
            avail_youth_2015 = max(avail_youth_2015)) %>%
  select(geoAreaName, series,
         avail_youth_2020, avail_youth_2015) %>% 
  ungroup() 

# series level - number of countries per series 
# n_countries as total count of countries (221)
n_countries <- df %>% 
  mutate(iso3 = countrycode(geoAreaName, 
                            origin = "country.name", 
                            destination = "wb")) %>% 
  drop_na(iso3) %>% 
  pull(geoAreaName) %>% 
  n_distinct()

df_series <- df_clean_a %>% 
  group_by(series) %>% 
  summarise(avail_youth_2020 = sum(avail_youth_2020),
            avail_youth_2015 = sum(avail_youth_2015)) %>% 
  mutate(n_countries = n_countries, 
         pct_youth_2020 = avail_youth_2020/n_countries * 100,
         pct_youth_2015 = avail_youth_2015/n_countries * 100) %>% 
  left_join(df_clean %>% 
              select(series, indicator) %>%
              distinct(), by = "series")

# country level - number of series per country
df_geoArea <- df_clean_a %>% 
  group_by(geoAreaName) %>% 
  summarise(avail_youth_2020 = sum(avail_youth_2020),
            avail_youth_2015 = sum(avail_youth_2015)) %>% 
  mutate(n_series = 34,
         pct_youth_2020 = avail_youth_2020/n_series * 100,
         pct_youth_2015 = avail_youth_2015/n_series * 100)
         

### let's do the same thing this time w/o filtering age groups we define as "youth"
# must filter to have the same series as the series df above
df_clean_all <- df %>% 
  mutate(iso3 = countrycode(geoAreaName, 
                            origin = "country.name", 
                            destination = "wb")) %>% 
  filter(series %in% df_series$series) %>% 
  drop_na(iso3)

# number of countries that have at least one data point in a series since 2015 and 2020
df_clean_b <- df_clean_all %>% 
  mutate(value = as.numeric(value),
         avail_2020 = case_when(timePeriodStart > 2019 & !is.na(value) ~ 1,
                                T ~ 0),
         avail_2015 = case_when(timePeriodStart > 2014 & !is.na(value) ~ 1,
                                T ~ 0)) %>%
  group_by(geoAreaName, series) %>% 
  summarise(avail_2020 = max(avail_2020),
            avail_2015 = max(avail_2015)) %>%
  select(geoAreaName, series,
         avail_2020, avail_2015) %>% 
  ungroup() 

# series level - number of countries per series
# n_countries is the total count of countries
df_series_all <- df_clean_b %>% 
  group_by(series) %>% 
  summarise(avail_2020 = sum(avail_2020),
            avail_2015 = sum(avail_2015)) %>% 
  mutate(n_countries = n_countries,
         pct_2020 = avail_2020/n_countries * 100,
         pct_2015 = avail_2015/n_countries * 100) %>% 
  left_join(df_clean_all %>% select(series, 
                                    indicator) %>% distinct(), by = "series")

# country level - number of series per country
df_geoArea_all <- df_clean_b %>% 
  group_by(geoAreaName) %>% 
  summarise(avail_2020 = sum(avail_2020),
            avail_2015 = sum(avail_2015)) %>% 
  mutate(n_series = 34,
         pct_2020 = avail_2020/n_series * 100,
         pct_2015 = avail_2015/n_series * 100)

### now let's merge all age and youth series dfs
df_series_clean <- df_series %>% 
  left_join(df_series_all, by = c("series", "indicator",
                                  "n_countries"))

# flatten the list of indicators
df_series_clean$indicator <- unlist(df_series_clean$indicator)

# write.csv
write.csv(df_series_clean, "df_series_clean.csv", row.names = FALSE)


### let's merge all age and youth geoArea dfs
df_geoArea_clean <- df_geoArea %>% 
  left_join(df_geoArea_all, by = c("geoAreaName",
                                  "n_series"))

### let's get data coverage for all of the indicators/series for all age groups
df_clean_total <- df %>% 
  mutate(iso3 = countrycode(geoAreaName, 
                            origin = "country.name", 
                            destination = "wb")) %>% 
  drop_na(iso3)

# number of countries that have at least one data point in a series since 2015 and 2020
df_clean_c <- df_clean_total %>% 
  mutate(value = as.numeric(value),
         avail_2020 = case_when(timePeriodStart > 2019 & !is.na(value) ~ 1,
                                T ~ 0),
         avail_2015 = case_when(timePeriodStart > 2014 & !is.na(value) ~ 1,
                                T ~ 0)) %>%
  group_by(geoAreaName, series) %>% 
  summarise(avail_2020 = max(avail_2020),
            avail_2015 = max(avail_2015)) %>%
  select(geoAreaName, series,
         avail_2020, avail_2015) %>% 
  ungroup() 

# series level - number of countries per series
# n_countries is the total count of countries
df_series_total <- df_clean_c %>% 
  group_by(series) %>% 
  summarise(avail_2020 = sum(avail_2020),
            avail_2015 = sum(avail_2015)) %>% 
  mutate(n_countries = n_countries,
         pct_2020 = avail_2020/n_countries * 100,
         pct_2015 = avail_2015/n_countries * 100) %>% 
  left_join(df_clean_total %>% select(series, 
                                    indicator) %>% distinct(), by = "series")

# flatten the list of indicators
df_series_total$indicator <- unlist(df_series_total$indicator)

# write.csv
write.csv(df_series_total, "df_series_total.csv", row.names = FALSE)

df_series %>% 
  mutate(indicator = unlist(indicator)) %>% 
  distinct(indicator)
# country level - number of series per country
df_geoArea_total <- df_clean_c %>% 
  group_by(geoAreaName) %>% 
  summarise(avail_2020 = sum(avail_2020),
            avail_2015 = sum(avail_2015)) %>% 
  mutate(n_series = 49,
         pct_2020 = avail_2020/n_series * 100,
         pct_2015 = avail_2015/n_series * 100)


