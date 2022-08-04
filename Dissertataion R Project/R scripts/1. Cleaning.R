library(tidyverse)
library(tidysynth)
library(lubridate)

rm(list = ls())

unemp_eurostat <- read_csv(here::here("data","ei_lmhr_m_linear.csv.gz"),col_types = "fffffffcdf") %>%
  janitor::clean_names() %>%
  filter(indic == "LM-UN-T-TOT") %>%
  select(-c(indic, dataflow,last_update,freq,unit)) %>%
  mutate(period_as_date = lubridate::as_date(paste0(time_period,"-15")),
         source = "Eurostats") %>%
  as.data.frame()

unemp_eurostat_nsa <- unemp_eurostat %>%
  filter(s_adj == "NSA") %>%
  select(-c(s_adj, obs_flag)) %>%
  rename(location = geo,
         unemp_rate = obs_value
         )

unemp_eurostat_sa <- unemp_eurostat %>%
  filter(s_adj == "SA") %>%
  select(-c(s_adj, obs_flag)) %>%
  rename(location = geo,
         unemp_rate = obs_value
  )

###plot chart

unemp_eurostat_nsa %>%
  filter(
    #location == "US", 
         period_as_date > "2015-01-01") %>%
  ggplot(aes(x = period_as_date,
             y = unemp_rate, 
             group = location)) +
  geom_line(alpha = 0.2)+
  geom_line(data = (unemp_eurostat_nsa %>%
                      filter(
                        location == "US", 
                        period_as_date > "2015-01-01")), 
            inherit.aes = T,
            colour = "red", 
            #size = 2
            )

unemp_oecd_monthly <- read_csv(here::here("data","OECD_monthly.csv"),col_types = "fffffcdf") %>%
  janitor::clean_names() %>%
  select(-c(indicator,subject,measure, frequency)) %>%
  mutate(period_as_date = lubridate::as_date(paste0(time,"-15")), 
         source = "OECD_monthly") %>%
  as.data.frame() %>%
  select(-flag_codes) %>%
  rename(time_period = time,
         unemp_rate = value)

unemp_oecd_quarterly <- read_csv(here::here("data","OECD_quarterly.csv"),col_types = "fffffcdf") %>%
  janitor::clean_names() %>%
  select(-c(indicator,subject,measure, frequency)) %>%
  filter(location %in% c("NZL","ISR")) %>%
  separate(time,into = c("year","qtr") ,sep = "-") %>%
  mutate(qtr_start_mth = case_when(qtr == "Q1" ~ 1,
                                   qtr == "Q2" ~ 4,
                                   qtr == "Q3" ~ 7,
                                   qtr == "Q4" ~ 10),
         qtr_end_mth = case_when(qtr == "Q1" ~ 3,
                                 qtr == "Q2" ~ 6, 
                                 qtr == "Q3" ~ 9, 
                                 qtr == "Q4" ~ 12))%>% 
  mutate(month = map2(qtr_start_mth, qtr_end_mth, `:`)) %>% 
  separate_rows()%>% 
  unnest(month) %>%
  select(-c(qtr_start_mth,qtr, qtr_end_mth)) %>%
  filter(location == "NZL" | (location == "ISR"  & year < 2012)) %>%
  mutate(month = str_pad(month, 2,"left", pad = "0")) %>%
  unite(time, year, month, sep = "-") %>%
  mutate(period_as_date = lubridate::as_date(paste0(time,"-15")),
         source = "OECD_quarterly") %>%
  as.data.frame() %>%
  select(-flag_codes) %>%
  rename(time_period = time,
         unemp_rate = value) 

unemp_all_countries <- unemp_eurostat_sa %>%
  rbind(unemp_oecd_monthly) %>%
  rbind(unemp_oecd_quarterly)

#country_codes <- clipr::read_clip()

#oecd_country_codes <- clipr::read_clip()

# country_codes_df <- data.frame(country_codes, oecd_country_codes) %>%
#   rename(default_code = country_codes) %>% 
#   mutate(country_n = 1:n())

country_codes_df <- read_csv(here::here("data","Country Codes.csv")) %>%
  select(-`...1`)

unemp_oecd_countries <- unemp_all_countries %>%
  filter(location %in% country_codes_df$default_code) %>%
  filter(period_as_date > "2004-12-31")

unemp_oecd_countries %>%
  count(location)

unemp_oecd_countries_firstlast <- unemp_oecd_countries %>%
  group_by(location) %>%
  summarise(max = max(period_as_date, na.rm = T),
            min = min(period_as_date, na.rm = T),
            n = n()) %>%
  mutate(earlymax = if_else(max < "2022-04-15", "Y", as.character(NA)),
         latemin = if_else(min > "2005-01-15", "Y", as.character(NA))) %>%
  mutate(comments = case_when(location == "CH" ~ "Only annual data available pre-2010 for Switzerland",
                              location == "CRI" ~ "No data available pre-2010 for Costa Rica"))

gdp <- read_csv(here::here("data","gdp quarterly indicator.csv")) %>%
  janitor::clean_names() %>%
  filter(subject == "TOT" & measure == "PC_CHGPP" & frequency == "Q") %>%
  select(location, time, value) %>%
  filter(location %in% country_codes_df$oecd_country_codes) %>%
  separate(time,into = c("year","qtr") ,sep = "-") %>%
  mutate(qtr_start_mth = case_when(qtr == "Q1" ~ 1,
                                   qtr == "Q2" ~ 4,
                                   qtr == "Q3" ~ 7,
                                   qtr == "Q4" ~ 10),
         qtr_end_mth = case_when(qtr == "Q1" ~ 3,
                                 qtr == "Q2" ~ 6, 
                                 qtr == "Q3" ~ 9, 
                                 qtr == "Q4" ~ 12))%>% 
  mutate(month = map2(qtr_start_mth, qtr_end_mth, `:`)) %>% 
  separate_rows()%>% 
  unnest(month) %>%
  select(-c(qtr_start_mth,qtr, qtr_end_mth)) %>%
  filter(year >2006 & year <2022) %>%
  mutate(month = str_pad(month, 2,"left", pad = "0")) %>%
  unite(time, year, month, sep = "-") %>%
  mutate(period_as_date = lubridate::as_date(paste0(time,"-15")),
         source = "OECD_quarterly") %>%
  as.data.frame() %>%
  rename(time_period = time,
         gdp_pct_change = value,
         oecd_country_codes = location)  %>%
  select(-source)

gdp_firstlast <- gdp %>%
  group_by(oecd_country_codes) %>%
  summarise(max = max(period_as_date, na.rm = T),
            min = min(period_as_date, na.rm = T),
            n = n()) %>%
  mutate(earlymax = if_else(max < "2021-12-15", "Y", as.character(NA)),
         latemin = if_else(min > "2007-01-15", "Y", as.character(NA)))

all_indicators <- unemp_oecd_countries %>%
  merge(country_codes_df, by.x = "location", by.y = "default_code") %>%
  merge(gdp,
        all = TRUE,
        #by.x = c("oecd_country_codes","period_as_date")
        ) %>%
  filter(period_as_date > "2007-01-01" & period_as_date < "2022-01-01")

all_indicators_checks <- all_indicators %>%
  mutate(which = case_when(!is.na(gdp_pct_change & unemp_rate) ~ "Both",
                   !is.na(gdp_pct_change) ~ "gdp",
                   !is.na(unemp_rate) ~ "unemp_rate",
                   TRUE ~ "neither")) %>%
  group_by(location,oecd_country_codes,which) %>%
  summarise(max = max(period_as_date, na.rm = T),
            min = min(period_as_date, na.rm = T),
            n = n()) %>%
  mutate(earlymax = if_else(max < "2021-12-15", "Y", as.character(NA)),
         latemin = if_else(min > "2007-01-15", "Y", as.character(NA)))

all_indicators <- all_indicators %>%
  merge(country_codes_df, all.x = T) %>%
  select(-default_code)

all_indicators %>% glimpse

scheme_deets <- read_csv(here::here("data","all_schemes.csv"))

all_indicators <- all_indicators %>% 
  merge(scheme_deets, all.x = T, by = "oecd_country_codes")

all_indicators_checks <- all_indicators %>%
  filter(is.na(scheme)) 

all_indicators_checks #should be 0

mortality <- read_csv(here::here("data","HEALTH_MORTALITY.csv")) %>%
  janitor::clean_names()%>%
  filter(gender == "TOTAL" & age == "TOTAL") %>%
  select(-starts_with("gender"),-starts_with("age"),-ends_with("_2"),-starts_with("flag"), -week_number)

mortality_wide <- mortality %>%
  pivot_wider(names_from = variable, 
              values_from = value) %>%
  janitor::clean_names() %>%
  filter(week != 53) %>% # due to difference in methodological calculation
  mutate(normnb = excessnb/(excesspc/100),
         totnb = excessnb+normnb,
         dayinyear = week*7,
         yearasnumber = as.numeric(as.Date(paste0(year,"-01-01"))),
         dayasnumber = as.Date(yearasnumber+dayinyear-1, origin = "1970-01-01"),
         month = floor_date(dayasnumber, "month"))

mortality_monthly <- mortality_wide %>%
  group_by(country,month) %>% 
  summarise(normnb = sum(normnb, na.rm = F),
            totnb = sum(totnb, na.rm = F)) %>%
  mutate(excessnb = totnb-normnb,
         excesspc = excessnb/normnb,
         month = month+14) %>%
  filter(!is.nan(excesspc)) %>%
  select(country, month,excesspc)

all_indicators_mortality <- all_indicators %>%
  merge(mortality_monthly,
        by.x = c("oecd_country_codes","period_as_date"),
        by.y = c("country","month"),
        all.x = T)

all_indicators <- all_indicators_mortality

all_indicators_checks <- all_indicators %>%
  filter(period_as_date > "2020-02-15") %>%
  group_by(oecd_country_codes,scheme) %>%
  count(is.na(excesspc))

##JPN and IRL filtered when doing the mortality indicators


save(all_indicators, file = here::here("data","all_indicators.rda"))

rm(list = ls())
