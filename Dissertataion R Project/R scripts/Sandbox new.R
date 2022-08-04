library(tidyverse)
library(tidysynth)

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

country_codes_df <- data.frame(country_codes, oecd_country_codes) %>%
  rename(default_code = country_codes)

unemp_oecd_countries <- unemp_all_countries %>%
  filter(location %in% country_codes) %>%
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
  mutate(earlymax = if_else(max < "2022-04-15", "Y", as.character(NA)),
         latemin = if_else(min > "2005-01-15", "Y", as.character(NA)))# %>%
  # mutate(comments = case_when(location == "CH" ~ "Only annual data available pre-2010 for Switzerland",
  #                             location == "CRI" ~ "No data available pre-2010 for Costa Rica"))


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
  mutate(earlymax = if_else(max < "2022-04-15", "Y", as.character(NA)),
         latemin = if_else(min > "2005-01-15", "Y", as.character(NA)))

# lfp <-  read_csv(here::here("data","labour force participation indicator.csv")) %>%
#   janitor::clean_names() %>%
#   filter(subject == "25_64") %>%
#   select(location, time, value) %>%
#   rename(lfp = value)
# 
# all_indicators <- unemp %>% 
#   merge(gdp, all= T) %>%
#   merge(lfp, all = T)

country_codes_df <- country_codes_df %>% 
  mutate(country_n = 1:n())

all_indicators <- all_indicators %>%
  merge(country_codes_df, all.x = T) %>%
  select(-default_code)


# all_indicators_wide <- all_indicators %>%
#   arrange(time) %>%
#   pivot_wider(names_from = time, values_from = c(unemp, gdp, lfp)) %>%
#   pivot_longer(starts_with(c("unemp","gdp","lfp")),
#                names_to = c("indicator","time"),
#                names_sep = "_") %>%
#   pivot_wider(names_from = indicator, values_from = value) %>%
#   group_by(country) %>%
#   mutate(na = sum(!is.na(unemp)),
#          time = as.numeric(time)) %>%
#   filter(na == 22) %>%
#   mutate(na = sum(is.na(lfp))) %>%
#   filter(na != 22) %>%
#   rename(countrycode = country_code) %>% 
#   #select(countrycode, time, country,unemp,gdp,lfp) %>%
#   
#   as.data.frame()

controls <- ((all_indicators %>% filter(country_n != 30))%>%  arrange(country_n))$country_n %>%unique() 

controls_top_used_schemes <- all_indicators %>%
  filter(oecd_country_codes %in% c( "NZL" ,#- New zealand (65%)
                                    "ITA" ,#- Italy (45%)
                                    "AUT" ,#- Austria (35%)
                                    "GBR" ,#(30%)
                                    "PRT" #- Portugal
  )) %>%
  select(oecd_country_codes,country_n) %>%
  unique()

controls_newWS_schemes <- all_indicators %>%
  filter(oecd_country_codes %in% c("AUS",
                                   "CAN",
                                   "EST",
                                   "IRL",
                                   "NLD",
                                   "NZL",
                                   "POL"
                                   
  )) %>%
  select(oecd_country_codes,country_n) %>%
  unique()
  

all_indicators %>% glimpse

all_indicators_out_all <- all_indicators %>%
  filter(!(oecd_country_codes %in% c("TUR","CHE","CRI"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=T) %>%
  # generate_predictor(time_window = as.numeric(as.Date("2017-01-15")):as.numeric(as.Date("2019-12-15")),
  #                    gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2009-01-15")),
                     unemp_2009 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2010 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2015 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2019-01-15")),
                     unemp_2019 = unemp_rate) %>%generate_weights() %>%
  generate_control()

all_indicators_out_top <- all_indicators %>%
  filter((oecd_country_codes %in% c(controls_newWS_schemes$oecd_country_codes,"USA"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=TRUE) %>%
  # generate_predictor(time_window = as.numeric(as.Date("2017-01-15")):as.numeric(as.Date("2019-12-15")),
  #                    gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2009-01-15")),
                     unemp_2009 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2010 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2019-01-15")),
                     unemp_2019 = unemp_rate) %>%
  generate_weights(optimization_window = as.numeric(as.Date("2015-01-15")):as.numeric(as.Date("2019-01-15"))) %>%
  generate_control()



synthetic_usa <- all_indicators_out_top[[6]][[2]] 

real_usa <- all_indicators %>%
  filter(oecd_country_codes == "USA")

usa <- synthetic_usa %>%
  merge(real_usa,
        by.x = "time_unit",
        by.y = "period_as_number")

usa %>%
  ggplot(aes(x = period_as_date )) +
  geom_line(aes(y = unemp_rate), colour = "red")+
  geom_line(aes(y = synth_y), colour = "blue")

all_indicators_out_all %>% plot_trends()
all_indicators_out_all %>% plot_placebos()
all_indicators_out_top %>% plot_placebos()

all_indicators_out_top %>% plot_differences()

all_indicators_out_top %>% plot_mspe_ratio()

all_indicators_out_top %>% plot_placebos()

all_indicators_out_top %>% grab_signficance()