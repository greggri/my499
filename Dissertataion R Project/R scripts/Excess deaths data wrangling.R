library(tidyverse)
library(lubridate)
library(tidysynth)

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
  filter(!is.nan(excesspc))

all_indictors_mortality <- all_indicators %>%
  merge(mortality_monthly,
        by.x = c("oecd_country_codes","period_as_date"),
        by.y = c("country","month"),
        all.x = T)


all_indicators_mort_out <- all_indictors_mortality %>%
  filter(!(oecd_country_codes %in% c("TUR","CHE","CRI","IRL","JPN","KOR"))) %>%
  #filter((oecd_country_codes %in% c(controls_newWS_schemes$oecd_country_codes,"USA"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=TRUE) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-01-15")):as.numeric(as.Date("2021-12-15")),
                      excesspc = mean(excesspc, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2010 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2015-01-15")),
                     unemp_2015 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2019-01-15")),
                     unemp_2019 = unemp_rate) %>%
  generate_weights(optimization_window = as.numeric(as.Date("2015-01-15")):as.numeric(as.Date("2019-01-15"))) %>%
  generate_control()


new_ws_mort_out <- all_indictors_mortality %>%
  filter(!(oecd_country_codes %in% c("TUR","CHE","CRI","IRL","JPN","KOR"))) %>%
  filter((oecd_country_codes %in% c(controls_newWS_schemes$oecd_country_codes,"USA"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=TRUE) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-01-15")):as.numeric(as.Date("2021-12-15")),
                     excesspc = mean(excesspc, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2009-01-15")),
                     unemp_2009 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2010 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2019-01-15")),
                     unemp_2019 = unemp_rate) %>%
  generate_weights(optimization_window = as.numeric(as.Date("2015-01-15")):as.numeric(as.Date("2019-01-15"))) %>%
  generate_control()

top_schemes_mort_out <- all_indictors_mortality %>%
  filter(!(oecd_country_codes %in% c("TUR","CHE","CRI","IRL","JPN","KOR"))) %>%
  filter((oecd_country_codes %in% c(controls_top_used_schemes$oecd_country_codes,"USA"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=TRUE) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-01-15")):as.numeric(as.Date("2021-12-15")),
                     excesspc = mean(excesspc, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2009-01-15")),
                     unemp_2009 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2010-01-15")),
                     unemp_2010 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2019-01-15")),
                     unemp_2019 = unemp_rate) %>%
  generate_weights(optimization_window = as.numeric(as.Date("2015-01-15")):as.numeric(as.Date("2019-01-15"))) %>%
  generate_control()


all_indicators_mort_out %>% plot_placebos(prune = F)
all_indicators_mort_out %>% plot_weights()
all_indicators_mort_out %>% plot_mspe_ratio()
all_indicators_mort_out %>% grab_signficance()
