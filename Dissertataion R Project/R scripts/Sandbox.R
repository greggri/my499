library(tidyverse)
library(Synth)

econ_ind <- read_csv(here::here("data","OECD economic indicators.csv")) %>%
  janitor::clean_names()

econ_ind_clean <- econ_ind %>% 
  separate(subject_2, into = c("sub_OECD","sub_series","sub_indicator","sub_type"),">") %>%
  mutate(across(.fns = ~str_squish(.x))) %>%
  select(-starts_with("flag"),-starts_with("reference"))

# econ_ind_clean %>% 
#   select(subject,measure,frequency,unit,power_code) %>% 
#   distinct() %>% 
#   mutate(dup = duplicated(subject, fromLast = TRUE)|duplicated(subject)) %>% 
#   View()

lookup_location <- econ_ind_clean %>% 
  select(location, country) %>%
  distinct() %>%
  mutate(country_code = 1:n())

lookup_subject <- econ_ind_clean %>% 
  select(starts_with("sub")) %>%
  distinct()

lookup_measure <- econ_ind_clean %>%
  select(starts_with("measure")) %>%
  distinct()

###frequency is either M monthly or Q quarterly (no annual)

lookup_unit <- econ_ind_clean %>% 
  select(starts_with("unit")) %>%
  distinct()

lookup_power <- econ_ind_clean %>%
  select(starts_with("power")) %>%
  distinct()

###Which indicators am I interested in?####
INDICATORS <- c(
  "LOCOEMNO", #Employment - Unemployment, Normalised
  "LORSGPNO", #Gross Domestic Product (GDP), Normalised
  "LOCOCENO", #CS - Expected economic situation, Normalised
  "LOCOXGNO" #Export of goods
)

econ_ind_codes <- econ_ind_clean %>% 
  filter(subject %in% INDICATORS) %>%
  select(location, subject, time, value) 

econ_ind_wide <- econ_ind_codes %>%
  pivot_wider(names_from = subject,
              values_from = value)

econ_ind_wide_TEST <- econ_ind_wide %>%
  mutate(time = paste0(time,"-01"),
         date = lubridate::as_date(time, format = "%Y-%m-%d")) %>%
  merge(lookup_location, by = "location")

unemp_annual <- unemp %>%
  filter(frequency == "A" & subject == "TOT") %>%
   mutate(time = as.numeric(time),
  #        date = lubridate::as_date(time, format = "%Y-%m-%d")
  ) %>%
  merge(countrycode, by = "location") %>%
  as.data.frame()

unemp_oecd <- unemp_annual %>%
  filter(country_code == 35)

balanced_data <- unemp_annual %>%
  #filter(time %in% unemp_oecd$time) %>%
  select(country_code, location, time, value) %>%
  group_by(country_code) %>%
  filter(!is.na(value))%>%
  mutate(n = n(),
         date = as.numeric(time),
         #X1 = runif(n(), min = 2, max = 10)
         ) %>%
  
  filter(n == 17) %>%
  
  select(-n) %>%
  
  as.data.frame()

balanced_data_test <- econ_ind_wide_TEST %>%
  rename(unit.num = country_code,
         #year = time,
         name = location,
         Y = LOCOEMNO) %>%
  mutate(year = as.numeric(date)) %>%
  #select(-c(time,date)) %>%
  filter(!is.na(Y))

dataprep.out<-
  dataprep(
    foo = balanced_data_test,
    predictors = c("LORSGPNO", #Gross Domestic Product (GDP), Normalised
                   "LOCOCENO", #CS - Expected economic situation, Normalised
                   "LOCOXGNO"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    # special.predictors = list(
    #   list("Y", 1991, "mean"),
    #   list("Y", 1985, "mean"),
    #   list("Y", 1980, "mean")
    # ),
    treatment.identifier = 30,
    controls.identifier = controls,
    time.predictors.prior = c(2005:2019),
    time.optimize.ssr = c(2005:2019),
    unit.names.variable = "name",
    time.plot = 2005:2021
  )

synth.out <- synth(dataprep.out)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out)


wide_data <- balanced_data_test %>% select(country_code, location, time, value) %>% pivot_wider(names_from = time, values_from = value)

controls <- (balanced_data_test%>% select(unit.num) %>% filter(unit.num!=30)%>% unique())$unit.num

unemp_data_out <- dataprep(foo = unemp_annual,
                           predictors = c("X1"),
                           predictors.op = "mean",
                           dependent = "value",
                           unit.variable = "country_code",
                           time.variable = "time",
                           treatment.identifier = 30,
                           controls.identifier = c(1,2,3,4),
                           time.predictors.prior = c(2012:2019),
                           time.optimize.ssr = c(2012:2019),
                           unit.names.variable = "location",
                           time.plot = 2012:2021)
