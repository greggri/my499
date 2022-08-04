library(tidyverse)
library(tidysynth)

rm(list = ls())

options(scipen = 999)

load(here::here("data","all_indicators.rda"))

all_indicators <- all_indicators %>% janitor::clean_names()

all_jrs <- all_indicators %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number) %>%
  filter((oecd_country_codes %in% c("USA") | scheme == 1) &
           !(oecd_country_codes %in% c("TUR","CHE","CRI","IRL","JPN"))) 

oecd_countries <- (all_jrs %>% select(oecd_country_codes) %>% filter(!(oecd_country_codes %in% c("USA")))%>% unique())$oecd_country_codes 

oecd_countries <- c("GBR","ISL","EST","HUN","DEU")

mylist <- list()

for(country in oecd_countries){
  m <- all_jrs %>%
    filter(!(oecd_country_codes %in% country))
  
  mylist[[country]]<-m
}

controls <- list()


my_synth_control <- function(data){
  data %>%
    synthetic_control(outcome = unemp_rate,
                      unit = oecd_country_codes,
                      time = period_as_number,
                      i_unit = "USA",
                      i_time = as.numeric(as.Date("2020-03-15")),
                      generate_placebos=T)
}

controls <- lapply(mylist,my_synth_control)

my_predictors <- function(data){
  data %>% 
    generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')), unemp_13559= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')), unemp_13618= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')), unemp_13679= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13740, origin = '1970-01-01')), unemp_13740= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13801, origin = '1970-01-01')), unemp_13801= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13862, origin = '1970-01-01')), unemp_13862= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13924, origin = '1970-01-01')), unemp_13924= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(13984, origin = '1970-01-01')), unemp_13984= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14045, origin = '1970-01-01')), unemp_14045= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14106, origin = '1970-01-01')), unemp_14106= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14167, origin = '1970-01-01')), unemp_14167= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14228, origin = '1970-01-01')), unemp_14228= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14290, origin = '1970-01-01')), unemp_14290= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14349, origin = '1970-01-01')), unemp_14349= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14410, origin = '1970-01-01')), unemp_14410= unemp_rate)%>% 
    generate_predictor(time_window = as.numeric(as.Date(14471, origin = '1970-01-01')), unemp_14471= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14532, origin = '1970-01-01')), unemp_14532= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14593, origin = '1970-01-01')), unemp_14593= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14655, origin = '1970-01-01')), unemp_14655= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14714, origin = '1970-01-01')), unemp_14714= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14775, origin = '1970-01-01')), unemp_14775= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14836, origin = '1970-01-01')), unemp_14836= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14897, origin = '1970-01-01')), unemp_14897= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(14958, origin = '1970-01-01')), unemp_14958= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15020, origin = '1970-01-01')), unemp_15020= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15079, origin = '1970-01-01')), unemp_15079= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15140, origin = '1970-01-01')), unemp_15140= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15201, origin = '1970-01-01')), unemp_15201= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15262, origin = '1970-01-01')), unemp_15262= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15323, origin = '1970-01-01')), unemp_15323= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15385, origin = '1970-01-01')), unemp_15385= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15445, origin = '1970-01-01')), unemp_15445= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15506, origin = '1970-01-01')), unemp_15506= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15567, origin = '1970-01-01')), unemp_15567= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15628, origin = '1970-01-01')), unemp_15628= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15689, origin = '1970-01-01')), unemp_15689= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15751, origin = '1970-01-01')), unemp_15751= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15810, origin = '1970-01-01')), unemp_15810= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15871, origin = '1970-01-01')), unemp_15871= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15932, origin = '1970-01-01')), unemp_15932= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(15993, origin = '1970-01-01')), unemp_15993= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16054, origin = '1970-01-01')), unemp_16054= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16116, origin = '1970-01-01')), unemp_16116= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16175, origin = '1970-01-01')), unemp_16175= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16236, origin = '1970-01-01')), unemp_16236= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16297, origin = '1970-01-01')), unemp_16297= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16358, origin = '1970-01-01')), unemp_16358= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16419, origin = '1970-01-01')), unemp_16419= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16481, origin = '1970-01-01')), unemp_16481= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16540, origin = '1970-01-01')), unemp_16540= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16601, origin = '1970-01-01')), unemp_16601= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16662, origin = '1970-01-01')), unemp_16662= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16723, origin = '1970-01-01')), unemp_16723= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16784, origin = '1970-01-01')), unemp_16784= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16846, origin = '1970-01-01')), unemp_16846= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16906, origin = '1970-01-01')), unemp_16906= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(16967, origin = '1970-01-01')), unemp_16967= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17028, origin = '1970-01-01')), unemp_17028= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17089, origin = '1970-01-01')), unemp_17089= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17150, origin = '1970-01-01')), unemp_17150= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17212, origin = '1970-01-01')), unemp_17212= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17271, origin = '1970-01-01')), unemp_17271= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17332, origin = '1970-01-01')), unemp_17332= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17393, origin = '1970-01-01')), unemp_17393= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17454, origin = '1970-01-01')), unemp_17454= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17515, origin = '1970-01-01')), unemp_17515= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17577, origin = '1970-01-01')), unemp_17577= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17636, origin = '1970-01-01')), unemp_17636= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17697, origin = '1970-01-01')), unemp_17697= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17758, origin = '1970-01-01')), unemp_17758= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17819, origin = '1970-01-01')), unemp_17819= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17880, origin = '1970-01-01')), unemp_17880= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(17942, origin = '1970-01-01')), unemp_17942= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18001, origin = '1970-01-01')), unemp_18001= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18062, origin = '1970-01-01')), unemp_18062= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18123, origin = '1970-01-01')), unemp_18123= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18184, origin = '1970-01-01')), unemp_18184= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18245, origin = '1970-01-01')), unemp_18245= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date(18307, origin = '1970-01-01')), unemp_18307= unemp_rate)%>%
    generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                      gdp = mean(gdp_pct_change, na.rm = T)) %>%
    generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                      excess_mort = mean(excesspc, na.rm = T)) %>%
    generate_weights() %>%
    generate_control()
  
}

permutations <- lapply(controls,my_predictors)

no_GBR <- controls$GBR %>% my_predictors()
no_ISL <- controls$ISL %>% my_predictors()
no_EST <- controls$EST %>% my_predictors()
no_HUN <- controls$HUN %>% my_predictors()
no_DEU <- controls$DEU %>% my_predictors()

no_GBR %>% grab_signficance()
no_EST %>% grab_signficance()
no_HUN %>% grab_signficance()
no_DEU %>% grab_signficance()

no_GBR %>% grab_unit_weights() %>% arrange(-weight)
no_EST %>% grab_unit_weights() %>% arrange(-weight)
no_HUN %>% grab_unit_weights() %>% arrange(-weight)
no_DEU %>% grab_unit_weights() %>% arrange(-weight)

####ISL####

no_ISL <- all_jrs %>%
  filter(!(oecd_country_codes %in% "ISL")) %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=T) %>%
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')), unemp_13559= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')), unemp_13618= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')), unemp_13679= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13740, origin = '1970-01-01')), unemp_13740= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13801, origin = '1970-01-01')), unemp_13801= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13862, origin = '1970-01-01')), unemp_13862= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13924, origin = '1970-01-01')), unemp_13924= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13984, origin = '1970-01-01')), unemp_13984= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14045, origin = '1970-01-01')), unemp_14045= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14106, origin = '1970-01-01')), unemp_14106= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14167, origin = '1970-01-01')), unemp_14167= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14228, origin = '1970-01-01')), unemp_14228= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14290, origin = '1970-01-01')), unemp_14290= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14349, origin = '1970-01-01')), unemp_14349= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14410, origin = '1970-01-01')), unemp_14410= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14471, origin = '1970-01-01')), unemp_14471= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14532, origin = '1970-01-01')), unemp_14532= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14593, origin = '1970-01-01')), unemp_14593= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14655, origin = '1970-01-01')), unemp_14655= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14714, origin = '1970-01-01')), unemp_14714= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14775, origin = '1970-01-01')), unemp_14775= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14836, origin = '1970-01-01')), unemp_14836= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14897, origin = '1970-01-01')), unemp_14897= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14958, origin = '1970-01-01')), unemp_14958= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15020, origin = '1970-01-01')), unemp_15020= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15079, origin = '1970-01-01')), unemp_15079= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15140, origin = '1970-01-01')), unemp_15140= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15201, origin = '1970-01-01')), unemp_15201= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15262, origin = '1970-01-01')), unemp_15262= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15323, origin = '1970-01-01')), unemp_15323= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15385, origin = '1970-01-01')), unemp_15385= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15445, origin = '1970-01-01')), unemp_15445= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15506, origin = '1970-01-01')), unemp_15506= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15567, origin = '1970-01-01')), unemp_15567= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15628, origin = '1970-01-01')), unemp_15628= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15689, origin = '1970-01-01')), unemp_15689= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15751, origin = '1970-01-01')), unemp_15751= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15810, origin = '1970-01-01')), unemp_15810= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15871, origin = '1970-01-01')), unemp_15871= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15932, origin = '1970-01-01')), unemp_15932= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15993, origin = '1970-01-01')), unemp_15993= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16054, origin = '1970-01-01')), unemp_16054= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16116, origin = '1970-01-01')), unemp_16116= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16175, origin = '1970-01-01')), unemp_16175= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16236, origin = '1970-01-01')), unemp_16236= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16297, origin = '1970-01-01')), unemp_16297= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16358, origin = '1970-01-01')), unemp_16358= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16419, origin = '1970-01-01')), unemp_16419= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16481, origin = '1970-01-01')), unemp_16481= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16540, origin = '1970-01-01')), unemp_16540= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16601, origin = '1970-01-01')), unemp_16601= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16662, origin = '1970-01-01')), unemp_16662= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16723, origin = '1970-01-01')), unemp_16723= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16784, origin = '1970-01-01')), unemp_16784= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16846, origin = '1970-01-01')), unemp_16846= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16906, origin = '1970-01-01')), unemp_16906= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16967, origin = '1970-01-01')), unemp_16967= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17028, origin = '1970-01-01')), unemp_17028= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17089, origin = '1970-01-01')), unemp_17089= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17150, origin = '1970-01-01')), unemp_17150= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17212, origin = '1970-01-01')), unemp_17212= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17271, origin = '1970-01-01')), unemp_17271= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17332, origin = '1970-01-01')), unemp_17332= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17393, origin = '1970-01-01')), unemp_17393= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17454, origin = '1970-01-01')), unemp_17454= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17515, origin = '1970-01-01')), unemp_17515= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17577, origin = '1970-01-01')), unemp_17577= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17636, origin = '1970-01-01')), unemp_17636= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17697, origin = '1970-01-01')), unemp_17697= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17758, origin = '1970-01-01')), unemp_17758= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17819, origin = '1970-01-01')), unemp_17819= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17880, origin = '1970-01-01')), unemp_17880= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(17942, origin = '1970-01-01')), unemp_17942= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18001, origin = '1970-01-01')), unemp_18001= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18062, origin = '1970-01-01')), unemp_18062= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18123, origin = '1970-01-01')), unemp_18123= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18184, origin = '1970-01-01')), unemp_18184= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18245, origin = '1970-01-01')), unemp_18245= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(18307, origin = '1970-01-01')), unemp_18307= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

no_ISL <- no_ISL %>% generate_weights(optimization_method = "All") 

no_ISL %>% grab_signficance()

no_ISL %>% grab_unit_weights() %>% arrange(-weight)

#############################

significance_tab_func <- function(data, spec){
 tab1 <- data %>% grab_signficance() %>%
    select(-rank) %>% 
    filter(type == "Treated") %>%
    mutate(spec = spec ) %>%
    select(-unit_name, -type) %>%
    pivot_longer(-spec) %>%
    pivot_wider(names_from = "spec")
 
 tab2 <-   data %>% 
   grab_unit_weights() %>% 
   slice_max(weight,n = 5) %>%
   filter(weight >0.05) %>%
   rename_with(.fn = ~gsub("weight",spec,.x))
 
 tab1 %>%
   rename(unit = name) %>%
   rbind(tab2)
}

results_tab <- no_GBR %>% significance_tab_func("No GBR") %>%
  merge(no_ISL %>% significance_tab_func("No ISL"), all = T)  %>%
  merge(no_EST %>% significance_tab_func("No EST"), all = T) %>%
  merge(no_HUN %>% significance_tab_func("No HUN"), all = T) %>%
  merge(no_DEU %>% significance_tab_func("No DEU"), all = T)  %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum) %>%
  mutate(unit = factor(unit) %>%
           fct_reorder(-`No DEU`) %>%
           fct_reorder(-`No EST`) %>%
           fct_reorder(-`No HUN`) %>%
           fct_reorder(-`No ISL`) %>%
           fct_reorder(-`No GBR`) %>%
           fct_relevel("pre_mspe","post_mspe","mspe_ratio","fishers_exact_pvalue","z_score", after = Inf)) %>%
  arrange(unit)
  
xtable::xtable(results_tab)

