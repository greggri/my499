library(tidyverse)
library(tidysynth)

rm(list = ls())

load(here::here("data","all_indicators.rda"))

all_indicators <- all_indicators %>% janitor::clean_names()

all_jrs <- all_indicators %>%
  filter((oecd_country_codes %in% c("USA") | scheme == 1) &
           !(oecd_country_codes %in% c("TUR","CHE","CRI"))) %>%
  mutate(period_as_number = as.numeric(period_as_date)) %>%
  arrange(period_as_number)

all_jrs_output <-  all_jrs %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=T) 

all_jrs_mort <- all_jrs %>%
  filter(!(oecd_country_codes %in% c("IRL","JPN")))

all_jrs_mort_out <- all_jrs_mort %>%
  synthetic_control(outcome = unemp_rate,
                    unit = oecd_country_codes,
                    time = period_as_number,
                    i_unit = "USA",
                    i_time = as.numeric(as.Date("2020-03-15")),
                    generate_placebos=T) 

#####################################################################################################
#########(1) Original specification: no pre-treatment outcome values, only predictors ###############
#####################################################################################################

only_predictors_out <- all_jrs_mort_out %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

only_predictors_final <- only_predictors_out  %>%
  generate_weights() %>%
  generate_control()


only_predictors_final %>% plot_trends()
only_predictors_final %>% plot_weights()
only_predictors_final %>% plot_placebos()
only_predictors_final %>% plot_differences()
only_predictors_final %>% plot_mspe_ratio()
only_predictors_final %>% grab_signficance()

###############################################################################
#########(1) All pre-treatment outcome values: ################################
############################################### ###############################

pre_intervention_dates <- (all_indicators %>% 
                             select(period_as_date) %>% 
                             distinct() %>% 
                             filter(period_as_date < "2020-03-15"))$period_as_date

pre_intervention_dates %>% head
pre_intervention_dates %>% median
pre_intervention_dates %>% tail
pre_intervention_dates %>% length

for(date in pre_intervention_dates){
  print(date)
  
  temp <- paste0(.GlobalEnv$temp, 
                 "%>% generate_predictor(time_window = as.numeric(as.Date(", 
                 date, ", origin = '1970-01-01')), unemp_",date,"= unemp_rate)")
                                   
  .GlobalEnv$temp <-temp
}

#################
all_t0_predictors <- all_jrs_mort_out %>%
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')),unemp_13528 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')),unemp_13559 = unemp_rate) %>% 
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')),unemp_13587 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')),unemp_13618 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')),unemp_13648 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')),unemp_13679 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')), unemp_13528 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')),unemp_13559 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')),unemp_13587 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')),unemp_13618 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')),unemp_13648 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')),unemp_13679 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13709, origin = '1970-01-01')),unemp_13709 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13740, origin = '1970-01-01')),unemp_13740 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13771, origin = '1970-01-01')),unemp_13771 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13801, origin = '1970-01-01')),unemp_13801 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13832, origin = '1970-01-01')),unemp_13832 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13862, origin = '1970-01-01')),unemp_13862 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13893, origin = '1970-01-01')),unemp_13893 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13924, origin = '1970-01-01')),unemp_13924 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13953, origin = '1970-01-01')),unemp_13953 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(13984, origin = '1970-01-01')),unemp_13984 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14014, origin = '1970-01-01')),unemp_14014 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14045, origin = '1970-01-01')),unemp_14045 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14075, origin = '1970-01-01')),unemp_14075 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14106, origin = '1970-01-01')),unemp_14106 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14137, origin = '1970-01-01')),unemp_14137 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14167, origin = '1970-01-01')),unemp_14167 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14198, origin = '1970-01-01')),unemp_14198 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14228, origin = '1970-01-01')), unemp_14228 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14259, origin = '1970-01-01')), unemp_14259 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14290, origin = '1970-01-01')), unemp_14290 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14318, origin = '1970-01-01')), unemp_14318 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14349, origin = '1970-01-01')), unemp_14349 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14379, origin = '1970-01-01')), unemp_14379 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14410, origin = '1970-01-01')), unemp_14410 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14440, origin = '1970-01-01')), unemp_14440 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14471, origin = '1970-01-01')), unemp_14471 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14502, origin = '1970-01-01')), unemp_14502 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14532, origin = '1970-01-01')), unemp_14532 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14563, origin = '1970-01-01')), unemp_14563 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14593, origin = '1970-01-01')), unemp_14593 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14624, origin = '1970-01-01')), unemp_14624 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14655, origin = '1970-01-01')), unemp_14655 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14683, origin = '1970-01-01')), unemp_14683 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14714, origin = '1970-01-01')), unemp_14714 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14744, origin = '1970-01-01')), unemp_14744 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14775, origin = '1970-01-01')), unemp_14775 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14805, origin = '1970-01-01')), unemp_14805 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14836, origin = '1970-01-01')), unemp_14836 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14867, origin = '1970-01-01')), unemp_14867 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14897, origin = '1970-01-01')), unemp_14897 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14928, origin = '1970-01-01')), unemp_14928 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14958, origin = '1970-01-01')), unemp_14958 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(14989, origin = '1970-01-01')), unemp_14989 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15020, origin = '1970-01-01')), unemp_15020 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15048, origin = '1970-01-01')), unemp_15048 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15079, origin = '1970-01-01')), unemp_15079 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15109, origin = '1970-01-01')), unemp_15109 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15140, origin = '1970-01-01')), unemp_15140 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15170, origin = '1970-01-01')), unemp_15170 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15201, origin = '1970-01-01')), unemp_15201 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15232, origin = '1970-01-01')), unemp_15232 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15262, origin = '1970-01-01')), unemp_15262 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15293, origin = '1970-01-01')), unemp_15293 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15323, origin = '1970-01-01')), unemp_15323 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15354, origin = '1970-01-01')), unemp_15354 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15385, origin = '1970-01-01')), unemp_15385 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15414, origin = '1970-01-01')), unemp_15414 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15445, origin = '1970-01-01')), unemp_15445 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15475, origin = '1970-01-01')), unemp_15475 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15506, origin = '1970-01-01')), unemp_15506 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15536, origin = '1970-01-01')), unemp_15536 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15567, origin = '1970-01-01')), unemp_15567 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15598, origin = '1970-01-01')), unemp_15598 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15628, origin = '1970-01-01')), unemp_15628 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15659, origin = '1970-01-01')), unemp_15659 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15689, origin = '1970-01-01')), unemp_15689 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15720, origin = '1970-01-01')), unemp_15720 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15751, origin = '1970-01-01')), unemp_15751 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15779, origin = '1970-01-01')), unemp_15779 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15810, origin = '1970-01-01')), unemp_15810 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15840, origin = '1970-01-01')), unemp_15840 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15871, origin = '1970-01-01')), unemp_15871 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15901, origin = '1970-01-01')), unemp_15901 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15932, origin = '1970-01-01')), unemp_15932 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15963, origin = '1970-01-01')), unemp_15963 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(15993, origin = '1970-01-01')), unemp_15993 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16024, origin = '1970-01-01')), unemp_16024 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16054, origin = '1970-01-01')), unemp_16054 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16085, origin = '1970-01-01')), unemp_16085 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16116, origin = '1970-01-01')), unemp_16116 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16144, origin = '1970-01-01')), unemp_16144 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16175, origin = '1970-01-01')), unemp_16175 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16205, origin = '1970-01-01')), unemp_16205 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16236, origin = '1970-01-01')), unemp_16236 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16266, origin = '1970-01-01')), unemp_16266 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16297, origin = '1970-01-01')), unemp_16297 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16328, origin = '1970-01-01')), unemp_16328 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16358, origin = '1970-01-01')), unemp_16358 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16389, origin = '1970-01-01')), unemp_16389 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16419, origin = '1970-01-01')), unemp_16419 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16450, origin = '1970-01-01')), unemp_16450 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16481, origin = '1970-01-01')), unemp_16481 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16509, origin = '1970-01-01')), unemp_16509 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16540, origin = '1970-01-01')), unemp_16540 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16570, origin = '1970-01-01')), unemp_16570 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16601, origin = '1970-01-01')), unemp_16601 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16631, origin = '1970-01-01')), unemp_16631 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16662, origin = '1970-01-01')), unemp_16662 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16693, origin = '1970-01-01')), unemp_16693 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16723, origin = '1970-01-01')), unemp_16723 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16754, origin = '1970-01-01')), unemp_16754 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16784, origin = '1970-01-01')), unemp_16784 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16815, origin = '1970-01-01')), unemp_16815 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16846, origin = '1970-01-01')), unemp_16846 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16875, origin = '1970-01-01')), unemp_16875 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16906, origin = '1970-01-01')), unemp_16906 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16936, origin = '1970-01-01')), unemp_16936 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16967, origin = '1970-01-01')), unemp_16967 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(16997, origin = '1970-01-01')), unemp_16997 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17028, origin = '1970-01-01')), unemp_17028 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17059, origin = '1970-01-01')), unemp_17059 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17089, origin = '1970-01-01')), unemp_17089 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17120, origin = '1970-01-01')), unemp_17120 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17150, origin = '1970-01-01')), unemp_17150 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17181, origin = '1970-01-01')), unemp_17181 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17212, origin = '1970-01-01')), unemp_17212 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17240, origin = '1970-01-01')), unemp_17240 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17271, origin = '1970-01-01')), unemp_17271 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17301, origin = '1970-01-01')), unemp_17301 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17332, origin = '1970-01-01')), unemp_17332 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17362, origin = '1970-01-01')), unemp_17362 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17393, origin = '1970-01-01')), unemp_17393 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17424, origin = '1970-01-01')), unemp_17424 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17454, origin = '1970-01-01')), unemp_17454 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17485, origin = '1970-01-01')), unemp_17485 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17515, origin = '1970-01-01')), unemp_17515 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17546, origin = '1970-01-01')), unemp_17546 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17577, origin = '1970-01-01')), unemp_17577 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17605, origin = '1970-01-01')), unemp_17605 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17636, origin = '1970-01-01')), unemp_17636 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17666, origin = '1970-01-01')), unemp_17666 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17697, origin = '1970-01-01')), unemp_17697 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17727, origin = '1970-01-01')), unemp_17727 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17758, origin = '1970-01-01')), unemp_17758 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17789, origin = '1970-01-01')), unemp_17789 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17819, origin = '1970-01-01')), unemp_17819 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17850, origin = '1970-01-01')), unemp_17850 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17880, origin = '1970-01-01')), unemp_17880 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17911, origin = '1970-01-01')), unemp_17911 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17942, origin = '1970-01-01')), unemp_17942 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(17970, origin = '1970-01-01')), unemp_17970 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18001, origin = '1970-01-01')), unemp_18001 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18031, origin = '1970-01-01')), unemp_18031 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18062, origin = '1970-01-01')), unemp_18062 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18092, origin = '1970-01-01')), unemp_18092 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18123, origin = '1970-01-01')), unemp_18123 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18154, origin = '1970-01-01')), unemp_18154 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18184, origin = '1970-01-01')), unemp_18184 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18215, origin = '1970-01-01')), unemp_18215 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18245, origin = '1970-01-01')), unemp_18245 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18276, origin = '1970-01-01')), unemp_18276 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date(18307, origin = '1970-01-01')), unemp_18307 = unemp_rate)
  
#################

all_t0_gdp_mort <-  all_t0_predictors %>% 
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

all_t0_final <- all_t0_gdp_mort  %>%
  generate_weights() %>%
  generate_control()

all_t0_final %>% plot_trends()
all_t0_final %>% plot_weights()
all_t0_final %>% plot_placebos()
all_t0_final %>% plot_differences()
all_t0_final %>% plot_mspe_ratio()
all_t0_final %>% grab_signficance()

################################################################################################################
#########(2) The  first  three-fourths  of  the  pre-treatment  outcome  values ################################
############################################### ################################################################
n_dates <- pre_intervention_dates %>% length()

three_quarters_dates <- pre_intervention_dates %>% head(n = round(n_dates*0.75))

three_quarters_dates %>% head
three_quarters_dates %>% median
three_quarters_dates %>% tail
three_quarters_dates %>% length

for(date in three_quarters_dates){
  print(date)
  
  temp <- paste0(.GlobalEnv$temp, 
                 "%>% generate_predictor(time_window = as.numeric(as.Date(", 
                 date, ", origin = '1970-01-01')), unemp_",date,"= unemp_rate)")
  
  .GlobalEnv$temp <-temp
}

############################
three_quarters_predictors <- all_jrs_mort_out %>% 
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')), unemp_13528 = unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')), unemp_13559= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')), unemp_13587= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')), unemp_13618= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')), unemp_13648= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')), unemp_13679= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')), unemp_13528= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')), unemp_13559= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')), unemp_13587= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')), unemp_13618= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')), unemp_13648= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')), unemp_13679= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13709, origin = '1970-01-01')), unemp_13709= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13740, origin = '1970-01-01')), unemp_13740= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13771, origin = '1970-01-01')), unemp_13771= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13801, origin = '1970-01-01')), unemp_13801= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13832, origin = '1970-01-01')), unemp_13832= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13862, origin = '1970-01-01')), unemp_13862= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13893, origin = '1970-01-01')), unemp_13893= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13924, origin = '1970-01-01')), unemp_13924= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13953, origin = '1970-01-01')), unemp_13953= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13984, origin = '1970-01-01')), unemp_13984= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14014, origin = '1970-01-01')), unemp_14014= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14045, origin = '1970-01-01')), unemp_14045= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14075, origin = '1970-01-01')), unemp_14075= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14106, origin = '1970-01-01')), unemp_14106= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14137, origin = '1970-01-01')), unemp_14137= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14167, origin = '1970-01-01')), unemp_14167= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14198, origin = '1970-01-01')), unemp_14198= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14228, origin = '1970-01-01')), unemp_14228= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14259, origin = '1970-01-01')), unemp_14259= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14290, origin = '1970-01-01')), unemp_14290= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14318, origin = '1970-01-01')), unemp_14318= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14349, origin = '1970-01-01')), unemp_14349= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14379, origin = '1970-01-01')), unemp_14379= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14410, origin = '1970-01-01')), unemp_14410= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14440, origin = '1970-01-01')), unemp_14440= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14471, origin = '1970-01-01')), unemp_14471= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14502, origin = '1970-01-01')), unemp_14502= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14532, origin = '1970-01-01')), unemp_14532= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14563, origin = '1970-01-01')), unemp_14563= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14593, origin = '1970-01-01')), unemp_14593= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14624, origin = '1970-01-01')), unemp_14624= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14655, origin = '1970-01-01')), unemp_14655= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14683, origin = '1970-01-01')), unemp_14683= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14714, origin = '1970-01-01')), unemp_14714= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14744, origin = '1970-01-01')), unemp_14744= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14775, origin = '1970-01-01')), unemp_14775= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14805, origin = '1970-01-01')), unemp_14805= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14836, origin = '1970-01-01')), unemp_14836= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14867, origin = '1970-01-01')), unemp_14867= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14897, origin = '1970-01-01')), unemp_14897= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14928, origin = '1970-01-01')), unemp_14928= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14958, origin = '1970-01-01')), unemp_14958= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14989, origin = '1970-01-01')), unemp_14989= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15020, origin = '1970-01-01')), unemp_15020= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15048, origin = '1970-01-01')), unemp_15048= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15079, origin = '1970-01-01')), unemp_15079= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15109, origin = '1970-01-01')), unemp_15109= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15140, origin = '1970-01-01')), unemp_15140= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15170, origin = '1970-01-01')), unemp_15170= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15201, origin = '1970-01-01')), unemp_15201= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15232, origin = '1970-01-01')), unemp_15232= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15262, origin = '1970-01-01')), unemp_15262= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15293, origin = '1970-01-01')), unemp_15293= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15323, origin = '1970-01-01')), unemp_15323= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15354, origin = '1970-01-01')), unemp_15354= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15385, origin = '1970-01-01')), unemp_15385= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15414, origin = '1970-01-01')), unemp_15414= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15445, origin = '1970-01-01')), unemp_15445= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15475, origin = '1970-01-01')), unemp_15475= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15506, origin = '1970-01-01')), unemp_15506= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15536, origin = '1970-01-01')), unemp_15536= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15567, origin = '1970-01-01')), unemp_15567= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15598, origin = '1970-01-01')), unemp_15598= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15628, origin = '1970-01-01')), unemp_15628= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15659, origin = '1970-01-01')), unemp_15659= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15689, origin = '1970-01-01')), unemp_15689= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15720, origin = '1970-01-01')), unemp_15720= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15751, origin = '1970-01-01')), unemp_15751= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15779, origin = '1970-01-01')), unemp_15779= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15810, origin = '1970-01-01')), unemp_15810= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15840, origin = '1970-01-01')), unemp_15840= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15871, origin = '1970-01-01')), unemp_15871= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15901, origin = '1970-01-01')), unemp_15901= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15932, origin = '1970-01-01')), unemp_15932= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15963, origin = '1970-01-01')), unemp_15963= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15993, origin = '1970-01-01')), unemp_15993= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16024, origin = '1970-01-01')), unemp_16024= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16054, origin = '1970-01-01')), unemp_16054= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16085, origin = '1970-01-01')), unemp_16085= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16116, origin = '1970-01-01')), unemp_16116= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16144, origin = '1970-01-01')), unemp_16144= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16175, origin = '1970-01-01')), unemp_16175= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16205, origin = '1970-01-01')), unemp_16205= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16236, origin = '1970-01-01')), unemp_16236= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16266, origin = '1970-01-01')), unemp_16266= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16297, origin = '1970-01-01')), unemp_16297= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16328, origin = '1970-01-01')), unemp_16328= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16358, origin = '1970-01-01')), unemp_16358= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16389, origin = '1970-01-01')), unemp_16389= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16419, origin = '1970-01-01')), unemp_16419= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16450, origin = '1970-01-01')), unemp_16450= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16481, origin = '1970-01-01')), unemp_16481= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16509, origin = '1970-01-01')), unemp_16509= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16540, origin = '1970-01-01')), unemp_16540= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16570, origin = '1970-01-01')), unemp_16570= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16601, origin = '1970-01-01')), unemp_16601= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16631, origin = '1970-01-01')), unemp_16631= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16662, origin = '1970-01-01')), unemp_16662= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16693, origin = '1970-01-01')), unemp_16693= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16723, origin = '1970-01-01')), unemp_16723= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16754, origin = '1970-01-01')), unemp_16754= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16784, origin = '1970-01-01')), unemp_16784= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16815, origin = '1970-01-01')), unemp_16815= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16846, origin = '1970-01-01')), unemp_16846= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16875, origin = '1970-01-01')), unemp_16875= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16906, origin = '1970-01-01')), unemp_16906= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16936, origin = '1970-01-01')), unemp_16936= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16967, origin = '1970-01-01')), unemp_16967= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16997, origin = '1970-01-01')), unemp_16997= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17028, origin = '1970-01-01')), unemp_17028= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17059, origin = '1970-01-01')), unemp_17059= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17089, origin = '1970-01-01')), unemp_17089= unemp_rate)

#############################

three_quarters_gdp_mort <- three_quarters_predictors %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

three_quarters_final <- three_quarters_gdp_mort %>% 
  generate_weights() %>%
  generate_control()


three_quarters_final %>% plot_trends()
three_quarters_final %>% grab_unit_weights()
three_quarters_final %>% plot_weights()
three_quarters_final %>% plot_placebos()
three_quarters_final %>% plot_differences()
three_quarters_final %>% plot_mspe_ratio()
three_quarters_final %>% grab_signficance()

###############################################################################
#########(3) The first half of the pre-treatment outcome values:: #############
############################################### ###############################

half_dates <- pre_intervention_dates %>% head(n = round(n_dates*0.5))

half_dates %>% head
half_dates %>% median
half_dates %>% tail
half_dates %>% length

temp <- ""

for(date in half_dates){
  print(date)
  
  temp <- paste0(.GlobalEnv$temp, 
                 "%>% generate_predictor(time_window = as.numeric(as.Date(", 
                 date, ", origin = '1970-01-01')), unemp_",date,"= unemp_rate)")
  
  .GlobalEnv$temp <-temp
}


#############################################################################
half_predictors <- all_jrs_mort_out %>% 
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')), unemp_13528= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13559, origin = '1970-01-01')), unemp_13559= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')), unemp_13587= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13618, origin = '1970-01-01')), unemp_13618= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')), unemp_13648= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13679, origin = '1970-01-01')), unemp_13679= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13709, origin = '1970-01-01')), unemp_13709= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13740, origin = '1970-01-01')), unemp_13740= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13771, origin = '1970-01-01')), unemp_13771= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13801, origin = '1970-01-01')), unemp_13801= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13832, origin = '1970-01-01')), unemp_13832= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13862, origin = '1970-01-01')), unemp_13862= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13893, origin = '1970-01-01')), unemp_13893= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(13924, origin = '1970-01-01')), unemp_13924= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13953, origin = '1970-01-01')), unemp_13953= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13984, origin = '1970-01-01')), unemp_13984= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14014, origin = '1970-01-01')), unemp_14014= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14045, origin = '1970-01-01')), unemp_14045= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14075, origin = '1970-01-01')), unemp_14075= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14106, origin = '1970-01-01')), unemp_14106= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14137, origin = '1970-01-01')), unemp_14137= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14167, origin = '1970-01-01')), unemp_14167= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14198, origin = '1970-01-01')), unemp_14198= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14228, origin = '1970-01-01')), unemp_14228= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14259, origin = '1970-01-01')), unemp_14259= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14290, origin = '1970-01-01')), unemp_14290= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14318, origin = '1970-01-01')), unemp_14318= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14349, origin = '1970-01-01')), unemp_14349= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14379, origin = '1970-01-01')), unemp_14379= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14410, origin = '1970-01-01')), unemp_14410= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14440, origin = '1970-01-01')), unemp_14440= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14471, origin = '1970-01-01')), unemp_14471= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14502, origin = '1970-01-01')), unemp_14502= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14532, origin = '1970-01-01')), unemp_14532= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14563, origin = '1970-01-01')), unemp_14563= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14593, origin = '1970-01-01')), unemp_14593= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14624, origin = '1970-01-01')), unemp_14624= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14655, origin = '1970-01-01')), unemp_14655= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14683, origin = '1970-01-01')), unemp_14683= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14714, origin = '1970-01-01')), unemp_14714= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14744, origin = '1970-01-01')), unemp_14744= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14775, origin = '1970-01-01')), unemp_14775= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14805, origin = '1970-01-01')), unemp_14805= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14836, origin = '1970-01-01')), unemp_14836= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14867, origin = '1970-01-01')), unemp_14867= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14897, origin = '1970-01-01')), unemp_14897= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14928, origin = '1970-01-01')), unemp_14928= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14958, origin = '1970-01-01')), unemp_14958= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(14989, origin = '1970-01-01')), unemp_14989= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15020, origin = '1970-01-01')), unemp_15020= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15048, origin = '1970-01-01')), unemp_15048= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15079, origin = '1970-01-01')), unemp_15079= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15109, origin = '1970-01-01')), unemp_15109= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15140, origin = '1970-01-01')), unemp_15140= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15170, origin = '1970-01-01')), unemp_15170= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15201, origin = '1970-01-01')), unemp_15201= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15232, origin = '1970-01-01')), unemp_15232= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15262, origin = '1970-01-01')), unemp_15262= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15293, origin = '1970-01-01')), unemp_15293= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15323, origin = '1970-01-01')), unemp_15323= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15354, origin = '1970-01-01')), unemp_15354= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15385, origin = '1970-01-01')), unemp_15385= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15414, origin = '1970-01-01')), unemp_15414= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15445, origin = '1970-01-01')), unemp_15445= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15475, origin = '1970-01-01')), unemp_15475= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15506, origin = '1970-01-01')), unemp_15506= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15536, origin = '1970-01-01')), unemp_15536= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15567, origin = '1970-01-01')), unemp_15567= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15598, origin = '1970-01-01')), unemp_15598= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15628, origin = '1970-01-01')), unemp_15628= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15659, origin = '1970-01-01')), unemp_15659= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15689, origin = '1970-01-01')), unemp_15689= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15720, origin = '1970-01-01')), unemp_15720= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15751, origin = '1970-01-01')), unemp_15751= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15779, origin = '1970-01-01')), unemp_15779= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15810, origin = '1970-01-01')), unemp_15810= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15840, origin = '1970-01-01')), unemp_15840= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15871, origin = '1970-01-01')), unemp_15871= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(15901, origin = '1970-01-01')), unemp_15901= unemp_rate)

#############################################################################

half_gdp_mort <- half_predictors %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

half_final <- half_gdp_mort %>% 
  generate_weights() %>%
  generate_control()


half_final %>% plot_trends()
half_final %>% grab_unit_weights()
half_final %>% plot_weights()
half_final %>% plot_placebos()
half_final %>% plot_differences()
half_final %>% plot_mspe_ratio()
half_final %>% grab_signficance()


###############################################################################
#########(4) Odd pre-treatment outcome values:: ###############################
############################################### ###############################

odd_dates <- pre_intervention_dates[c(TRUE, FALSE)]

odd_dates %>% head
odd_dates %>% median
odd_dates %>% tail
odd_dates %>% length

temp <- ""

for(date in odd_dates){
  print(date)
  
  temp <- paste0(.GlobalEnv$temp, 
                 "%>% generate_predictor(time_window = as.numeric(as.Date(", 
                 date, ", origin = '1970-01-01')), unemp_",date,"= unemp_rate)")
  
  .GlobalEnv$temp <-temp
}

###############################################################################
odd_predictors <- all_jrs_mort_out %>% 
  generate_predictor(time_window = as.numeric(as.Date(13528, origin = '1970-01-01')), unemp_13528= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13587, origin = '1970-01-01')), unemp_13587= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13648, origin = '1970-01-01')), unemp_13648= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13709, origin = '1970-01-01')), unemp_13709= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(13771, origin = '1970-01-01')), unemp_13771= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13832, origin = '1970-01-01')), unemp_13832= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(13893, origin = '1970-01-01')), unemp_13893= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(13953, origin = '1970-01-01')), unemp_13953= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14014, origin = '1970-01-01')), unemp_14014= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14075, origin = '1970-01-01')), unemp_14075= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14137, origin = '1970-01-01')), unemp_14137= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14198, origin = '1970-01-01')), unemp_14198= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14259, origin = '1970-01-01')), unemp_14259= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14318, origin = '1970-01-01')), unemp_14318= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14379, origin = '1970-01-01')), unemp_14379= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14440, origin = '1970-01-01')), unemp_14440= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14502, origin = '1970-01-01')), unemp_14502= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14563, origin = '1970-01-01')), unemp_14563= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14624, origin = '1970-01-01')), unemp_14624= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14683, origin = '1970-01-01')), unemp_14683= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14744, origin = '1970-01-01')), unemp_14744= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14805, origin = '1970-01-01')), unemp_14805= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14867, origin = '1970-01-01')), unemp_14867= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14928, origin = '1970-01-01')), unemp_14928= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(14989, origin = '1970-01-01')), unemp_14989= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15048, origin = '1970-01-01')), unemp_15048= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15109, origin = '1970-01-01')), unemp_15109= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15170, origin = '1970-01-01')), unemp_15170= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15232, origin = '1970-01-01')), unemp_15232= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15293, origin = '1970-01-01')), unemp_15293= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15354, origin = '1970-01-01')), unemp_15354= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15414, origin = '1970-01-01')), unemp_15414= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15475, origin = '1970-01-01')), unemp_15475= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15536, origin = '1970-01-01')), unemp_15536= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15598, origin = '1970-01-01')), unemp_15598= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15659, origin = '1970-01-01')), unemp_15659= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15720, origin = '1970-01-01')), unemp_15720= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15779, origin = '1970-01-01')), unemp_15779= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15840, origin = '1970-01-01')), unemp_15840= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15901, origin = '1970-01-01')), unemp_15901= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(15963, origin = '1970-01-01')), unemp_15963= unemp_rate)%>%
  generate_predictor(time_window = as.numeric(as.Date(16024, origin = '1970-01-01')), unemp_16024= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16085, origin = '1970-01-01')), unemp_16085= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16144, origin = '1970-01-01')), unemp_16144= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16205, origin = '1970-01-01')), unemp_16205= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16266, origin = '1970-01-01')), unemp_16266= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16328, origin = '1970-01-01')), unemp_16328= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16389, origin = '1970-01-01')), unemp_16389= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16450, origin = '1970-01-01')), unemp_16450= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16509, origin = '1970-01-01')), unemp_16509= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16570, origin = '1970-01-01')), unemp_16570= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16631, origin = '1970-01-01')), unemp_16631= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16693, origin = '1970-01-01')), unemp_16693= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16754, origin = '1970-01-01')), unemp_16754= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16815, origin = '1970-01-01')), unemp_16815= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16875, origin = '1970-01-01')), unemp_16875= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16936, origin = '1970-01-01')), unemp_16936= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(16997, origin = '1970-01-01')), unemp_16997= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17059, origin = '1970-01-01')), unemp_17059= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17120, origin = '1970-01-01')), unemp_17120= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17181, origin = '1970-01-01')), unemp_17181= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17240, origin = '1970-01-01')), unemp_17240= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17301, origin = '1970-01-01')), unemp_17301= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17362, origin = '1970-01-01')), unemp_17362= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17424, origin = '1970-01-01')), unemp_17424= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17485, origin = '1970-01-01')), unemp_17485= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17546, origin = '1970-01-01')), unemp_17546= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17605, origin = '1970-01-01')), unemp_17605= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17666, origin = '1970-01-01')), unemp_17666= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17727, origin = '1970-01-01')), unemp_17727= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17789, origin = '1970-01-01')), unemp_17789= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17850, origin = '1970-01-01')), unemp_17850= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17911, origin = '1970-01-01')), unemp_17911= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(17970, origin = '1970-01-01')), unemp_17970= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(18031, origin = '1970-01-01')), unemp_18031= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(18092, origin = '1970-01-01')), unemp_18092= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(18154, origin = '1970-01-01')), unemp_18154= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(18215, origin = '1970-01-01')), unemp_18215= unemp_rate)%>% 
  generate_predictor(time_window = as.numeric(as.Date(18276, origin = '1970-01-01')), unemp_18276= unemp_rate)

###############################################################################

odd_gdp_mort <- odd_predictors %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

odd_final <- odd_gdp_mort %>% 
  generate_weights() %>%
  generate_control()


odd_final %>% plot_trends()
odd_final %>% grab_unit_weights()
odd_final %>% plot_weights()
odd_final %>% plot_placebos()
odd_final %>% plot_differences()
odd_final %>% plot_mspe_ratio()
odd_final %>% grab_signficance()


###############################################################################
#########(5) Even pre-treatment outcome values:: ##############################
############################################### ###############################

even_dates <- pre_intervention_dates[c(FALSE, TRUE)]

even_dates %>% head
even_dates %>% median
even_dates %>% tail
even_dates %>% length

temp <- ""

for(date in even_dates){
  print(date)
  
  temp <- paste0(.GlobalEnv$temp, 
                 "%>% generate_predictor(time_window = as.numeric(as.Date(", 
                 date, ", origin = '1970-01-01')), unemp_",date,"= unemp_rate)")
  
  .GlobalEnv$temp <-temp
}

###############################################################################

even_predictors <- all_jrs_mort_out %>% 
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
  generate_predictor(time_window = as.numeric(as.Date(18307, origin = '1970-01-01')), unemp_18307= unemp_rate) 

###############################################################################


even_gdp_mort <- even_predictors %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

even_final <- even_gdp_mort %>% 
  generate_weights() %>%
  generate_control()


even_final %>% plot_trends()
even_final %>% grab_unit_weights()
even_final %>% plot_weights()
even_final %>% plot_placebos()
even_final %>% plot_differences()
even_final %>% plot_mspe_ratio()
even_final %>% grab_signficance()

###############################################################################
#########(6)Pre-treatment outcome mean ########################################
###############################################################################

mean_predictors <- all_jrs_mort_out %>%
  generate_predictor(time_window = as.numeric(as.Date("2007-01-15")):as.numeric(as.Date("2020-02-15")),
                     unemp = mean(unemp_rate, na.rm = T)) 

mean_gdp_mort <- mean_predictors %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

mean_final <- mean_gdp_mort %>% 
  generate_weights() %>%
  generate_control()


mean_final %>% plot_trends()
mean_final %>% grab_unit_weights()
mean_final %>% plot_weights()
mean_final %>% plot_placebos()
mean_final %>% plot_differences()
mean_final %>% plot_mspe_ratio()
mean_final %>% grab_signficance()

###############################################################################
######### (7) Three outcome values (the first one, the middle one, and the last one) ###################################
##############################################################################

pre_intervention_dates %>% head(n = 1L)
pre_intervention_dates %>% median #"2013-07-30"
pre_intervention_dates %>% tail(n = 1L)


three_dates_out <- all_jrs_mort_out %>%
  generate_predictor(time_window = as.numeric(as.Date("2007-01-15")),
                     unemp_2007 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2013-07-15")),
                     unemp_2013 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-02-15")),
                     unemp_2020 = unemp_rate) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     gdp = mean(gdp_pct_change, na.rm = T)) %>%
  generate_predictor(time_window = as.numeric(as.Date("2020-03-15")):as.numeric(as.Date("2021-12-15")),
                     excess_mort = mean(excesspc, na.rm = T))

three_dates_final <- three_dates_out %>% 
  generate_weights() %>%
  generate_control()


three_dates_final %>% plot_trends()
three_dates_final %>% grab_unit_weights()
three_dates_final %>% plot_weights()
three_dates_final %>% plot_placebos()
three_dates_final %>% plot_differences()
three_dates_final %>% plot_mspe_ratio()
three_dates_final %>% grab_signficance()


save(list = ls(pattern = "final"), file = here::here("data","SC outputs","All JRS final specs.rdata"))

rm(list = ls())
