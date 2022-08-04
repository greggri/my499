library(tidyverse)

all_schemes <- read_csv(here::here("Data","all_schemes.csv"))

for_reg <- all_indictors_mortality %>%
  filter(time_period %in% c("2019-12", "2021-06")) %>%
  select(oecd_country_codes, time_period,unemp_rate,gdp_pct_change,excesspc) %>%
  pivot_wider(names_from = time_period,
              values_from = c(unemp_rate,gdp_pct_change,excesspc)) %>%
  merge(all_schemes, 
        by = "oecd_country_codes",
        all = T) %>%
  janitor::clean_names()

lm <- lm(unemp_rate_2021_06 ~  new_improved + unemp_rate_2019_12 + gdp_pct_change_2021_06 + excesspc_2021_06 , for_reg)

summary.lm(lm)
