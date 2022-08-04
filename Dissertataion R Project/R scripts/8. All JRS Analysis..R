library(tidyverse)
library(tidysynth)
library(xtable)

rm(list = ls())

load(here::here("data","SC outputs","All JRS final specs.rdata"))

options(scipen=999)

all_weights <- all_t0_final %>% 
  grab_unit_weights() %>%
  rename(all_t0 = weight) %>%
  merge(three_quarters_final %>% grab_unit_weights()) %>%
  rename(three_quarters = weight) %>%
  merge(half_final %>% grab_unit_weights()) %>%
  rename(half = weight) %>%
  merge(odd_final %>% grab_unit_weights()) %>%
  rename(odd = weight) %>% 
  merge(even_final %>% grab_unit_weights()) %>%
  rename(even = weight) %>% 
  merge(mean_final %>% grab_unit_weights()) %>%
  rename(mean = weight) %>% 
  merge(three_dates_final %>% grab_unit_weights()) %>%
  rename(three_dates = weight) %>%
  pivot_longer(-unit) %>%
  group_by(name) %>%
  mutate(rank = rank(-value)) 


all_weights %>%
  filter(value > 0.02) %>%
  ggplot(aes(y = fct_reorder(factor(unit),value), 
             x = value)) +
  geom_col() + 
  facet_wrap(~name)

#########################################################################
all_significance <-  all_t0_final %>% 
  grab_signficance() %>%
  select(-contains("mspe")) %>%
  rename_with(.cols = -c(unit_name,type),
              .fn = ~paste0("all_t0.",.x)) %>%
  merge(three_quarters_final %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("three_quarters.",.x))) %>%
  merge(half_final  %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("half.",.x))) %>%
  merge(odd_final  %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("odd.",.x))) %>%
  merge(even_final  %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("even.",.x))) %>%
  merge(mean_final  %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("mean.",.x))) %>%
  merge(three_dates_final  %>%
          grab_signficance() %>%
          select(-contains("mspe")) %>%
          rename_with(.cols = -c(unit_name,type),
                      .fn = ~paste0("three_dates.",.x))) %>%
  pivot_longer(-c(unit_name,type),
               names_to = c("weights","name"),
               names_sep = "\\.") %>%
  pivot_wider()


all_significance %>%
  ggplot(aes(x = abs(z_score),
             y = fct_reorder(factor(unit_name),-rank))) +
  geom_col()+
  facet_wrap(~weights) +
  geom_col(data = all_significance %>% filter(type == "Treated"),
           fill = "red")

all_t0_final %>% 
  plot_placebos(prune = F) +
  labs(x = "",
       y = "Unemployment rate",
       title = "Difference of each 'OECD country' in the donor pool") +
  scale_x_continuous(breaks = c(13879,14610,15340,16071,16801,17532,18262,18993),
                     labels = c("01-01-2008",
                                "01-01-2010",
                                "01-01-2012",
                                "01-01-2014",
                                "01-01-2016",
                                "01-01-2018",
                                "01-01-2020",
                                "01-01-2022")) + 
  theme(axis.text.x = element_text(angle  = 45,
                                   vjust = 1,
                                   hjust = 1))

significance_tab_func <- function(data, spec){
  data %>% 
    grab_signficance() %>%
    select(-rank) %>% 
    filter(type == "Treated") %>%
    mutate(spec = spec )
}


significance_tab <- all_t0_final %>% results_tab_func(spec = "Spec. 1") %>%
  rbind(three_quarters_final %>% results_tab_func(spec = "Spec. 2")) %>%
  rbind(half_final %>% results_tab_func(spec = "Spec. 3")) %>%
  rbind(odd_final %>% results_tab_func(spec = "Spec. 4")) %>%
  rbind(even_final %>% results_tab_func(spec = "Spec. 5")) %>%
  rbind(mean_final %>% results_tab_func(spec = "Spec. 6")) %>%
  rbind(three_dates_final %>% results_tab_func(spec = "Spec. 7")) %>%
  select(-unit_name, -type) %>%
  pivot_longer(-spec) %>%
  pivot_wider(names_from = "spec")

weights_tab_func <- function(data, spec){
  data %>% 
    grab_unit_weights() %>% 
    slice_max(weight,n = 5) %>% 
    rename_with(.fn = ~gsub("weight",spec,.x))
  }

weights_tab <- all_t0_final %>% weights_tab_func(spec = "Spec. 1") %>%
  merge(three_quarters_final %>% weights_tab_func(spec = "Spec. 2"), all = T) %>%
  merge(half_final %>% weights_tab_func(spec = "Spec. 3"), all = T) %>%
  merge(odd_final %>% weights_tab_func(spec = "Spec. 4"), all = T) %>%
  merge(even_final %>% weights_tab_func(spec = "Spec. 5"), all = T) %>%
  merge(mean_final %>% weights_tab_func(spec = "Spec. 6"), all = T) %>%
  merge(three_dates_final %>% weights_tab_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum)
  

results_tab <- significance_tab %>%
  rename(unit = name) %>%
  rbind(weights_tab)

xtable(results_tab)

even_final %>% 
  plot_trends() +
  labs(x = "",
       y = "Unemployment rate",
       title = "")+
  scale_x_continuous(breaks = c(13879,14610,15340,16071,16801,17532,18262,18993),
                     labels = c("01-01-2008",
                                "01-01-2010",
                                "01-01-2012",
                                "01-01-2014",
                                "01-01-2016",
                                "01-01-2018",
                                "01-01-2020",
                                "01-01-2022"))+ 
  theme(axis.text.x = element_text(angle  = 45,
                                   vjust = 1,
                                   hjust = 1))

ggsave(here::here("Outputs","Time series output_All JRS.jpeg"), width = 15,height = 12, units = "cm")  

even_final %>% grab_synthetic_control()%>% filter(time_unit > 18000) %>% View()

#################################################
##### Placebo effects ###########################
#################################################

placebo_table_func <- function(data,spec){
  treated <- data %>%
    grab_signficance() %>%
    filter(type == "Treated") %>%
    select(unit_name, pre_mspe, post_mspe)%>%
    pivot_longer(-unit_name)
  
  # donors <- data %>%
  #   grab_signficance() %>%
  #   filter(type == "Donor") %>%
  #   #slice_min(rank,n = 5) %>%
  #   select(unit_name, post_mspe)
  
  average <- data %>%
    grab_signficance() %>%
    filter(type == "Donor") %>% 
    summarise(post_mspe = mean(post_mspe, na.rm = T),
              pre_mspe = mean(pre_mspe, na.rm = T)) %>%
    mutate(unit_name = "Average Donor") %>%
    pivot_longer(-unit_name)
  
  rbind(treated,
        #donors,
        average) %>%
    rename_with(.fn = ~gsub("value",spec,.x))
}

placebo_table <- all_t0_final %>% placebo_table_func("Spec. 1") %>% 
  merge(three_quarters_final %>% placebo_table_func(spec = "Spec. 2"), all = T) %>%
  merge(half_final %>% placebo_table_func(spec = "Spec. 3"), all = T) %>%
  merge(odd_final %>% placebo_table_func(spec = "Spec. 4"), all = T) %>%
  merge(even_final %>% placebo_table_func(spec = "Spec. 5"), all = T) %>%
  merge(mean_final %>% placebo_table_func(spec = "Spec. 6"), all = T) %>%
  merge(three_dates_final %>% placebo_table_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  mutate(unit_name = factor(unit_name) %>%
           fct_reorder(-sum) %>%
           fct_relevel("USA") %>%
           fct_relevel("Average Donor",after = Inf)) %>%
  arrange(unit_name) %>%
  select(-sum)

xtable(placebo_table)
  



