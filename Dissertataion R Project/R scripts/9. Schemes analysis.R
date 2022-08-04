library(tidyverse)
library(tidysynth)
library(cowplot)

rm(list = ls())

options(scipen=999)

load(here::here("Data","SC outputs","All STW final specs.rdata"))

all_stw_all_t0 <- all_t0_final
all_stw_even <- even_final
all_stw_half <- half_final
all_stw_mean <- mean_final
all_stw_odd <- odd_final
all_stw_three_dates <- three_dates_final
all_stw_three_quarters <- three_quarters_final

rm(list = ls(pattern = "final$"))

load(here::here("Data","SC outputs","New JRS final specs.rdata"))

new_jrs_all_t0 <- all_t0_final
new_jrs_even <- even_final
new_jrs_half <- half_final
new_jrs_mean <- mean_final
new_jrs_odd <- odd_final
new_jrs_three_dates <- three_dates_final
new_jrs_three_quarters <- three_quarters_final

rm(list = ls(pattern = "final$"))

load(here::here("Data","SC outputs","New STW final specs.rdata"))

new_stw_all_t0 <- all_t0_final
new_stw_even <- even_final
new_stw_half <- half_final
new_stw_mean <- mean_final
new_stw_odd <- odd_final
new_stw_three_dates <- three_dates_final
new_stw_three_quarters <- three_quarters_final

rm(list = ls(pattern = "final$"))

load(here::here("Data","SC outputs","Wage subsidy final specs.rdata"))

ws_all_t0 <- all_t0_final
ws_even <- even_final
ws_half <- half_final
ws_mean <- mean_final
ws_odd <- odd_final
ws_three_dates <- three_dates_final
ws_three_quarters <- three_quarters_final

rm(list = ls(pattern = "final$"))

weights_tab_func <- function(data, spec){
  data %>% 
    grab_unit_weights() %>% 
    slice_max(weight,n = 5) %>% 
    rename_with(.fn = ~gsub("weight",spec,.x))
}

significance_tab_func <- function(data, spec){
  data %>% 
    grab_signficance() %>%
    select(-rank) %>% 
    filter(type == "Treated") %>%
    mutate(spec = spec )
}

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


##############All STW schemes####################

all_stw_weights_tab <- all_stw_all_t0 %>% weights_tab_func(spec = "Spec. 1") %>%
  merge(all_stw_three_quarters %>% weights_tab_func(spec = "Spec. 2"), all = T) %>%
  merge(all_stw_half %>% weights_tab_func(spec = "Spec. 3"), all = T) %>%
  merge(all_stw_odd %>% weights_tab_func(spec = "Spec. 4"), all = T) %>%
  merge(all_stw_even %>% weights_tab_func(spec = "Spec. 5"), all = T) %>%
  merge(all_stw_mean %>% weights_tab_func(spec = "Spec. 6"), all = T) %>%
  merge(all_stw_three_dates %>% weights_tab_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum)

all_stw_significance_tab <- all_stw_all_t0 %>% significance_tab_func(spec = "Spec. 1") %>%
  rbind(all_stw_three_quarters %>% significance_tab_func(spec = "Spec. 2")) %>%
  rbind(all_stw_half %>% significance_tab_func(spec = "Spec. 3")) %>%
  rbind(all_stw_odd %>% significance_tab_func(spec = "Spec. 4")) %>%
  rbind(all_stw_even %>% significance_tab_func(spec = "Spec. 5")) %>%
  rbind(all_stw_mean %>% significance_tab_func(spec = "Spec. 6")) %>%
  rbind(all_stw_three_dates %>% significance_tab_func(spec = "Spec. 7")) %>%
  select(-unit_name, -type) %>%
  pivot_longer(-spec) %>%
  pivot_wider(names_from = "spec")

all_stw_results_tab <- all_stw_significance_tab %>%
  rename(unit = name) %>%
  rbind(all_stw_weights_tab)

all_stw_placebo_table <- all_stw_all_t0 %>% placebo_table_func("Spec. 1") %>% 
  merge(all_stw_three_quarters %>% placebo_table_func(spec = "Spec. 2"), all = T) %>%
  merge(all_stw_half %>% placebo_table_func(spec = "Spec. 3"), all = T) %>%
  merge(all_stw_odd %>% placebo_table_func(spec = "Spec. 4"), all = T) %>%
  merge(all_stw_even %>% placebo_table_func(spec = "Spec. 5"), all = T) %>%
  merge(all_stw_mean %>% placebo_table_func(spec = "Spec. 6"), all = T) %>%
  merge(all_stw_three_dates %>% placebo_table_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  mutate(unit_name = factor(unit_name) %>%
           fct_reorder(-sum) %>%
           fct_relevel("USA") %>%
           fct_relevel("Average Donor",after = Inf)) %>%
  arrange(unit_name) %>%
  select(-sum)

#################################################################
#############New STW schemes ####################################
#################################################################

new_stw_weights_tab <- new_stw_all_t0 %>% weights_tab_func(spec = "Spec. 1") %>%
  merge(new_stw_three_quarters %>% weights_tab_func(spec = "Spec. 2"), all = T) %>%
  merge(new_stw_half %>% weights_tab_func(spec = "Spec. 3"), all = T) %>%
  merge(new_stw_odd %>% weights_tab_func(spec = "Spec. 4"), all = T) %>%
  merge(new_stw_even %>% weights_tab_func(spec = "Spec. 5"), all = T) %>%
  merge(new_stw_mean %>% weights_tab_func(spec = "Spec. 6"), all = T) %>%
  merge(new_stw_three_dates %>% weights_tab_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum)

new_stw_significance_tab <- new_stw_all_t0 %>% significance_tab_func(spec = "Spec. 1") %>%
  rbind(new_stw_three_quarters %>% significance_tab_func(spec = "Spec. 2")) %>%
  rbind(new_stw_half %>% significance_tab_func(spec = "Spec. 3")) %>%
  rbind(new_stw_odd %>% significance_tab_func(spec = "Spec. 4")) %>%
  rbind(new_stw_even %>% significance_tab_func(spec = "Spec. 5")) %>%
  rbind(new_stw_mean %>% significance_tab_func(spec = "Spec. 6")) %>%
  rbind(new_stw_three_dates %>% significance_tab_func(spec = "Spec. 7")) %>%
  select(-unit_name, -type) %>%
  pivot_longer(-spec) %>%
  pivot_wider(names_from = "spec")

new_stw_results_tab <- new_stw_significance_tab %>%
  rename(unit = name) %>%
  rbind(new_stw_weights_tab)

new_stw_placebo_table <- new_stw_all_t0 %>% placebo_table_func("Spec. 1") %>% 
  merge(new_stw_three_quarters %>% placebo_table_func(spec = "Spec. 2"), all = T) %>%
  merge(new_stw_half %>% placebo_table_func(spec = "Spec. 3"), all = T) %>%
  merge(new_stw_odd %>% placebo_table_func(spec = "Spec. 4"), all = T) %>%
  merge(new_stw_even %>% placebo_table_func(spec = "Spec. 5"), all = T) %>%
  merge(new_stw_mean %>% placebo_table_func(spec = "Spec. 6"), all = T) %>%
  merge(new_stw_three_dates %>% placebo_table_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  mutate(unit_name = factor(unit_name) %>%
           fct_reorder(-sum) %>%
           fct_relevel("USA") %>%
           fct_relevel("Average Donor",after = Inf)) %>%
  arrange(unit_name) %>%
  select(-sum)

#################################################################
#############WS schemes ####################################
#################################################################

ws_weights_tab <- ws_all_t0 %>% weights_tab_func(spec = "Spec. 1") %>%
  merge(ws_three_quarters %>% weights_tab_func(spec = "Spec. 2"), all = T) %>%
  merge(ws_half %>% weights_tab_func(spec = "Spec. 3"), all = T) %>%
  merge(ws_odd %>% weights_tab_func(spec = "Spec. 4"), all = T) %>%
  merge(ws_even %>% weights_tab_func(spec = "Spec. 5"), all = T) %>%
  merge(ws_mean %>% weights_tab_func(spec = "Spec. 6"), all = T) %>%
  merge(ws_three_dates %>% weights_tab_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum)

ws_significance_tab <- ws_all_t0 %>% significance_tab_func(spec = "Spec. 1") %>%
  rbind(ws_three_quarters %>% significance_tab_func(spec = "Spec. 2")) %>%
  rbind(ws_half %>% significance_tab_func(spec = "Spec. 3")) %>%
  rbind(ws_odd %>% significance_tab_func(spec = "Spec. 4")) %>%
  rbind(ws_even %>% significance_tab_func(spec = "Spec. 5")) %>%
  rbind(ws_mean %>% significance_tab_func(spec = "Spec. 6")) %>%
  rbind(ws_three_dates %>% significance_tab_func(spec = "Spec. 7")) %>%
  select(-unit_name, -type) %>%
  pivot_longer(-spec) %>%
  pivot_wider(names_from = "spec")

ws_results_tab <- ws_significance_tab %>%
  rename(unit = name) %>%
  rbind(ws_weights_tab)

ws_placebo_table <- ws_all_t0 %>% placebo_table_func("Spec. 1") %>% 
  merge(ws_three_quarters %>% placebo_table_func(spec = "Spec. 2"), all = T) %>%
  merge(ws_half %>% placebo_table_func(spec = "Spec. 3"), all = T) %>%
  merge(ws_odd %>% placebo_table_func(spec = "Spec. 4"), all = T) %>%
  merge(ws_even %>% placebo_table_func(spec = "Spec. 5"), all = T) %>%
  merge(ws_mean %>% placebo_table_func(spec = "Spec. 6"), all = T) %>%
  merge(ws_three_dates %>% placebo_table_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  mutate(unit_name = factor(unit_name) %>%
           fct_reorder(-sum) %>%
           fct_relevel("USA") %>%
           fct_relevel("Average Donor",after = Inf)) %>%
  arrange(unit_name) %>%
  select(-sum)


#################################################################
#############New JRS schemes ####################################
#################################################################

new_jrs_weights_tab <- new_jrs_all_t0 %>% weights_tab_func(spec = "Spec. 1") %>%
  merge(new_jrs_three_quarters %>% weights_tab_func(spec = "Spec. 2"), all = T) %>%
  merge(new_jrs_half %>% weights_tab_func(spec = "Spec. 3"), all = T) %>%
  merge(new_jrs_odd %>% weights_tab_func(spec = "Spec. 4"), all = T) %>%
  merge(new_jrs_even %>% weights_tab_func(spec = "Spec. 5"), all = T) %>%
  merge(new_jrs_mean %>% weights_tab_func(spec = "Spec. 6"), all = T) %>%
  merge(new_jrs_three_dates %>% weights_tab_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(-sum) %>%
  filter(sum > 0.1) %>%
  select(-sum)

new_jrs_significance_tab <- new_jrs_all_t0 %>% significance_tab_func(spec = "Spec. 1") %>%
  rbind(new_jrs_three_quarters %>% significance_tab_func(spec = "Spec. 2")) %>%
  rbind(new_jrs_half %>% significance_tab_func(spec = "Spec. 3")) %>%
  rbind(new_jrs_odd %>% significance_tab_func(spec = "Spec. 4")) %>%
  rbind(new_jrs_even %>% significance_tab_func(spec = "Spec. 5")) %>%
  rbind(new_jrs_mean %>% significance_tab_func(spec = "Spec. 6")) %>%
  rbind(new_jrs_three_dates %>% significance_tab_func(spec = "Spec. 7")) %>%
  select(-unit_name, -type) %>%
  pivot_longer(-spec) %>%
  pivot_wider(names_from = "spec")

new_jrs_results_tab <- new_jrs_significance_tab %>%
  rename(unit = name) %>%
  rbind(new_jrs_weights_tab)

new_jrs_placebo_table <- new_jrs_all_t0 %>% placebo_table_func("Spec. 1") %>% 
  merge(new_jrs_three_quarters %>% placebo_table_func(spec = "Spec. 2"), all = T) %>%
  merge(new_jrs_half %>% placebo_table_func(spec = "Spec. 3"), all = T) %>%
  merge(new_jrs_odd %>% placebo_table_func(spec = "Spec. 4"), all = T) %>%
  merge(new_jrs_even %>% placebo_table_func(spec = "Spec. 5"), all = T) %>%
  merge(new_jrs_mean %>% placebo_table_func(spec = "Spec. 6"), all = T) %>%
  merge(new_jrs_three_dates %>% placebo_table_func(spec = "Spec. 7"), all = T) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  mutate(unit_name = factor(unit_name) %>%
           fct_reorder(-sum) %>%
           fct_relevel("USA") %>%
           fct_relevel("Average Donor",after = Inf)) %>%
  arrange(unit_name) %>%
  select(-sum)


##########Charts####################

new_jrs_plot <- new_jrs_even %>% plot_trends() + 
  labs(y="",
       x="",
       title = "",
       caption = "") +
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
                                   hjust = 1),
        legend.position = "none")
all_stw_plot <- all_stw_even %>% plot_trends()+ 
  labs(y="",
       x="",
       title = "",
       caption = "") +
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
                                   hjust = 1),
        legend.position = "none")
new_stw_plot <- new_stw_even %>% plot_trends()+ 
  labs(y="",
       x="",
       title = "",
       caption = "") +
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
                                   hjust = 1),
        legend.position = "none")
ws_plot <- ws_even %>% plot_trends()+ 
  labs(y="",
       x="",
       title = "",
       caption = "Dashed line denotes the time of the intervention") +
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

legend <- get_legend(ws_plot)

ws_plot <- ws_plot + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(new_jrs_plot,all_stw_plot,new_stw_plot,ws_plot, labels = c("New JRS",
                                                                                      "All STW",
                                                                                      "New STW",
                                                                                      "WS")),
                 plot_grid(legend),
                 rel_heights=c(1, 0.1),
                 ncol = 1) +
         draw_label("Unemployment rate (%)", x=  0, y=0.5, vjust= 1.5, angle=90))

ggsave(here::here("Outputs","Time series output_Type of schme.jpeg"), width = 15,height = 16, units = "cm") 

all_synthetic_versions <- all_stw_even %>% 
  grab_synthetic_control() %>% 
  rename(all_stw = synth_y) %>% 
  merge(new_stw_even %>% grab_synthetic_control()) %>%
  rename(new_stw  = synth_y) %>%
  merge(new_jrs_even %>% grab_synthetic_control()) %>%
  rename(new_jrs = synth_y) %>%
  merge(ws_even %>% grab_synthetic_control()) %>%
  rename(ws_even = synth_y) %>%
  pivot_longer(-time_unit)

all_synthetic_versions %>%
  mutate(name = factor(case_when(name == "new_jrs" ~ "New JRS",
                           name == "all_stw" ~ "All STW",
                           name == "new_stw" ~ "New STW",
                           name == "ws_even" ~ "New WS",
                           name == "real_y" ~ "Observed USA"))) %>%
  ggplot(aes(x = time_unit,
             y = value,
             group = name,
             colour = name,
             linetype = name))  +
  geom_line(size = 1, alpha = .7) +
  geom_point()+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")),color="black",linetype=2)+
  geom_line(alpha = 0.5) +
  scale_color_manual(values=c("#b41e7c","#4254a7","#18bc9c","#7ee787","grey50")) +
  scale_linetype_manual(values=c(4,4,4,4,1)) +
  labs(color="",linetype="",y="Unemployment rate (%)",x="",
                title=paste0("Time Series of the synthetic and observed unemployment rate USA"),
                caption = "Dashed line denotes the time of the intervention.") +
  theme_minimal() +
  theme(legend.position = "bottom")+
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


########################################################################
########### Results tables #############################################
########################################################################

results_tab <- new_jrs_results_tab %>% 
  select(unit,`Spec. 5`) %>%
  rename(`New JRS`= `Spec. 5`) %>%
  merge(all_stw_results_tab %>% 
          select(unit,`Spec. 5`), 
        all = T) %>%
  rename(`All STW`= `Spec. 5`) %>%
  merge(new_stw_results_tab %>% 
          select(unit,`Spec. 5`), 
        all = T) %>%
  rename(`New STW`= `Spec. 5`)%>%
  merge(ws_results_tab %>% 
          select(unit,`Spec. 5`), 
        all = T) %>%
  rename(`All WS`= `Spec. 5`) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T),
         na_count = rowSums(is.na(select(.,everything())))) %>%
  filter(na_count != 4 & sum > 0.01) %>%
  select(-c(na_count,sum)) %>%
  mutate(unit = factor(unit) %>%
           fct_reorder(-`All WS`) %>%
           fct_reorder(-`New STW`) %>%
           fct_reorder(-`All STW`) %>%
           fct_reorder(-`New JRS`) %>%
           fct_relevel("pre_mspe","post_mspe","mspe_ratio","fishers_exact_pvalue","z_score", after = Inf)) %>%
  arrange(unit)

xtable::xtable(results_tab)




