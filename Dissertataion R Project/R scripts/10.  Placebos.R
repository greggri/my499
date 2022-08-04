library(tidyverse)
library(tidysynth)
library(cowplot)

rm(list = ls())

load(here::here("data","SC outputs","All JRS final specs.rdata"))


even_final %>% plot_placebos() +
  labs(title = "",
       x = "",
       y = "Difference between unemployment rate in \nsynthetic and observed OECD country",
       caption = "Pruned all synthetic placebo cases with a pre-intervention RMPSE \nexceeding two times the observed unit's pre-intervention RMSPE.")+
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

ggsave(here::here("Outputs","Placebo.jpeg"), width = 15,height = 12, units = "cm")

#even_final %>% grab_loss()

even_final %>% grab_signficance()

xtable::xtable(even_final %>% grab_signficance())
