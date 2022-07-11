
library(tidyverse)
library(lubridate)
library(patchwork)
library(broom)
library(janitor)


rm(list=ls())

load("./dat.RData")
source("./code/functions.R")



# date of 1st hh activation
first_hh_activation <- dat_sev_hh %>% 
  summarise(first_hh_activation = min(time_period)) %>% 
  tibble::deframe()


# infections over time (fig 1a-c)----
# fig 1a - all infections----
weekly_counts <- dat_sev %>% 
  group_by(time_period) %>% 
  count(severity_clinic_imp) %>% 
  mutate(percent = n / sum(n)*100) 

# there's a plot function for fig 1b & 1c, but can't use for 1a bc of annotations
# (and want annotations *under* other layers)
fig_1a <- ggplot(weekly_counts, 
                 aes(x    = time_period,
                     y    = n,
                     fill = severity_clinic_imp)) +
  geom_vline( # line indicating 1st hh activation
    xintercept = ymd(first_hh_activation)
  ) +
  annotate("text", # (under/behind bar plot layer)
           x = ymd(first_hh_activation), 
           y = 18, 
           hjust = -0.05,
           label = "first households activated") +
  
  geom_col() +
  
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = colors_severity) +
  labs(x = NULL) +
  
  date_labels(20, "%b '%y") + 
  add_sampling(56) + 
  labs(title = "All infections") +
  geom_point(data = dat_sev %>% 
               filter(covid_death==1), 
             aes(x = floor_date(pcr_date, unit = "week"), 
                 y = 56),  
             color = "#49006a",
             shape = 8,
             size = 3, show.legend = F) 
fig_1a

# fig 1b&c - 1st/2nd infections in hh transmission study----
fig_1b <- dat_sev_hh %>% filter(infection_n==1) %>% # 1st infections
  plot_sev_group(time_period, n) + # plot function
  date_labels(20, "%b '%y") +
  add_sampling_notext() +
  labs(title = "First infections, from household transmission study") +
  geom_point(data = dat_sev %>% 
               filter(covid_death==1 & infection_n==1), 
             aes(x = floor_date(pcr_date, unit = "week"), 
                 y = 28),  
             color = "#49006a",
             shape = 8,
             size = 3, show.legend = F) 
fig_1b

fig_1c <- dat_sev_hh %>% filter(infection_n==2) %>% # 2nd infections
  plot_sev_group(time_period, n) + 
  date_labels(20, "%b '%y") +
  add_sampling_notext() +
  labs(title = "Second infections (1st confirmed by ELISA or PCR), from household transmission study") +
  geom_point(data = dat_sev %>% 
               filter(covid_death==1 & infection_n==2), 
             aes(x = floor_date(pcr_date, unit = "week"), 
                 y = 32),  
             color = "#49006a",
             shape = 8,
             size = 3, show.legend = F) 
fig_1c


# fig 1d - counts and percentages----
counts_1_2_sev <- dat_sev_hh %>% 
  group_by(infection_n_char) %>% 
  count(severity_clinic_imp) %>% 
  mutate(total = sum(n),
         percent = n / sum(n)*100) 

counts_1_2 <- dat_sev_hh %>% 
  count(infection_n_char) %>% 
  mutate(percent = n / sum(n)*100) 


# base plot
sev_base_plot <- ggplot(counts_1_2_sev, aes(x    = infection_n_char, 
                              fill = severity_clinic_imp)) 

fig_1d_n <- sev_base_plot + geom_col(aes(y = n)) +
  geom_text(data = counts_1_2, 
            aes(x = infection_n_char, y = n,
                label = n,
                fill = NULL), 
            nudge_y = 20) +
  labs(title = paste0("Severity counts and percentages by infection order"))
fig_1d_p <- sev_base_plot + geom_col(aes(y = percent)) 

fig_1d <- (fig_1d_n | fig_1d_p)  &
  theme_hm() &
  scale_fill_manual(values = colors_severity) &
  labs(x = NULL, color = NULL, fill = NULL)
fig_1d


# fig 1e - severity models----
nest <- dat_sev_hh %>% nest(data = everything()) # dataset used in functions

mod <- add_model("severe") %>% # function uses dataset above
  full_join(add_model("severe_moderate")) %>% 
  full_join(add_model("subclinical")) %>% 
  select(-c(data, model,term))
mod

fig_1e <- mod %>% 
  ggplot(aes(x = outcome,
             y = ratio,
             ymin = l,
             ymax = u)) +
  geom_pointrange(aes(color = outcome,
                      fill = outcome), 
                  shape = 21 , size = 1.25) +
  geom_hline(yintercept = 1) +
  scale_y_log10() +
  geom_text(aes(
    label = paste0( sprintf("%.2f", round_half_up(ratio,2))," ", CI)),
    nudge_x = -0.3, size = 3
  ) +
  scale_color_manual(values = c("#49006a","#49006a","#fde0dd")) +
  scale_fill_manual(values = c("#49006a","#ae017e","#fde0dd")) +
  theme_bw(base_size = 12) + # guides(shape = guide_legend(override.aes = list(size = 1)))
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = "risk ratio",
       color = NULL, fill = NULL,
       title = "Risk of severity level for second vs first infection")
fig_1e


# ~~~FIGURE 1: combine pieces and save~~~----
fig_1 <- fig_1a / 
  fig_1b / 
  fig_1c /
  
  ((fig_1d  | fig_1e) + plot_layout(widths = c(0.5, 0.5, 1.5))) +
  
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c('A','B','C',
                                      
                                      'D','','E'
                                      ))) &
  labs(x = NULL, color = NULL, fill = NULL) & 
  theme(legend.position = "bottom")
fig_1


# note: title in "D" is cut off - fix in Adobe Illustrator (bring to front)
ggsave(fig_1, 
       filename = "./results/fig1.pdf",
       width = 15, 
       height = 12 
       )

  

# * fig 2a - counts and percentages, by age----
counts_age_1_2_sev <- dat_sev_hh %>% 
  group_by(infection_n_char, agecat_pcr) %>% 
  count(severity_clinic_imp) %>% 
  mutate(total = sum(n),
         percent = n / sum(n)*100) 

counts_age_1_2 <- dat_sev_hh %>% 
  count(infection_n_char, agecat_pcr) %>% 
  mutate(percent = n / sum(n)*100) 



sev_base_plot_age <- ggplot(counts_age_1_2_sev, aes(x    = infection_n_char, 
                                                      fill = severity_clinic_imp)) 
fig_2a_n <- sev_base_plot_age + geom_col(aes(y = n)) +
  geom_text(data = counts_age_1_2, 
            aes(x = infection_n_char, y = n,
                label = n,
                fill = NULL), nudge_y = 10) +
  labs(title = "Severity counts and percentages by infection order",
       fill = NULL) + 
  facet_wrap(~agecat_pcr)

fig_2a_p <- sev_base_plot_age + geom_col(aes(y = percent)) + facet_wrap(~agecat_pcr) + 
  labs(fill = NULL)


fig_2a <- (fig_2a_n | fig_2a_p)  &
  theme_hm() &
  scale_fill_manual(values = colors_severity) &
  labs(x = NULL)
fig_2a

# * fig 2b - severity model, by age ----
nest_age <- dat_sev_hh %>% # dataset used in functions below
  group_by(agecat_pcr) %>% nest() %>% 
  full_join(dat_sev_hh %>% nest(data = everything()) %>% mutate(agecat_pcr = "everyone")) %>% 
  mutate(agecat_pcr = factor(agecat_pcr, levels = c("everyone", "0-9y","10-49y", "50+y"))) %>% 
  arrange(agecat_pcr)
nest_age

mod_age <- add_model_age("severe") %>%  # function uses dataset above
  full_join(add_model_age("severe_moderate")) %>% 
  full_join(add_model_age("subclinical")) %>% 
  select(-c(data, model)) 
mod_age

fig_2b <- mod_age %>% 
  filter(u != Inf) %>% 
  filter(agecat_pcr != "everyone") %>% 
  ggplot(aes(x = outcome,
             y = ratio,
             ymin = l,
             ymax = u)) +
  geom_pointrange(aes(color = outcome,
                      fill = outcome), shape = 21 , size = 1.25) +
  geom_hline(yintercept = 1) +
  scale_y_log10() +
  geom_text(aes(
    y = l,
    label = paste0( sprintf("%.2f", round_half_up(ratio,2)),
                    #" \n", 
                    " ",
                    CI)),
    #nudge_x = -0.2,
    nudge_y = -0.1
  ) +
  scale_color_manual(values = c("#49006a","#49006a","#fde0dd")) +
  scale_fill_manual(values = c("#49006a","#ae017e","#fde0dd")) +
  theme_bw(base_size = 12) + 
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = "risk ratio",
       color = NULL, fill = NULL,
       title = "Risk of severity level for secondary vs primary infection") + 
  facet_wrap(~ agecat_pcr, scales = "free_x")
fig_2b



# figure 2: combine plots----
fig_2 <- (fig_2a / fig_2b) + 
  plot_layout(widths = c(0.5, 0.5, 1.5)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c('A','','B'))) & 
  labs(x = NULL, color = NULL, fill = NULL) & 
  theme(legend.position = "bottom")
fig_2


ggsave(fig_2,
       filename = "./results/fig2.png",
       width = 12, 
       height = 9 
       )


