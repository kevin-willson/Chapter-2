# Need an explanation for this script

## Used directions from: https://www.r-bloggers.com/2018/08/bootstrapping-clustered-data/
## Also used insight from: https://statisticsbyjim.com/hypothesis-testing/bootstrapping/
library(readxl)
library(dplyr)
library(tidyverse)
library(car)
library(MASS)
library(dplR)
library(data.table)
library(reshape2)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(agricolae)
library(rstatix)

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

Fire_trees <- read.csv("recurrent_fire_tree_growth_std_mne_6-6-24.csv")

Control_trees <- read_excel("Control_trees_std_mne_growth_6-6-24.xlsx") 
Control_trees <- Control_trees[,-c(12,13)]

Control_trees$Year <- as.numeric(Control_trees$Year)

## Select growth periods within 2 years of a fire event
fire_events_growth <- Fire_trees %>% 
  filter((Year <= Fire_year_1 +2 & Year >= Fire_year_1 - 2)|
           (Year <= Fire_year_2 +2 & Year >= Fire_year_2 - 2)|
           (Year <= Fire_year_3 +2 & Year >= Fire_year_3 - 2)|
           (Year <= Fire_year_4 +2 & Year >= Fire_year_4 - 2)|
           (Year <= Fire_year_5 +2 & Year >= Fire_year_5 - 2)) %>% 
  mutate(Fire_number = case_when(Year <= Fire_year_1 +2 & Year >= Fire_year_1 - 2 ~ "First",
                                 Year <= Fire_year_2 +2 & Year >= Fire_year_2 - 2 ~ "Second",
                                 Year <= Fire_year_3 +2 & Year >= Fire_year_3 - 2 ~ "Third",
                                 Year <= Fire_year_4 +2 & Year >= Fire_year_4 - 2 ~ "Fourth",
                                 Year <= Fire_year_5 +2 & Year >= Fire_year_5 - 2 ~ "Fifth")) %>% 
  group_by(Fire, Patch, Transect, Tree, Fire_number) %>% 
  mutate(growth_zscore = (value-mean(value))/sd(value)) %>% 
  mutate(year_since_fire = case_when(Fire_number == "First" ~ Year - Fire_year_1,
                                     Fire_number == "Second" ~ Year - Fire_year_2,
                                     Fire_number == "Third" ~ Year - Fire_year_3,
                                     Fire_number == "Fourth" ~ Year - Fire_year_4,
                                     Fire_number == "Fifth" ~ Year - Fire_year_5)) %>% 
  ungroup() %>% 
  mutate(first_sub = ifelse(Fire_number == "First", "First", "Subsequent"),
         FPTTF = paste0(Fire, Patch, Transect, Tree, Fire_number),
         FPTT = paste0(Fire, Patch, Transect, Tree))

years_first_fire <- unique(fire_events_growth[fire_events_growth$first_sub == "First",]$Year)
control_first_fire <- Control_trees[Control_trees$Year %in% years_first_fire,]

### Needed to add another set of 1995 years for the 5th first fire
control_first_fire_95 <- control_first_fire[control_first_fire$Year == 1995,]
control_first_fire_95$Year <- 1995.5

control_first_fire <- rbind(control_first_fire, control_first_fire_95)

control_first_fire <- control_first_fire %>% 
  arrange(Fire, Patch, Transect, Tree, Year) %>% 
  mutate(Num_fires = 0) %>% 
  group_by(Fire, Patch, Transect, Tree) %>% 
  mutate(Fire_number = case_when(Year <= 1952 ~ "First", Year >= 1977 & Year <= 1981 ~ "Second",
                                 Year >= 1983 & Year <= 1987 ~ "Third", Year >= 1991 & Year <= 1995 ~ "Fourth",
                                 Year >= 1995.5 & Year <= 1999 ~ "Fifth",)) %>% 
  ungroup() %>% 
  mutate(Year = plyr::round_any(Year,accuracy = 1, floor)) %>% 
  group_by(Fire, Patch, Transect, Tree, Fire_number) %>% 
  mutate(growth_zscore = (value-mean(value))/sd(value)) %>% 
  mutate(year_since_fire = case_when(Fire_number == "First" ~ Year - 1950,
                                     Fire_number == "Second" ~ Year - 1979,
                                     Fire_number == "Third" ~ Year - 1985,
                                     Fire_number == "Fourth" ~ Year - 1993,
                                     Fire_number == "Fifth" ~ Year - 1997)) %>% 
  ungroup %>% 
  mutate(first_sub = "control",
         FPTTF = paste0(Fire, Patch, Transect, Tree, Fire_number),
         FPTT = paste0(Fire, Patch, Transect, Tree))

control_first_fire$year_since_fire <- ifelse(control_first_fire$year_since_fire == 1.5, 2,
                                             control_first_fire$year_since_fire)
  

first_fire_growth <- fire_events_growth[fire_events_growth$first_sub == "First",]
first_fire_growth_w_cont <- rbind(first_fire_growth, control_first_fire)

#### need to remove one NA that formed because a tree's first ring was in 1952 and so couldn't get a value
first_fire_growth_w_cont <- first_fire_growth_w_cont[!is.na(first_fire_growth_w_cont$growth_zscore),]

### Needed to get the subsequent control trees ready as well
#### There was a lot of finageling I needed to do, though, to get this ready, including a second 1995, 2004, and 2005-2008
years_sub_fire <- unique(fire_events_growth[fire_events_growth$first_sub == "Subsequent",]$Year)
control_sub_fire <- Control_trees[Control_trees$Year %in% years_sub_fire,]

control_sub_fire <- rbind(control_sub_fire, control_first_fire_95)

control_sub_fire_04 <- control_sub_fire[control_sub_fire$Year == 2004,]
control_sub_fire_04$Year <- 2004.5

control_sub_fire <- rbind(control_sub_fire, control_sub_fire_04)

control_sub_fire_05_09 <- control_sub_fire[control_sub_fire$Year >= 2005 & control_sub_fire$Year <= 2008,]
control_sub_fire_05_09$Year <- control_sub_fire_05_09$Year *-1

control_sub_fire <- rbind(control_sub_fire, control_sub_fire_05_09)

control_sub_fire <- control_sub_fire %>% 
  arrange(Fire, Patch, Transect, Tree, Year) %>% 
  mutate(Num_fires = 0) %>% 
  group_by(Fire, Patch, Transect, Tree) %>% 
  mutate(Fire_number = case_when(Year >= 1983 & Year <= 1987 ~ 1985, Year >= 1991 & Year <= 1995 ~ 1993,
                                 Year >= 1995.5 & Year <= 1999 ~ 1997,Year >= 2000 & Year <= 2004 ~ 2002,
                                 Year >= 2004.5 & Year <= 2008 ~ 2006, Year >= -2008 & Year <= -2005 ~ 2007,
                                 Year == 2009 ~ 2007, Year >= 2010 & Year <= 2014 ~ 2012)) %>% 
  ungroup() %>% 
  mutate(Year = ifelse(Year < 0, Year*-1, Year)) %>% 
  mutate(Year = plyr::round_any(Year,accuracy = 1, floor)) %>% 
  group_by(Fire, Patch, Transect, Tree, Fire_number) %>% 
  mutate(growth_zscore = (value-mean(value))/sd(value)) %>% 
  mutate(year_since_fire = Year - Fire_number) %>% 
  ungroup %>% 
  mutate(first_sub = "control",
         FPTTF = paste0(Fire, Patch, Transect, Tree, Fire_number),
         FPTT = paste0(Fire, Patch, Transect, Tree))

sub_fire_growth <- fire_events_growth[fire_events_growth$first_sub == "Subsequent",]

sub_fire_growth$Fire_number <- case_when(sub_fire_growth$Fire_number == "Second" ~ sub_fire_growth$Fire_year_2,
                                         sub_fire_growth$Fire_number == "Third" ~ sub_fire_growth$Fire_year_3,
                                         sub_fire_growth$Fire_number == "Fourth" ~ sub_fire_growth$Fire_year_4,
                                         sub_fire_growth$Fire_number == "Fifth" ~ sub_fire_growth$Fire_year_5)

sub_fire_growth_w_cont <- rbind(sub_fire_growth, control_sub_fire)

### Now that data is in shape, get means and then get things ready for the bootstrapping procedure
first_fire <- first_fire_growth_w_cont %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(mean_growth = mean(value), #growth_zscore
            se_growth = sd(value)/sqrt(n())) # growth_zscore

sub_fire <- sub_fire_growth_w_cont %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(mean_growth = mean(value),
            se_growth = sd(value)/sqrt(n()))

first_fire$first_sub <- factor(first_fire$first_sub, levels = c("First", "control"))

sub_fire$first_sub <- factor(sub_fire$first_sub, levels = c("Subsequent", "control"))

## Bootstrap time
### First fire
first_fire_growth_w_cont$first_sub <- factor(first_fire_growth_w_cont$first_sub, levels = c("First", "control"))

D <- first_fire_growth_w_cont %>% ungroup() %>% nest(data = -FPTTF)

library(rsample)

bs <- bootstraps(D, times = 1000)

#### Lets make sure it is doing what we think its doing
as.tibble(bs$splits[[1]])

bs_AvgMeasure <- map(bs$splits, ~as.tibble(.) %>% unnest %>% group_by(year_since_fire, first_sub) %>% 
                       summarize(AvgMeasure = mean(value))) %>% 
  bind_rows(.id = 'boots')


bs_AvgMeasure_CI <- bs_AvgMeasure %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(lower_CI = quantile(AvgMeasure, 0.025),
            upper_CI = quantile(AvgMeasure, 0.975),
            median = quantile(AvgMeasure, 0.5))

### Significantly lower all years after fire
#### I use the mean value of growth by year instead of the median from the bootstrap, but it funcitonally ends up being the same value
first_gg <- ggplot() +  #
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.25) +
  geom_errorbar(data = bs_AvgMeasure_CI, aes(x = year_since_fire, ymin = lower_CI, ymax = upper_CI, color = first_sub), width = .25,
              size = 2.25, show.legend = F) +
  geom_point(data = bs_AvgMeasure_CI, aes(x = year_since_fire, y = median, color = first_sub), size = 9) +
  # geom_line(data = bs_AvgMeasure_CI, aes(x = year_since_fire, y = median, color = first_sub), size = 1.25) +
  scale_color_manual(values = c("firebrick1", "skyblue"), labels = c("First-entry\nfire", "Control")) +
  # scale_fill_manual(values = c("firebrick1", "skyblue")) +
  scale_y_continuous(breaks = seq(0.6, 1.4, 0.2), limits = c(0.55,1.375)) +
  labs(x = "Years since fire", y = "Ring width index (95% CI)", color = NULL) +
  ggtitle(expression("First-entry fire")) + #
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=40),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1))

# ggplot(data = first_fire, aes(x = year_since_fire, y = mean_growth, color = first_sub, label = plyr::round_any(mean_growth, accuracy = 0.01)), size = 1.25) +
#   geom_hline(yintercept = 1, linetype = "dashed", size = 1.25) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = c("violetred3", "skyblue"), labels = c("First fire", "Control")) +
#   scale_fill_manual(values = c("firebrick1", "skyblue")) +
#   geom_label(size = 6) +
#   labs(x = "Years since fire", y = "RWI (95% CI)", color = NULL) +
#   ggtitle(expression("RWI trends - first fire")) +
#   theme(plot.title = element_text(hjust = 0),
#         panel.background = element_rect(fill = 'white'),
#         panel.border = element_rect(colour = "black", fill=NA, size=2),
#         text = element_text(size=28),
#         axis.ticks.length=unit(.25, "cm"),
#         axis.ticks = element_line(colour = "black", size = 1))

### Subsequent fires
sub_fire_growth_w_cont$first_sub <- factor(sub_fire_growth_w_cont$first_sub, levels = c("Subsequent", "control"))

D_sub <- sub_fire_growth_w_cont %>% ungroup() %>% nest(data = -FPTTF)

bs_sub <- bootstraps(D_sub, times = 1000)

bs_AvgMeasure_sub <- map(bs_sub$splits, ~as.tibble(.) %>% unnest %>% group_by(year_since_fire, first_sub) %>% 
                       summarize(AvgMeasure = mean(value))) %>% 
  bind_rows(.id = 'boots')


bs_AvgMeasure_CI_sub <- bs_AvgMeasure_sub %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(lower_CI = quantile(AvgMeasure, 0.025),
            upper_CI = quantile(AvgMeasure, 0.975),
            median = quantile(AvgMeasure, 0.5))

bs_AvgMeasure_CI_sub$first_sub <- factor(bs_AvgMeasure_CI_sub$first_sub, levels = c("Subsequent", "control"))

### The general trend is the same, but year two is slightly smaller in effect 
sub_gg <- ggplot() +  #
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.25) +
  geom_errorbar(data = bs_AvgMeasure_CI_sub, aes(x = year_since_fire, ymin = lower_CI, ymax = upper_CI, color = first_sub), width = .25,
                size = 2.25, show.legend = F) +
  geom_point(data = bs_AvgMeasure_CI_sub, aes(x = year_since_fire, y = median, color = first_sub), size = 9) +
  scale_color_manual(values = c("black", "skyblue"), labels = c("Reburns", "Control")) +
  # scale_fill_manual(values = c("tan4", "skyblue")) +
  scale_y_continuous(breaks = seq(0.6, 1.4, 0.2), limits = c(0.55,1.375)) +
  labs(x = "Years since fire", y = "Ring width index (95% CI)", color = NULL) +
  ggtitle(expression("Reburns")) + #
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=40),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1))

# ggplot(data = sub_fire, aes(x = year_since_fire, y = mean_growth, color = first_sub, label = plyr::round_any(mean_growth, accuracy = 0.01)), size = 1.25) +
#   geom_hline(yintercept = 1, linetype = "dashed", size = 1.25) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = c("darkgoldenrod", "skyblue"), labels = c("Subsequent \nfires", "Control")) +
#   scale_fill_manual(values = c("tan4", "skyblue")) +
#   geom_label(size = 6)+
#   labs(x = "Years since fire", y = "Ring width index (95% CI)", color = NULL) +
#   ggtitle(expression("RWI trends - subsequent fires")) +
#   theme(plot.title = element_text(hjust = 0),
#         panel.background = element_rect(fill = 'white'),
#         panel.border = element_rect(colour = "black", fill=NA, size=2),
#         text = element_text(size=28),
#         axis.ticks.length=unit(.25, "cm"),
#         axis.ticks = element_line(colour = "black", size = 1))

## Based on these trends, I wanted to see the difference between the control and burned trees for each and then compare them, which is accessible if I do some manipulation of the AvgMeasure dataframes
### The method of subtracting means is reasonable: https://bookdown.org/gregcox7/ims_psych/inference-two-means.html
#### (Section 14.2 and figure 14.5)
#### Also, this; https://online.stat.psu.edu/stat200/lesson/4/4.3/4.3.2
bs_AvgMeasure_diff <- map(bs$splits, ~as.tibble(.) %>% unnest %>% group_by(year_since_fire, first_sub) %>% 
                       summarize(AvgMeasure = mean(value))) %>% 
  bind_rows(.id = 'boots')

bs_AvgMeasure_diff_test <- bs_AvgMeasure_diff %>% 
  pivot_wider(names_from = "first_sub", values_from = "AvgMeasure") %>% 
  mutate(diff = First - control) %>% 
  group_by(year_since_fire) %>% 
  summarise(lower_CI = quantile(diff, 0.025),
            upper_CI = quantile(diff, 0.975),
            median = quantile(diff, 0.5)) %>% 
  mutate(First_sub = "First")

bs_AvgMeasure_diff_sub <- map(bs_sub$splits, ~as.tibble(.) %>% unnest %>% group_by(year_since_fire, first_sub) %>% 
                                summarize(AvgMeasure = mean(value))) %>% 
  bind_rows(.id = 'boots')

bs_AvgMeasure_diff_sub_test <- bs_AvgMeasure_diff_sub %>% 
  pivot_wider(names_from = "first_sub", values_from = "AvgMeasure") %>% 
  mutate(diff = Subsequent - control) %>% 
  group_by(year_since_fire) %>% 
  summarise(lower_CI = quantile(diff, 0.025),
            upper_CI = quantile(diff, 0.975),
            median = quantile(diff, 0.5)) %>% 
  mutate(First_sub = "Subsequent")

diff_fig_data <- rbind(bs_AvgMeasure_diff_test, bs_AvgMeasure_diff_sub_test)

diff_first_fire <- first_fire_growth_w_cont %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(mean_growth = mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "first_sub", values_from = "mean_growth") %>% 
  mutate(diff = First - control) %>% 
  mutate(First_sub = "First")
  

diff_sub_fire <- sub_fire_growth_w_cont %>% 
  group_by(year_since_fire, first_sub) %>% 
  summarise(mean_growth = mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "first_sub", values_from = "mean_growth") %>% 
  mutate(diff = Subsequent - control) %>% 
  mutate(First_sub = "Subsequent")

diff_both_fire_means <- rbind(diff_first_fire[,c(1,4,5)], diff_sub_fire[,c(1,4,5)])

# first_fire$first_sub <- factor(first_fire$first_sub, levels = c("First", "control"))
# 
# sub_fire$first_sub <- factor(sub_fire$first_sub, levels = c("Subsequent", "control"))


diff_gg <- ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.25) +
  # geom_ribbon(data = diff_fig_data, aes(x = year_since_fire, ymin = lower_CI, ymax = upper_CI, fill = First_sub)
  # , alpha = 0.3, show.legend = F) +
  geom_point(data = diff_both_fire_means, aes(x = year_since_fire, y = diff, color = First_sub), size = 9) +
  geom_errorbar(data = diff_fig_data, aes(x = year_since_fire, ymin = lower_CI, ymax = upper_CI, color = First_sub), width = .25,
                size = 2.25, show.legend = F) +
  scale_color_manual(values = c("firebrick1", "black"), labels = c("First-entry\nfire", "Reburns")) +
  # scale_fill_manual(values = c("firebrick1", "tan4")) +
  # scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2)) +
  labs(x = "Years since fire", y = "Difference in RWI (95% CI)", color = NULL) +
  ggtitle(expression("Growth response")) + #expression("Difference in RWI from control by year")
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=40),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1))

ggarrange(first_gg, sub_gg, diff_gg, align = "hv", labels = c("a)", "b)", "c)"), ncol = 3, common.legend = T, legend = "right", #"auto"
          font.label = list(size = 40))



### Results section calculations
#### percent change in growth among burned trees from year before to year of first fire
(bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "First" & bs_AvgMeasure_CI$year_since_fire == 0, "median"] -
  bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "First" & bs_AvgMeasure_CI$year_since_fire == -1, "median"])/
  bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "First" & bs_AvgMeasure_CI$year_since_fire == -1, "median"] *100

#### percent change in growth among control trees from year before to year of first fire
(bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "control" & bs_AvgMeasure_CI$year_since_fire == 0, "median"] -
    bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "control" & bs_AvgMeasure_CI$year_since_fire == -1, "median"])/
  bs_AvgMeasure_CI[bs_AvgMeasure_CI$first_sub == "control" & bs_AvgMeasure_CI$year_since_fire == -1, "median"] *100

#### percent change in growth among burned trees from year before to year of subsequent fire
(bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "Subsequent" & bs_AvgMeasure_CI_sub$year_since_fire == 0, "median"] -
    bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "Subsequent" & bs_AvgMeasure_CI_sub$year_since_fire == -1, "median"])/
  bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "Subsequent" & bs_AvgMeasure_CI_sub$year_since_fire == -1, "median"] *100

#### percent change in growth among control trees from year before to year of subsequent fire
(bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "control" & bs_AvgMeasure_CI_sub$year_since_fire == 0, "median"] -
    bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "control" & bs_AvgMeasure_CI_sub$year_since_fire == -1, "median"])/
  bs_AvgMeasure_CI_sub[bs_AvgMeasure_CI_sub$first_sub == "control" & bs_AvgMeasure_CI_sub$year_since_fire == -1, "median"] *100

#### Differences between control and burned trees the year after fire
min(diff_first_fire$diff)

min(diff_sub_fire$diff)




