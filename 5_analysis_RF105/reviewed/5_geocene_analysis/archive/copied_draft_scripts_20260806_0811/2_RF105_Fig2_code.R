# RF105 Figure 2: This code creates the figure 2, LPG usage figure for RF105

# Author: Chris LeBoa 
# Version: 2024-08-09

# Libraries
library(tidyverse)
library(gridExtra)

# Parameters
df_events_stove_on_per_day #This dataset comes from 1_stove_use_analysis



#===============================================================================

#Code

#Create a variable for number of days from receiving lpg
df_days_receive <- 
  df_events_stove_on_per_day %>% 
  mutate(days_after_first_receiving = date - first_receive_lpg_ymd,
         before_after = if_else(days_after_first_receiving< 0, "Days Before Intervention", "Days After Receiving"), 
         before_after = factor(before_after, levels = c( "Days Before Intervention", "Days After Receiving"))) %>% 
  filter(before_after == "Days After Receiving")

#Check that the math worked for days since receive intervention 

# df_days_receive %>% 
#   filter(lpg_enrolled_and_receiving == "not yet receiving LPG through distribution program") %>% 
#   view()


#Top two rows: The number of minutes of use per day 
#For top two rows of the graph use the mean of hh that had at least least 1 minutes of use in a day  
fig2.1 <- 
  df_days_receive %>% 
  group_by(days_after_first_receiving, before_after) %>%
  summarise(biomass = mean(stove_on_min_sum_biomass), lpg = mean(stove_on_min_sum_lpg)) %>% 
  pivot_longer(cols = c(biomass, lpg), names_to = "Stove", values_to = "Average Minutes of Use") %>%
  filter(`Average Minutes of Use`>0) %>% 
  ggplot(aes(x = days_after_first_receiving, y = `Average Minutes of Use`)) +
  geom_point(aes(color = Stove)) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "top")


#Next two rows: Percent of daily use 
   #For this one I did not filter by households that were using the stove at all
   # becasue we placed monitors in hh that had a stove at all. 
   #If no data it meant that there was only 

fig2.2 <- 
  df_days_receive %>% 
  mutate(total_time = stove_on_min_sum_biomass + stove_on_min_sum_lpg, 
         pct_biomass = stove_on_min_sum_biomass/ total_time *100, pct_lpg = stove_on_min_sum_lpg/ total_time *100) %>% 
  group_by(days_after_first_receiving, before_after) %>%
  summarise(biomass = mean(pct_biomass), lpg = mean(pct_lpg)) %>% 
  pivot_longer(cols = c(biomass, lpg), names_to = "Stove", values_to = "Average Percent Use") %>% 
  ggplot(aes(x = days_after_first_receiving, y = `Average Percent Use`)) +
  geom_point(aes(color = Stove)) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "none")



fig2.2.2 <- 
  df_days_receive %>% 
  mutate(total_time = stove_on_min_sum_biomass + stove_on_min_sum_lpg, 
         pct_biomass = stove_on_min_sum_biomass/ total_time *100, pct_lpg = stove_on_min_sum_lpg/ total_time *100) %>% 
  group_by(days_after_first_receiving, before_after) %>%
  mutate(days_after_first_receiving_num = as.numeric(days_after_first_receiving)) %>%  
  filter(days_after_first_receiving_num <= 0 | n() >5) %>% 
  summarise(biomass = mean(pct_biomass), lpg = mean(pct_lpg)) %>% 
  pivot_longer(cols = c(biomass, lpg), names_to = "Stove", values_to = "Average Percent Use") %>% 
  filter(!(days_after_first_receiving < 0 & `Average Percent Use` == 0)) %>%  #Removes the 0 points from second graph
  ggplot(aes(x = days_after_first_receiving, y = `Average Percent Use`)) +
  geom_point(aes(color = Stove)) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "none")

# Last two rows: Number of households being monitored 

fig2.3 <- 
  df_days_receive %>% 
  mutate(
    biomass = if_else(!is.na(stove_on_min_sum_biomass_na), 1,0), 
    lpg = if_else(!is.na(stove_on_min_sum_lpg_na), 1, 0)
  ) %>% 
  pivot_longer(cols = c(biomass, lpg), names_to = "stove_monitored", values_to = "stove_count" ) %>% 
  group_by(days_after_first_receiving, before_after, stove_monitored) %>%
  summarise(`Number Stoves Monitored`= sum(stove_count)) %>% 
 # pivot_longer(cols = c(biomass, lpg), names_to = "Stove", values_to = "Average Percent Use") %>% 
  ggplot(aes(x = days_after_first_receiving, y = `Number Stoves Monitored`)) +
  geom_jitter(aes(color = stove_monitored)) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "none")

# Plot each on top of one another in larger figure using the geomExtra package
gA <- ggplotGrob(fig2.1)
gB <- ggplotGrob(fig2.2.2)
gC <- ggplotGrob(fig2.3)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)



grid.arrange(
  gA,
  gB,
  gC,
 # heights=c(1,2),
  widths = c(1, .2),
  nrow = 3,
  layout_matrix = rbind(c(1), c(2), c(3))
)

fig3 <- grid.arrange(gA, gB, gC)

fig3

df_days_receive


#########################################################################
# Figure 4 energy conversions 
## In this figure we convert stove usage to MJ of use for each stove type and total 
## This means multiplying the Time use by the efficiency of each stove and creating a total column for each 

# Stove efficiencies: % Thermal efficiency 
lpg_efficiency <- .67 #This is taken from UNHCR stove specifications from safe+2
biomass_efficiency <-.128

#This paper says 14% for three stone file https://pubs-acs-org.libproxy.berkeley.edu/doi/10.1021/es301693f
#we used the value for trditional stove from here https://baec.portal.gov.bd/sites/default/files/files/baec.portal.gov.bd/page/1f00cd0e_737d_4e2e_ab9f_08183800b7a2/NSA-Vol%2024-Article%201.pdf
#This article also has traditional wood fires as between 17 and 12% efficient 

#Energy Reached the pot (MJ) == (Power (KW) * Time (min)) * Thermal efficiency (%)

#Convert KWH to MJ
# 1 KWH = 3.6 MJ

conv_mJ <- 3.6

power_wood <- 6.824/conv_mJ  #KW Power of wood stove mixed start from Islam, Khanam, Rouf, and Rahaman 2012
power_lpg <- 3.4/conv_mJ #KW  Power of LPG stove taken from UNHCR specification


df_days_receive %>% 
  group_by(days_after_first_receiving, before_after, hh_id) %>%
  summarise(
    biomass = mean(stove_on_min_sum_biomass),
    lpg = mean(stove_on_min_sum_lpg), 
 #   both = biomass + lpg, 
    biomass = (biomass/60* power_wood) * biomass_efficiency, 
    lpg = (lpg/60 * power_lpg) * lpg_efficiency 
  #  both = biomass + lpg
    ) %>% 
  pivot_longer(cols = c(biomass, lpg), names_to = "Energy", values_to = "Energy Use (MJ)") %>%
  filter(`Energy Use (MJ)`>0) %>% 
  ggplot(aes(x = days_after_first_receiving, y = `Energy Use (MJ)`)) +
  geom_point(aes(color = Energy), alpha = 0.5) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "top") + 
  labs(ylab = "Energy that reached the cooking pot (MJ)")
  

df_days_receive %>% 
  group_by(days_after_first_receiving, before_after, hh_id) %>%
  summarise(
    biomass = sum(stove_on_min_sum_biomass),
    lpg = sum(stove_on_min_sum_lpg), 
    both = biomass + lpg, 
    biomass = (biomass/60* power_wood)*biomass_efficiency, 
    lpg = (lpg/60 * power_lpg)* lpg_efficiency, 
    both = biomass + lpg
  ) %>% 
  pivot_longer(cols = c(biomass, lpg, both), names_to = "Energy", values_to = "Energy Use (MJ)") %>%
  filter(`Energy Use (MJ)`>0) %>% 
  ggplot(aes(x = days_after_first_receiving, y = `Energy Use (MJ)`)) +
  geom_point(aes(color = Energy), alpha = 0.5) + 
  facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "top") + 
  labs(ylab = "Mean Daily Energy Use (MJ)")

##### Throw on boxplot of these ########



df_days_receive %>% 
  group_by(hh_id) %>%
  summarise(
    biomass = sum(stove_on_min_sum_biomass),
    lpg = sum(stove_on_min_sum_lpg), 
    both = biomass + lpg, 
    biomass = (biomass/60* power_wood)*biomass_efficiency, 
    lpg = (lpg/60 * power_lpg)* lpg_efficiency, 
    both = biomass + lpg
  ) %>% 
  pivot_longer(cols = c(biomass, lpg, both), names_to = "Energy", values_to = "Energy Use (MJ)") %>%
  filter(`Energy Use (MJ)`>0) %>% 
  ggplot(aes(x = Energy, y = `Energy Use (MJ)`)) +
  geom_boxplot(aes(color = Energy), alpha = 0.5) + 
  #facet_grid(cols = vars(before_after), scales = "free_x") + 
  theme(legend.position = "top") + 
  labs(ylab = "Energy Reached Pot (MJ)")
  
