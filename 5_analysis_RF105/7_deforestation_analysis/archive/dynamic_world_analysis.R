# Plots dynamic world geo data 

# Author: Chris leBoa 
# Version: 2022-12-05

# Libraries
library(tidyverse)

# Parameters
file_in <- here::here("2_data_raw/deforestation/6_mo_land_cover Change.xlsx")
file_in_hansen <- here::here("2_data_raw/deforestation/hansen_deforestation_21.csv")

#Load data 
data_dw <- readxl::read_excel(file_in)
data_hansen <- read_csv(file_in_hansen)


data_hansen %>%
	mutate(forest_loss_km = round(forest_loss_m / 10^6, 1)) %>% 
	ggplot(aes(x = year, y = forest_loss_km)) +
	geom_hline(aes(yintercept = 4.76), color = "darkgreen", linetype = "dashed") + 
	geom_hline(aes(yintercept = 8.83), color = "red", linetype = "solid") + 
	geom_hline(aes(yintercept = 5.4), color = "orange", linetype = "dotdash") + 
	geom_bar(stat='identity', fill = "darkred") +
	geom_text(aes(label = forest_loss_km), vjust = - 0.5) + 
	annotate("text", x = 2002, y = 5, label = "pre-refugee") + 
	annotate("text", x = 2002, y = 9.2, label = "before LPG") + 
	annotate("text", x = 2002, y = 5.7, label = "after LPG") + 
	theme_bw() + 
	labs(x = "Year", y = "Forest Loss in Cox's Bazar Penninsula \n (in sq. km)")

averages <- 
	tibble(time = ordered(c("pre-refugee", "before LPG", "after LPG")), 'forest loss (km^2)' = c(4.76, 8.83, 6.4)) 

level_order <- c("pre-refugee", "before LPG", "after LPG")

averages %>% 
	ggplot() +
	geom_col(aes(x = factor(time, level = level_order), y =`forest loss (km^2)`, fill = time)) +
	theme_bw() + 
	scale_fill_manual(values = c("orange", "red", "darkgreen" )) +
	annotate("text", x = "pre-refugee", y = 5, label = "4.8") + 
	annotate("text", x = "before LPG", y = 9.2, label = "8.8") + 
	annotate("text", x = "after LPG", y = 6.7, label = "6.4") + 
	labs(x = "", y = "Forest Loss in Cox's Bazar Penninsula \n (in sq. km)", title = "Annual Mean Forest Loss in Cox's Bazar Penninsula")

# Find averages 
data_hansen %>%
	mutate(forest_loss_km = round(forest_loss_m / 10^6, 1)) %>% 
	mutate(
		group = 
				 	case_when(
				 		year < 2005 ~ 1, 
				 		year >=2005 & year < 2010 ~ 2,
				 		year >= 2010 & year < 2015 ~ 3, 
				 		year < 2017 & year > 2014 ~ 4, 
				 		year == 2017 ~ 2017,
				 		year == 2018 ~ 2018,
				 		year == 2019 ~ 2019,
				 		year >= 2019 ~ 5, 
				 		TRUE ~ NA_real_
				 	)) %>% 
	group_by(group) %>% 
	summarise(
		n = n(),
		mean = mean(forest_loss_km), 
		sd = sd(forest_loss_km)
		) %>% 
	mutate(
		margin = qt(0.975,df=n-1)*sd/sqrt(n), 
		low = mean - margin, 
		high = mean + margin
	)
	


data_dw %>% 
	pivot_longer(cols = -c(.geo, date, `system:index`), names_to = "type", values_to = "pixels") %>% 
	#select(-c(.geo, `system:index`)) %>% 
	ggplot(aes(date, pixels, color = type)) +
	geom_point() +
	scale_color_discrete() +
	geom_smooth(method = "loess") + 
	labs(
		title = "Land Cover of Cox's Bazar Penninsula", 
		y = "Area (100m^2)", 
		x = "time (6 month composites) "
	)
				 	
#===============================================================================

#Code
