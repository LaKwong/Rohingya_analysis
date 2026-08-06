################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Stove use and uptake in host households
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))

file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")
survey_data <- read_rds(file_in_1)

 survey_data %>% select(timepoint, SubmissionDate, hh_id_short) %>% pivot_wider(names_from = timepoint, values_from = SubmissionDate) %>% 
 	select(baseline, midline, endline)

### Count of hh receiving LPG in last 30 days
survey_data %>% 
	count(fuel_30_receive_lpg, fuel_ever_receive_lpg, timepoint) %>% 
	pivot_wider(names_from = timepoint, values_from = n)


### Count of hh buying LPG in last 30 days
survey_data %>% 
	count(fuel_30_buy_lpg, fuel_ever_receive_lpg, timepoint) %>% 
	pivot_wider(names_from = timepoint, values_from = n)


### Plot of fuel use amongst host houses 
survey_data %>% 
	select(fuel_30_receive_lpg, fuel_30_buy_lpg, fuel_30_buy_wood, fuel_30_collect_wood, fuel_30_gather_scraps, timepoint) %>% 
	mutate(timepoint_num = 
				 	case_when(
				 		timepoint == "baseline" ~ 1, 
				 		timepoint == "midline" ~ 2,
				 		timepoint == "endline" ~ 3,
	)) %>% 
	pivot_longer(cols = starts_with("fuel_30"), names_to = "fuel_type", values_to = "fuel_use_30") %>% 
	group_by(timepoint_num, fuel_type) %>% 
	summarise(
		number_used = sum(fuel_use_30, na.rm = TRUE), 
		percent_used = number_used / 115 * 100) %>% 
	ggplot(aes(x = timepoint_num, y = percent_used, color = fuel_type)) + 
	geom_point() +
	geom_line() + 
	scale_color_brewer(palette = "Dark2", labels = c("Buy LPG", "Buy Wood", "Collect Wood", "Collect Sticks", "Receive LPG")) + 
	scale_color_viridis_d( labels = c("Buy LPG", "Buy Wood", "Collect Wood", "Collect Sticks", "Receive LPG")) + 
	scale_x_continuous(breaks = 1:3, labels=c("Baseline","Midline","Endline")) +
	#theme(axis.title.y = scales::percent()) +
	theme_bw() +
	labs(
		title = "Host Community Cooking Fuel Usage", 
		y = "Percent of Households", 
		x = ""
			 )
	

## At baseline no one had received fuel 
## at midline nearly all hh were receiving fuel 
## at endline 45 still bought lpg (39%) 

45/115


#### Dif in dif of household fuel procurement between baseline and endline 

#Cannot do dif in dif with host data 

#Can do chi square to tell the difference 

#Buy LPG 
prop.test(x = c(0, 45), 
					n = c(115, 115), 
					alternative = "less")
# p = 1.301e-13

#Buy wood 
prop.test(x = c(89, 66), 
					n = c(115, 115), 
					alternative = "greater")
# p = .001
	
#Gather scraps 
prop.test(x = c(104, 91), 
					n = c(115, 115), 
					alternative = "greater")
# p = .001

