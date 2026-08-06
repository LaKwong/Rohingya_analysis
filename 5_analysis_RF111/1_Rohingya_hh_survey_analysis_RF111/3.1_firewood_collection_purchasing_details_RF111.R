################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Firewood collection, cost of wood in the market, reasons for using wood
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

## Chris
# data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
# 
# data_hh_member_host_filename <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")
# 
# file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files
# data_hh_member_base <- read_rds(data_hh_member_filename)
# 
# data_hh_member_host_base <- read_rds(data_hh_member_host_filename)

survey_data <- read_rds(file_survey_data_base)


survey_data_baseline <-
	survey_data %>%
	filter(timepoint == "baseline")

survey_data %>%
	filter(timepoint == "baseline") %>%
	count(study_arm_overall)
# For each baseline, midline, endline there are 305 comparison hh and 486 intervention hh
# 791 hh with triplicate measures

## If you want to center-align values in Word, use noSpaces option.
# print(tableOne, # nonnormal = c(nonnormal vars, each in quotes)
# 			exact = c("status","stage"), quote = TRUE, noSpaces = TRUE) # quote = TRUE so excel doesn't mess up the cells; noSpaces == TRUE to center-align


################################################################################
################################################################################


################################################################################
# Who gathers fuel (only asked at baseline)
################################################################################

# Not mutally exclusive

# At baseline
fuel_procurement_who_baseline <-
	survey_data %>% 
	filter(timepoint == "baseline") %>%
	select(
		fcn_id,
		study_arm_overall,
		gather_scraps_w, gather_scraps_m, gather_scraps_g, gather_scraps_b,
		collect_wood_w, collect_wood_m, collect_wood_g, collect_wood_b,
		buy_wood_w, buy_wood_m, buy_wood_g, buy_wood_b,
		receive_wood_w, receive_wood_m, receive_wood_g, receive_wood_b,
		receive_lpg_w, receive_lpg_m, receive_lpg_g, receive_lpg_b,
		buy_lpg_w, buy_lpg_m, buy_lpg_g, buy_lpg_b,
		receive_crh_w, receive_crh_m, receive_crh_g, receive_crh_b,
		buy_crh_w, buy_crh_m, buy_crh_g, buy_crh_b
	) %>%
	pivot_longer( 
		cols = c(
			gather_scraps_w, gather_scraps_m, gather_scraps_g, gather_scraps_b,
			collect_wood_w, collect_wood_m, collect_wood_g, collect_wood_b,
			buy_wood_w, buy_wood_m, buy_wood_g, buy_wood_b,
			receive_wood_w, receive_wood_m, receive_wood_g, receive_wood_b,
			receive_lpg_w, receive_lpg_m, receive_lpg_g, receive_lpg_b,
			buy_lpg_w, buy_lpg_m, buy_lpg_g, buy_lpg_b,
			receive_crh_w, receive_crh_m, receive_crh_g, receive_crh_b,
			buy_crh_w, buy_crh_m, buy_crh_g, buy_crh_b
		),
		names_to = "vars"
	) %>%
	mutate(value = ifelse(is.na(value), 0, value)) %>%
	count(study_arm_overall, vars, value) %>%
	group_by(study_arm_overall, vars) %>%
	mutate(prop = prop.table(n) * 100) %>%
	mutate(sex = str_extract(vars, "[a-z]{1}$")) %>%
	mutate(vars = str_remove(vars, "_[a-z]{1}$")) %>%
	pivot_wider(names_from = value, values_from = c(n, prop)) %>%
	select(-c(n_0, prop_0)) %>%
	replace_na(list(n_1 = 0L, prop_1 = 0)) %>% # need 0L because n_1 is an integer
	pivot_wider(names_from = c("sex"), values_from = c("n_1", "prop_1"), names_vary = "slowest") %>% # slowest" makes it vary first by the values then the names
	mutate(
		fuel_procurement = 
			ordered(
				vars,
				levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg"),
				labels = c("Gather scraps", "Collect wood", "Buy wood", "Receive wood", "Buy compressed rice husks", "Receive compressed rice husks", "Buy LPG", "Receive LPG")
			)
	) %>%
	ungroup %>%
	arrange(study_arm_overall, fuel_procurement) %>%
	select(study_arm_overall, fuel_procurement, n_1_m, prop_1_m, n_1_w, prop_1_w, n_1_b, prop_1_b, n_1_g, prop_1_g) # %>%
	# Doesn't work because names must be unique
	# rename(n = n_1_m, "%" = prop_1_m, n = n_1_w, "%" = prop_1_w, n = n_1_b, "%" = prop_1_b, n = n_1_g, "%" = prop_1_g)
	

fuel_procurement_who_baseline_comparison <-
	fuel_procurement_who_baseline %>%
	filter(study_arm_overall == "comparison") %>%
	select(-study_arm_overall)

fuel_procurement_who_baseline_comparison %>%
	kbl(
		caption = "Fuel Procurement by Sex, at Baseline",
		booktabs = TRUE,
		digits = 1, # only rounded the dbl (didn't add digits to int), which is good 
		format = "latex"
	) %>%
	add_header_above(c("", "n", "%", "n", "%", "n", "%", "n", "%")) %>%
	add_header_above(c("Fuel", "Men" = 2, "Women" = 2, "Boys" = 2, "Girls" = 2)) %>%
	# remove the header manually
	kable_styling(latex_options = c("scale_down")) # "striped",

# Men are primarily responsible for gathering all types of fuel; we should see that more women are getting LPG as some camps (like 8w) require that women procure the LPG. 


# [0-9]{6}$


# [0-9]{6}$

##############################################################################
# When did Firewood collection occur?
##############################################################################

# When did hh start and stop collecting wood?
survey_data %>%
	filter(!is.na(collect_wood_forest_start)) %>%
	select(
		fcn_id, study_arm_overall, timepoint,
		collect_wood_walk_hr, 
		collect_wood_times_week,
		collect_wood_forest_start,
		collect_wood_forest_stop,
		buy_wood_when_start, 
		forest_wood_fee
	) %>%
	arrange(fcn_id, desc(timepoint))


survey_data %>% 
	filter(collect_wood_duration < 0) %>%
	select(fcn_id, study_arm_overall, timepoint, collect_wood_duration, collect_wood_forest_start, collect_wood_forest_stop) %>%
	arrange(timepoint, collect_wood_forest_stop)

## The following were fixed 220721 so after that there were 0 days that had this problem. 
# fcn_id with negative duration of collecting firewood --> probably the start and stop dates are incorrect
# correct this during data cleaning --> fixed on 220721
# fcn_id study_arm_overall timepoint collect_wood_duration collect_wood_forest_start collect_wood_forest_stop
# <chr>  <ord>             <ord>                     <dbl> <chr>                     <chr>                   
# 1 600061 intervention      baseline                   -122 1-Aug-17                  1-Apr-17                
# 2 124870 comparison        baseline                    -31 1-Aug-17                  1-Jul-17                
# 3 120773 comparison        baseline                    -31 1-Aug-18                  1-Jul-18                
# 4 124869 comparison        baseline                   -153 1-Aug-17                  1-Mar-17                
# 5 107198 intervention      baseline                   -365 1-Sep-19                  1-Sep-18                
# 6 121839 comparison        midline                    -153 Sep 1, 2017               Apr 1, 2017             
# 7 182703 comparison        midline                     -61 Oct 1, 2017               Aug 1, 2017             
# 8 110349 intervention      midline                    -181 Aug 1, 2017               Feb 1, 2017             
# 9 295889 comparison        midline                    -181 Aug 1, 2017               Feb 1, 2017             
# 10 184266 comparison        midline                    -184 Sep 1, 2017               Mar 1, 2017             
# 11 109816 comparison        midline                     -92 Aug 1, 2017               May 1, 2017             
# 12 184565 comparison        midline                     -30 Dec 1, 2017               Nov 1, 2017   

survey_data %>%
	select(fcn_id, study_arm_overall, timepoint, collect_wood_forest_start_ymd, collect_wood_forest_stop_ymd, collect_wood_duration) %>%
	group_by(study_arm_overall, timepoint) %>%
	summarise(collect_wood_duration_wk_mean = mean(collect_wood_duration, na.rm = TRUE)/7 )

# study_arm_overall timepoint collect_wood_duration_wk_mean
# <ord>             <ord>                             <dbl>
# 1 comparison        baseline                           44.0 # at baseline they said they'd been collecing for 44 weeks before they stopped 
# 2 comparison        midline                            54.1
# 3 intervention      baseline                           52.3
# 4 intervention      midline                            81.0 # at midline they said they'd been collecting for 81 weeks before they stopped

## This actually isn't that helpful except to correlate with deforestation. At what point did hh decide it was too far / too much work to go to the forest? (look at baseline)

survey_data %>%
	select(fcn_id, study_arm_overall, timepoint, collect_wood_forest_stop_ymd, first_received_lpg) %>%

	ggplot() + 
	geom_histogram(aes(x = collect_wood_forest_stop_ymd)) + 
	facet_wrap(study_arm_overall ~ timepoint)

survey_data %>%
	select(fcn_id, study_arm_overall, timepoint, collect_wood_forest_stop_ymd, first_received_lpg) %>%
	pivot_longer(names_to = "event", values_from = c("collect_wood_forest_stop_ymd", ))
	ggplot() + 
	geom_histogram(aes(x = collect_wood_forest_stop_ymd)) + 
	facet_wrap(study_arm_overall ~ timepoint)


# For understanding whether or not people were collecting wood, look at fuel_30_collect_wood (in 3.2 fuel_sources)


######################################################################################################
######################################################################################################


## Hours spent gathering wood

# collect_wood_walk_hr

# English: Currently, how long (in nearest hour) does it take you to walk to and from the area where you collect wood?
# Bengali: How much time (in hours) does it take to walk from where you collect fuel?
# The Bengali seems to ask about ONE WAY walk time

survey_data %>%
	select(study_arm_overall, timepoint, collect_wood_walk_hr) %>% 
	group_by(study_arm_overall, timepoint) %>%
	summarise(
		min = min(collect_wood_walk_hr, na.rm = TRUE), 
		mean = mean(collect_wood_walk_hr, na.rm = TRUE), 
		max = max(collect_wood_walk_hr, na.rm = TRUE)
	)

fig_wood_collection_hours <-
	survey_data %>%
	select(study_arm_overall, timepoint, collect_wood_walk_hr) %>% 
	ggplot() +
	geom_boxplot(aes(y = collect_wood_walk_hr, x = timepoint, fill = study_arm_overall)) +
	# geom_histogram(stat = "count") + 
	# geom_vline(aes(xintercept = mean(collect_wood_walk_hr, na.rm = TRUE), col = 'red'), size = 2, show.legend = TRUE) + 
	# geom_vline(aes(xintercept = median(collect_wood_walk_hr, na.rm = TRUE), col = 'blue'), size = 2, show.legend = TRUE) +
	# scale_colour_manual("Legend", values = c(red = "red", blue = "blue"), labels = c("mean", "median")) +
	# scale_colour_manual("Legend", values = c(red = "red"), labels = c("mean")) +
	# scale_x_continuous(breaks = 0:11, minor_breaks = NULL) +
	labs(
		title = "Round-trip time to walk to the forest and collect wood",
		x = "Timepoint", 
		y = "Hours",
		fill = "study arm"
	) +
	theme_classic()

fig_wood_collection_hours

ggsave(
	here::here("6_figures", "fig_wood_collection_hours.png"), 
	plot = last_plot(),
	scale = 1,
	device = "png"
)
######################################################################################################
######################################################################################################


## Number of times firewood is gathered per week
# collect_wood_times_week


survey_data %>%
	select(study_arm_overall, timepoint, collect_wood_times_week) %>% 
	group_by(study_arm_overall, timepoint) %>%
	summarise(
		min = min(collect_wood_times_week, na.rm = TRUE), 
		mean = mean(collect_wood_times_week, na.rm = TRUE), 
		max = max(collect_wood_times_week, na.rm = TRUE)
	)

fig_wood_collection_times_week <-
	survey_data %>%
	select(study_arm_overall, timepoint, collect_wood_times_week) %>% 
	ggplot() +
	geom_boxplot(aes(y = collect_wood_times_week, x = timepoint, fill = study_arm_overall)) +
	scale_y_continuous(breaks = c(1:10)) +
	labs(
		title = "Weekly trips to the forest for wood collection",
		x = "Timepoint", 
		y = "Trips per week",
		fill = "study arm"
	) +
	theme_classic()

fig_wood_collection_times_week

ggsave(
	here::here("6_figures", "fig_wood_collection_times_week.png"), 
	plot = last_plot(),
	scale = 1,
	device = "png"
)

fig_wood_collection_times_week_hh_size <-
	survey_data %>%
	ggplot(aes(x = hh_size, y = collect_wood_times_week, color = collect_wood_walk_hr)) +
	geom_point() + 
	scale_color_viridis_c() + 
	# Need to fix ticks so they are whole numbers
	facet_grid(timepoint ~ fuel_30_buy_wood)

# There are 3 hh that collect 8-9 times per week and they do not have huge hh_size and are not very close to the forest though they also don't buy any fuel.
# No clear relationship between hh size and number of times went to the forest for wood, even when didn't buy wood

fig_wood_collection_times_week_hh_size

ggsave(
	here::here("6_figures", "fig_wood_collection_times_week_hh_size.png"), 
	plot = last_plot(),
	scale = 1,
	device = "png"
)




######################################################################################################
## Cost to enter the forest for firewood
######################################################################################################

# forest_wood_fee


survey_data %>%
	group_by(timepoint) %>%
	select(study_arm_overall, forest_wood_fee) %>%
	summarise(
		min = min(forest_wood_fee, na.rm = TRUE), 
		mean = mean(forest_wood_fee, na.rm = TRUE), 
		max = max(forest_wood_fee, na.rm = TRUE)
	)

fig_forest_wood_fee <-
	survey_data %>%
	select(study_arm_overall, timepoint, forest_wood_fee) %>% 
	ggplot() +
	geom_boxplot(aes(y = forest_wood_fee, x = timepoint, fill = study_arm_overall)) +
	# scale_y_continuous(breaks = c(1:10)) +
	labs(
		title = "Cost to enter forest for wood collection",
		x = "timepoint", 
		y = "cost (BDT)",
		fill = "study arm"
	) +
	theme_classic()

fig_forest_wood_fee

ggsave(
	here::here("6_figures", "fig_forest_wood_fee.png"), 
	plot = last_plot(),
	scale = 1,
	device = "png"
)

# 4 hh report a cost of 200 BDT to enter the forest, which is high, especially considering the next highest is 120 BDT. Mistake?

######################################################################################################
######################################################################################################

## Cost of firewood

# 	buy_wood_cost 
# "In the past 30 days, how much money (taka) did you spend on firewood?"

survey_data %>%
	group_by(timepoint) %>%
	select(study_arm_overall, buy_wood_cost) %>%
	summarise(
		min = min(buy_wood_cost, na.rm = TRUE), 
		mean = mean(buy_wood_cost, na.rm = TRUE), 
		max = max(buy_wood_cost, na.rm = TRUE)
	)

fig_buy_wood_cost <-
	survey_data %>%
	select(study_arm_overall, timepoint, buy_wood_cost, hh_size) %>% 
	ggplot() +
	geom_boxplot(aes(y = buy_wood_cost / hh_size, x = timepoint, fill = study_arm_overall)) +
	# scale_y_continuous(breaks = c(1:10)) +
	labs(
		title = "Monthly cost of firewood per household member",
		x = "timepoint", 
		y = "cost (BDT)",
		fill = "study arm"
	) +
	theme_classic()

fig_buy_wood_cost

ggsave(
	here::here("6_figures", "fig_buy_wood_cost.png"), 
	plot = last_plot(),
	scale = 1,
	device = "png"
)

# No strong relationship between monthly cost and hh size

fig_buy_wood_cost_hh_size <-
	survey_data %>%
	ggplot(aes(x = hh_size, y = buy_wood_cost)) +
	geom_point() + 
	scale_color_viridis_c() + 
	# Need to fix ticks so they are whole numbers
	facet_grid(timepoint ~ study_arm_overall)

fig_buy_wood_cost_hh_size


# Since the hh that reported spent most purchasing firewood don't have an obvious reason for this (they don't have huge hh size though are cooking for an un-recorded number of other people). It appears that these hh reporting very high cost of firewood just have bad recall, or there is an error.

######################################################################################################
######################################################################################################

## Reason for using wood fuel

# 1	We don’t have enough of other types of fuel
# 2	We prefer using wood to cook food
# 66	Other
# 
# Need to take account for hh that both gather wood and buy wood
# Make the question: Among hh that use wood (collect or purchase), why do they use wood?

survey_data %>%
	filter(timepoint == "baseline") %>%
	select(gather_wood_reason, buy_wood_reason) %>%
	count(gather_wood_reason, buy_wood_reason) %>%
	mutate(percent = n / sum(n))


# Almost all hh that rely on firewood report the primary reason they use firewood is because they do not have enough of other fuel sources; 
# only (1+16+9+2+21) / 791 = 6.2% report that the primary reason they use firewood is because they prefer the taste. 


# are there hh that give diff reasons for collecting vs buying firewood?
survey_data %>%
	filter(!is.na(gather_wood_reason) & !is.na(buy_wood_reason)) %>%
	mutate(
		gather_buy_wood_reason_discordant = ifelse(gather_wood_reason != buy_wood_reason, "discordant", 0)
	) %>%
	filter(gather_buy_wood_reason_discordant == "discordant") %>%
	select(gather_wood_reason, buy_wood_reason)






