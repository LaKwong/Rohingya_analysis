################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Stove use data analysis
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

# ============================================================================

# Notes

# comparison = had an LPG stove at baseline
# intervention = did not have LPG stove at baseline
# study_arm:comparison
# Study_arm:intervention
# 
# study_arm:host
# intervention_period:endline   only  (there are no measurements from baseline or midline)
# 
# 
# empty tags on geocene that I can't figure out how to get rid of
# intervention_period:control --> empty
# intervention_period:intervention --> empty
# intervention_period:intervention_arm --> empty
# intervention_period:post --> empty
# intervention_period:pre --> empty
# intervention_period:pre_intervention --> empty
# 
# intevention_period:intervention #"intevention_period" is missing an "r"
# 




# files_in

file_events <- here::here("2_data_raw/Geocene_220705/events_22.csv")
file_mission_logs <- here::here("2_data_raw/Geocene_220705/mission_logs_22.csv")
file_missions <- here::here("2_data_raw/Geocene_220705/missions_22.csv")
file_sensors <- here::here("2_data_raw/Geocene_220705/sensors_22.csv")
file_tags <- here::here("2_data_raw/Geocene_220705/tags_22.csv")

list_of_file_path_geocene <- here::here("2_data_raw/Geocene_220705/metrics") 

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_clean.rds")


# file out
file_out_errors_1 <- here::here("4_data/df_have_geocene_but_no_survey_data.csv")
file_out_errors_2 <- here::here("4_data/df_exclusive_use_pre_intervention_in_intervention_arm.csv")

#===============================================================================

# load files

# list_of_file_geocene <-   
# 	list.files(
# 		path = list_of_file_path_geocene,
# 		recursive = TRUE, 
# 		pattern = "*.csv", 
# 		full.names = TRUE 
# 	)

# length(list_of_file_geocene) #134

df_events <- read.csv(file_events)
df_mission_logs <- read.csv(file_mission_logs)
df_missions <- read.csv(file_missions)
df_sensors <- read.csv(file_sensors)
df_tags <- read.csv(file_tags)

survey_data_raw <- 
	read_rds(file_survey_data_base)

survey_data <-
	survey_data_raw %>%
	select(timepoint, study_arm_overall, fcn_id, camp_id, block_id, subblock_id, first_enrolled_lpg, first_receive_lpg) %>% 
	mutate(timepoint = ifelse(timepoint == "endline", "midline", timepoint))
	
# survey_data %>% 
# 	count(first_enrolled_lpg, timepoint, study_arm_overall) %>% 
# 	view()
	#===============================================================================
##################################### Need to change survey "endline" to "midline #####################################
#===============================================================================

	



#===============================================================================
# Create dataset
#===============================================================================

# processors
# 
# cooking_time_50_70C_3_40min
# stove_on_50_70C_3_3min
# - for this select only stove_on


df_mission_id_practice <-
	df_tags %>%
	filter(tag %in% c("practice", "not_normal", "empty")) %>%
	pull(mission_id) %>%
	unique()

df_mission_id_to_analyze <-
	df_tags %>%
	filter(mission_id %notin% df_mission_id_practice) %>%
	mutate(
		study_arm =
			case_when(
				tag == "study_arm:intervention" ~ "intervention",
				tag == "study_arm:comparison" ~ "comparison" # Note that intervention is misspelled in the tag
			),
		fuel_type = 
			case_when(
				tag == "fuel_type:biomass" ~ "biomass",
				tag == "fuel_type:lpg" ~ "lpg"
			),
		timepoint = 
			case_when(
				tag == "intervention_period:baseline" ~ "baseline",
				tag == "intervention_period:midline" ~ "midline",
				tag == "intervention_period:endline" ~ "endline"
			)
	) %>% 
	group_by(mission_id) %>% 
	fill(study_arm, fuel_type, timepoint) %>% 
	fill(study_arm, fuel_type, timepoint, .direction = 'up') %>% 
	distinct() %>%
	select(-tag)

#===============================================================================
#### Get the mission_name (which includes the hhid) associated with each mission id ####
#===============================================================================

# CF_20191013_0_10GG38108799_11:00

df_missions_hh_id <-
	df_mission_id_to_analyze %>%
	left_join(
		df_missions %>%
			separate("mission_name", into = c("nothing_1", "date", "study_arm_overall_numeric", "hh_id", "start_time"), sep = "_", remove = FALSE),
		by = "mission_id"
	) %>%
	mutate(fcn_id = str_extract(hh_id, ".{6}$") ) %>%
	select(mission_id, hh_id, fcn_id) %>%
	
	# Missing hh_id and fcn_id: 623181c4-0000-0000-0000-806FB06367FC, 62357165-0000-0000-0000-806FB063667A, 623971f7-0000-0000-0000-806FB063665F, 	
	# 6257accc-0000-0000-0000-806FB0635D87, 628b29a1-0000-0000-0000-806FB063619A, 628b3edd-0000-0000-0000-806FB06347AB
	# assume these are practice but were not labelled as such --> remove
		filter(!is.na(fcn_id))




#===============================================================================
#### Get date first enrolled in lpg distribution and date first actually received lpg ####
#===============================================================================


# In the baseline surveys the enrollment date was saved as dmy
df_first_enrolled_lpg_baseline_comparison <- 
	survey_data %>%
	filter(timepoint == "baseline", study_arm_overall == "comparison") %>%
	filter(!is.na(first_enrolled_lpg)) %>%
	mutate(
		first_enrolled_lpg_ymd = dmy(as.character(first_enrolled_lpg)),
		first_receive_lpg_ymd = dmy(as.character(first_receive_lpg))
	) %>%
	select(fcn_id, first_enrolled_lpg_ymd, first_receive_lpg_ymd)
# filter(fcn_id %in% c(no_exclusive_use_fcn_id))



# In the baseline surveys the enrollment date was saved as mdy
df_first_enrolled_lpg_endline_intervention <- 
	survey_data %>%
	filter(timepoint == "midline", study_arm_overall == "intervention") %>% # look at the "midline" dataset because at baseline the intervention hh didn't have LPG
	filter(!is.na(first_enrolled_lpg)) %>%
	mutate(
		first_enrolled_lpg_ymd = mdy(as.character(first_enrolled_lpg)),
		first_receive_lpg_ymd = mdy(as.character(first_receive_lpg))
	) %>%
	select(fcn_id, first_enrolled_lpg_ymd, first_receive_lpg_ymd)
# filter(fcn_id %in% c(no_exclusive_use_fcn_id))

df_first_enrolled_lpg <-
	bind_rows(df_first_enrolled_lpg_baseline_comparison, df_first_enrolled_lpg_endline_intervention)

#### Combine datasets ####

#### Dataset for cooking events #######
anti_join(
	df_missions_hh_id  %>% select(mission_id) %>% distinct() %>% nrow() , # 529
as_tibble(df_events) %>% select(mission_id) %>% distinct() %>% nrow()  # 338
) %>% View()

df_events_stove_on <-
	df_missions_hh_id %>%
	left_join(df_events, by = "mission_id") %>%  # add processor_name, model_name, event_kind, start_time, stop_time
	right_join(df_mission_id_to_analyze, by = c("mission_id")) %>% # keep only those mission_ids that have the correct tags
	#separate(mission_id, into = c("fcn_id", "nothing_1", "nothing_2", "nothing_3", "sensor"), sep = "-", extra = "merge", remove = FALSE)
	
	# filter(processor_name == "cooking_time_50_70C_2_10min") %>%  # "stove_on_50_70C_3_3min"
	
	left_join(df_first_enrolled_lpg, by = c("fcn_id")) %>%
	ungroup() %>% 
	
	# convert start_time to date so I can analyze events by date # 2019-09-24T13:19:06Z
	mutate(date = ymd(str_extract(start_time, "^[0-9]{4}-[0-9]{2}-[0-9]{2}"))) %>%


	mutate(
		# 2019-09-19T13:16:20Z	
		start_time = ymd_hms(as.character(str_replace(str_replace(start_time, "T", " "), "Z", ""))), # ^[0-9]{4}-[0-9]{2}-[0-9]{2}
		stop_time = ymd_hms(as.character(str_replace(str_replace(stop_time, "T", " "), ":Z", "")))
	) %>%
	rowwise() %>%
	drop_na(start_time) %>%  #The NA's were making the calculation fail 
	mutate(
		stove_on_min = difftime(stop_time, start_time, units = "mins")
	) %>%
	ungroup() # %>%

# # The following is not necessary because at baseline comparison hh were required to be enrolled and have an LPG tank in their house; intervention hh were required to not yet be enrolled. 
	# mutate(
	# 	lpg_enrolled_and_receiving = 
	# 		case_when(
	# 			date < first_receive_lpg_ymd ~ "not yet receiving LPG through distribution program",
	# 			date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
	# 			study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
	# 			TRUE ~ "not yet receiving LPG through distribution program" # default to false
	# 		)
	# )

####################################################################################


##### check dataset ####
df_have_geocene_but_no_survey_data <-
	df_events_stove_on %>%
	filter(is.na(first_receive_lpg_ymd)) %>%
	distinct(fcn_id, .keep_all = TRUE) %>%
	select(hh_id, fcn_id, date, timepoint)

df_have_geocene_but_no_survey_data %>% count(timepoint)
####################################################################################

####################################################################################

####################################################################################



# # There are 206 datasets associated with hh that do not have survey data
# survey_data %>%
# 	filter(hh_id == "8WDI18224646")

####################################################################################

####################################################################################

####################################################################################



df_have_geocene_but_no_survey_data_fcn_id <-
	df_have_geocene_but_no_survey_data	%>%
	arrange(fcn_id) %>%
	pull(fcn_id)

write_csv(df_have_geocene_but_no_survey_data, file_out_errors_1)

# As of 12 April 2021, don't worry about the following
# Not clear why these fcn_id's don't have surveys because the surveys associated with these fcn_ids do exist
# "101573" "101595" "107339" "123577" "133582" "179029" "192619" "192754" "192773" "197310" "201621" "201745" "224646" "291216" "291519"

####################################################################################

df_events_stove_on$first_receive_lpg_ymd

df_events_stove_on_per_day <-
	df_events_stove_on %>%
	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
	group_by(study_arm, hh_id, fcn_id, fuel_type, date, first_receive_lpg_ymd) %>% # study_arm_overall
	summarise(n = n(), stove_on_min_sum = sum(stove_on_min)) %>% # count number of cooking events for each fuel type
	pivot_wider(names_from = fuel_type, values_from = c(n, stove_on_min_sum)) %>%
	 mutate(
	 	stove_on_min_sum_biomass = ifelse(!is.na(stove_on_min_sum_biomass), as.numeric(as.character(stove_on_min_sum_biomass)), 0),
	 	stove_on_min_sum_lpg = ifelse(!is.na(stove_on_min_sum_lpg), as.numeric(as.character(stove_on_min_sum_lpg)), 0)
	 ) %>%
	ungroup() %>%
	rowwise() %>%
	 mutate(
	 	stove_on_min_pc_biomass =  stove_on_min_sum_biomass/ sum(stove_on_min_sum_biomass, stove_on_min_sum_lpg, na.rm = TRUE) * 100,
	 	stove_on_min_pc_lpg = stove_on_min_sum_lpg / sum(stove_on_min_sum_biomass, stove_on_min_sum_lpg, na.rm = TRUE) * 100,
	 ) %>%
	ungroup() %>%
	mutate(
		days_after_first_receiving = as.numeric(date - first_receive_lpg_ymd),
		# Lump together all of the data prior to receiving LPG because the actual number of days prior to receiving LPG doesn't matter
		days_after_first_receiving = ifelse(days_after_first_receiving < 0, -1, days_after_first_receiving), #is.na(days_after_first_receiving) 
		# hh that have days_after_first_receiving == NA are because there was no corresponding survey, but don't present -1 data for these hh because they already had LPG, we just don't know how long
		months_after_first_receiving = 
			cut(
				days_after_first_receiving,
				breaks = seq(-30, 30 * 36, 30),  #37
				labels = c(-1, seq(0, 35, 1))
			),
		# cut_interval(
		# 	days_after_first_receiving,
		# 	width = 30, 
		# 	boundary = 0.5,
		# 	center = 0,
		# 	labels = FALSE,
		# 	closed = "right"
		# ),
		months_after_first_receiving_numeric = as.numeric(as.character(months_after_first_receiving)), # without character, it reads the factor labels (1, 2, 3...)	,
		exclusive_biomass = if_else(is.na(n_lpg), TRUE, FALSE),
		exclusive_lpg = if_else(is.na(n_biomass), TRUE, FALSE),
		mixed_use = if_else(!is.na(n_biomass) & !is.na(n_lpg), TRUE, FALSE)
	) %>%
	rename(
		cooking_events_with_biomass = n_biomass,
		cooking_events_with_lpg = n_lpg
	)

# 600021 was receiving  -  first_receive_lpg_ymd: 2019-12-01 - not sure how we had this info; maybe the month was incorrect and should have been 2019-10-01?, but no lpg was observed used

df_events_stove_on_per_day_long <-
	df_events_stove_on_per_day %>%
	# pivot_longer(
	# 	cols = c("stove_on_min_sum_biomass", "stove_on_min_sum_lpg"),
	# 	names_to = c("fuel_type"),
	# 	values_to = c("stove_on_min")
	# ) %>%
	pivot_longer(
		cols = c("stove_on_min_sum_biomass", "stove_on_min_sum_lpg", "stove_on_min_pc_biomass", "stove_on_min_pc_lpg"),
		names_to = c(".value", "fuel_type"),
		names_sep = "(?s)_(?!.*_)"
		# match the last occurance: (?s)pattern(?!.*pattern) OR pattern(?![\s\S]*pattern) OR pattern(?!(?s:.*)pattern)
		# where [\s\S]* matches any zero or more chars as many as possible. (?s) and (?s:.) can be used with regex engines that support these constructs so as to use . to match any chars
	)







#===============================================================================
# Determine percent of days that exclusivity of use by 
#===============================================================================

# get the number of days the hh was monitored
df_stove_exclusive_use_num_days_measured <-
	df_events_stove_on_per_day %>%
	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
	group_by(fcn_id) %>%
	summarise(n = n ()) %>%
	arrange(n)

df_stove_exclusive_use_num_days_measured %>%
	ggplot(aes(x = n)) + 
	geom_histogram(binwidth = 1) 


df_stove_exclusive_use_by_month <-
	df_events_stove_on_per_day %>%
	group_by(fcn_id, months_after_first_receiving) %>%
	summarise(mean = mean(exclusive_lpg == TRUE, na.rm = TRUE)) %>%
	arrange(months_after_first_receiving)

fig_df_stove_exclusive_use_label_group <-
	df_stove_exclusive_use_by_month %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	group_by(months_after_first_receiving) %>%
	summarise(n = n())

# df_events_stove_on_per_day %>%
# 	filter(fcn_id == "286053") %>%
# 	View()
	





# Define the number of colors you want
color_count <- 24
getPalette <- colorRampPalette(brewer.pal(8, "Set1"))

fig_df_stove_exclusive_use <-
	df_stove_exclusive_use_by_month %>%
	mutate(months_after_first_receiving_group = as.character(months_after_first_receiving), 
				 months_after_first_receiving_group = as.double(months_after_first_receiving_group)) %>% 
	filter(
		fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id), 
		months_after_first_receiving_group >= 0
		) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = months_after_first_receiving_group, y = mean, color = as.factor(months_after_first_receiving_group))) +
	geom_jitter(alpha = 0.7) + 
	stat_summary(aes(group = months_after_first_receiving_group), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	stat_summary(aes(group = months_after_first_receiving_group), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	# scale_color_brewer(palette = "Paired") +
	# scale_color_manual(values = mycolors) +
	#scale_color_viridis_c() +
	scale_y_continuous(labels = scales::percent) +
	geom_text(
		inherit.aes = FALSE,
		aes(
			x = 0.5, 
			y = 1.05, 
			label = "n ="
		)
	) +
	geom_text(
		data = fig_df_stove_exclusive_use_label_group,
		inherit.aes = FALSE,
		aes(
			x = months_after_first_receiving, 
			y = 1.05, 
			label = n
			# label = paste("n = ", n)
		)
	) +
	theme_bw() + 
	theme(
		legend.position = "none"
	) + 
	labs(
		x = "Months after first receiving LPG through free distribution program",
		y = "Percent of days household exclusively used LPG when cooking"
	) +
	coord_cartesian(clip = "off")

fig_df_stove_exclusive_use


ggsave(
	here::here("6_figures/stove_exclusive_use_since_month_received.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 7,
	units = "in",
	device = "tiff"
)








# # There appear to be a number of intervention hh that were using LPG before the intervention 
# # evern though using LPG was an exclusion criteria for hh in the intervention arm
# df_exclusive_use_pre_intervention_in_intervention_arm <-
# 	df_events_stove_on_per_day %>%
# 	filter(
# 		study_arm_overall == "intervention",
# 		lpg_enrolled_and_receiving == "not yet receiving LPG through distribution program",
# 		# hh that did not exclusively use biomass
# 		mean > 0
# 	)
# 
# write_csv(df_exclusive_use_pre_intervention_in_intervention_arm, file_out_errors_2)






#===============================================================================
# Number of stoves monitored each day before and after first receiving LPG (biomass vs LPG)
#===============================================================================

# asses only those days that had over 10 stoves monitored per day
df_stove_monitored_per_day <-
	df_events_stove_on_per_day_long %>%
	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	select(fcn_id, fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving) %>%
	unique() %>%
	group_by(fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving) %>%
	count()

df_days_8_plus_stoves_monitored <-
	df_stove_monitored_per_day %>%
	filter(n >= 8) %>%
	select(fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving)


fig_stove_monitored_per_day <-
	df_days_8_plus_stoves_monitored %>%
	left_join(df_stove_monitored_per_day, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = n, color = fuel_type)) +
	# geom_point(alpha = 0.5) +
	geom_jitter(alpha = 0.5, shape = 16) + # must jitter or points are directly on top of each other
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	scale_color_manual(
		name = "Fuel type",
		breaks = c("lpg", "biomass"),
		labels = c( "LPG", "Biomass"),
		values = c("#0072B2", "#D55E00") # c("#0072B2","#999999","#F0E442","#D55E00")
	) +
	theme_bw() + 
	# theme(
	# 	legend.position = "none"
	# ) + 
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "# of stoves monitored"
	) # + 
# facet_wrap(~ lpg_enrolled_and_receiving, scales = "free")

fig_stove_monitored_per_day

ggsave(
	here::here("6_figures/stoves_monitored_per_day.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 7,
	units = "in",
	device = "tiff"
)

#===============================================================================
# Determine average mintues of daily use
#===============================================================================

df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
filter(days_after_first_receiving > 30) %>%
	group_by(fuel_type) %>%
	summarise(mean = mean(stove_on_min_sum, na.rm = TRUE))
# Over all the stove tested on all the days (after 30 days since "receiving" since some didn't actually receive during this time), mean percent of time using biomass was 3.7 min and 93.5 min


fig_time_stove_on_per_day <-
	df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
		# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = stove_on_min_sum, color = fuel_type)) +
	# geom_point(alpha = 0.5) +
	geom_jitter(alpha = 0.5, shape = 16) +
	# add average line
	geom_hline(yintercept = 93.5, color = "#0072B2") +
	geom_text(inherit.aes = FALSE, aes(x = 600, y = 93.5 + 20), label = "93.5 min", color = "#0072B2") +
	geom_hline(yintercept = 3.7, color = "#D55E00") +
	geom_text(inherit.aes = FALSE, aes(x = 600, y = 3.7 + 20), label = "3.7 min", color = "#D55E00") +
	# geom_smooth(aes(group = fuel_type), method = 'lm', formula =  y ~ x) + 
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	scale_color_manual(
		name = "Fuel type",
		breaks = c("lpg", "biomass"),
		labels = c( "LPG", "Biomass"),
		values = c("#0072B2", "#D55E00") # c("#0072B2","#999999","#F0E442","#D55E00")
	) +
	theme_bw() + 
	# theme(
	# 	legend.position = "none"
	# ) +
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "Minutes of use"
	) # + 
# facet_wrap(~ fuel_type, scales = "free")

fig_time_stove_on_per_day


ggsave(
	here::here("6_figures/stove_used_min_per_day.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 7,
	units = "in",
	device = "tiff"
)





#===============================================================================
# Percent of daily use
#===============================================================================

df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	filter(days_after_first_receiving > 30) %>%
	group_by(fuel_type) %>%
	summarise(mean = mean(stove_on_min_pc, na.rm = TRUE))
# Over all the stove tested on all the days (after 30 days since "receiving" since some didn't actually receive during this time), mean percent of time using biomass was 3.9% and lpg was 96.1%


fig_pc_time_stove_on_per_day <-
	df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = stove_on_min_pc, color = fuel_type)) +
	# geom_point() +
	geom_jitter(alpha = 0.5, shape = 16) +
	geom_hline(yintercept = 96.1, color = "#0072B2") +
	geom_text(inherit.aes = FALSE, aes(x = 600, y = 96.1 + 2), label = "96.1%", color = "#0072B2") +
	geom_hline(yintercept = 3.9, color = "#D55E00") +
	geom_text(inherit.aes = FALSE, aes(x = 600, y = 3.9 + 2), label = "3.9%", color = "#D55E00") +
	# geom_smooth(aes(group = fuel_type), method = 'loess', formula =  y ~ x) + 
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	theme_bw() + 
	theme(
		legend.position = "none"
	) +
	scale_color_manual(
		name = "Fuel type",
		breaks = c("lpg", "biomass"),
		labels = c( "LPG", "Biomass"),
		values = c("#0072B2", "#D55E00") # c("#0072B2","#999999","#F0E442","#D55E00")
	) +
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "Percent of daily cooking time"
	) # + 
# facet_wrap(~ lpg_enrolled_and_receiving, scales = "free")

fig_pc_time_stove_on_per_day

ggsave(
	here::here("6_figures/stove_used_min_pc_per_day.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 7,
	units = "in",
	device = "tiff"
)


# library(ggpubr)


ggpubr::ggarrange(
	fig_stove_monitored_per_day, fig_time_stove_on_per_day, fig_pc_time_stove_on_per_day, 
	# labels = c("A", "", "B"),
	heights = c(1, 1, 1),
	ncol = 1, nrow = 3,
	common.legend = TRUE,
	legend = "bottom"
)

ggsave(
	here::here("6_figures/stove_use_num_min_pc_per_day.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 7,
	units = "in",
	device = "tiff"
)





#===============================================================================
# Energy consumed and produced
#===============================================================================

# For LPG
# 

