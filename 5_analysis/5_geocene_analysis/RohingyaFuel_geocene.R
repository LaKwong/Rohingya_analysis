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

# files_in

file_events <- here::here("2_data_raw/Geocene/events.csv")
file_mission_logs <- here::here("2_data_raw/Geocene/mission_logs.csv")
file_missions <- here::here("2_data_raw/Geocene/missions.csv")
file_sensors <- here::here("2_data_raw/Geocene/sensors.csv")
file_tags <- here::here("2_data_raw/Geocene/tags.csv")

list_of_file_path_geocene <- here::here("2_data_raw/Geocene/metrics") 

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

survey_data <- 
	read_rds(file_survey_data_base) %>% 
	select(timepoint, study_arm_overall, fcn_id, camp_id, block_id, subblock_id, first_enrolled_lpg, first_receive_lpg)



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

df_mission_id_to_analyze_base <-
	df_tags %>%
	filter(mission_id %notin% df_mission_id_practice) %>%
	mutate(
		study_arm_overall =
			case_when(
				tag == "intervention_period:pre_intervention" ~ "intervention",
				tag %in% c("intervention_period:intervention", "intevention_period:intervention") ~ "comparison" # Note that intervention is misspelled in the tag
			),
		fuel_type = 
			case_when(
				tag == "fuel_type:biomass" ~ "biomass",
				tag == "fuel_type:lpg" ~ "lpg"
			)
	)

mission_id_study_arm_overall <-
	df_mission_id_to_analyze_base %>%
	filter(!is.na(study_arm_overall)) %>%
	select(-c(tag, fuel_type))

mission_id_fuel_type <-
	df_mission_id_to_analyze_base %>%
	filter(!is.na(fuel_type)) %>%
	select(-c(tag, study_arm_overall))

df_mission_id_to_analyze <-
	mission_id_study_arm_overall %>%
	left_join(mission_id_fuel_type, by = c("mission_id"))


#### Get the mission_name (which includes the hhid) associated with each mission id ####
# CF_20191013_0_10GG38108799_11:00

df_missions_hh_id <-
	df_mission_id_to_analyze %>%
	left_join(
		df_missions %>%
			separate("mission_name", into = c("nothing_1", "date", "study_arm_overall_numeric", "hh_id", "start_time"), sep = "_", remove = FALSE),
		by = "mission_id"
	) %>%
	mutate(fcn_id = str_extract(hh_id, ".{6}$") ) %>%
	select(mission_id, hh_id, fcn_id)

#### Get date first enrolled in lpg distribution and date first actually received lpg ####
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
	filter(timepoint == "endline", study_arm_overall == "intervention") %>%
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
df_events_stove_on <-
	df_missions_hh_id %>%
	left_join(df_events, by = "mission_id") %>%
	right_join(df_mission_id_to_analyze, by = "mission_id") %>% # keep only those mission_ids that have the correct tags
	# separate(mission_id, into = c("fcn_id", "nothing_1", "nothing_2", "nothing_3", "sensor"), sep = "-", extra = "merge", remove = FALSE)
	filter(processor_name == "stove_on_50_70C_3_3min") %>%
	# convert start_time to date so I can analyze events by date # 2019-09-24T13:19:06Z
	mutate(date = ymd(str_extract(start_time, "^[0-9]{4}-[0-9]{2}-[0-9]{2}"))) %>%
	left_join(df_first_enrolled_lpg, by = "fcn_id") %>%
	ungroup() %>%
	mutate(
		# 2019-09-19T13:16:20Z	
		start_time = ymd_hms(as.character(str_replace(str_replace(start_time, "T", " "), "Z", ""))), # ^[0-9]{4}-[0-9]{2}-[0-9]{2}
		stop_time = ymd_hms(as.character(str_replace(str_replace(stop_time, "T", " "), ":Z", "")))
	) %>%
	rowwise() %>%
	mutate(
		stove_on_min = difftime(stop_time, start_time, units = "mins")
	) %>%
	ungroup() %>%
	mutate(
		lpg_enrolled_and_receiving = 
			case_when(
				date < first_receive_lpg_ymd ~ "not yet receiving LPG through distribution program",
				date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
				TRUE ~ "not yet receiving LPG through distribution program" # default to false
			)
	)

df_events_stove_on_per_day <-
	df_events_stove_on %>%
	group_by(study_arm_overall, hh_id, fcn_id, fuel_type, date, first_receive_lpg_ymd, lpg_enrolled_and_receiving) %>% # study_arm_overall
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
		months_after_first_receiving = 
			cut(
				days_after_first_receiving,
				breaks = seq(-60, 30 * 36, 30),  #38
				labels = c(-2, -1, seq(1, 36, 1))
			),
		# cut_interval(
		# 	days_after_first_receiving,
		# 	width = 30, 
		# 	boundary = 0.5,
		# 	center = 0,
		# 	labels = FALSE,
		# 	closed = "right"
		# ),
		months_after_first_receiving = as.numeric(months_after_first_receiving),
		exclusive_biomass = if_else(is.na(n_lpg), TRUE, FALSE),
		exclusive_lpg = if_else(is.na(n_biomass), TRUE, FALSE),
		mixed_use = if_else(!is.na(n_biomass) & !is.na(n_lpg), TRUE, FALSE)
	) %>%
	rename(
		cooking_events_with_biomass = n_biomass,
		cooking_events_with_lpg = n_lpg
	)


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



#### check dataset ####
df_have_geocene_but_no_survey_data <-
	df_events_stove_on %>%
	filter(is.na(first_receive_lpg_ymd)) %>%
	distinct(fcn_id, .keep_all = TRUE) %>%
	select(hh_id, fcn_id, date)

# There are 15 datasets associated with hh that do not have survey data
survey_data %>%
	filter(hh_id == "8WDI18224646")

write_csv(df_have_geocene_but_no_survey_data, file_out_errors)




#===============================================================================
# Determine percent of days that exclusivity of use by 
#===============================================================================

# get the number of days the hh was monitored
df_stove_exclusive_use_num_days_measured <-
	df_events_stove_on_per_day %>%
	group_by(fcn_id, lpg_enrolled_and_receiving) %>%
	summarise(n = n ()) %>%
	arrange(desc(n))

df_stove_exclusive_use_num_days_measured %>%
	ggplot(aes(x = n)) + 
	geom_histogram(binwidth = 1) 


df_stove_exclusive_use_by_month <-
	df_events_stove_on_per_day %>%
	group_by(fcn_id, months_after_first_receiving, lpg_enrolled_and_receiving) %>%
	summarise(mean = mean(exclusive_lpg == TRUE, na.rm = TRUE)) %>%
	arrange(months_after_first_receiving)

fig_df_stove_exclusive_use_label_group <-
	df_stove_exclusive_use_by_month %>%
	filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	group_by(months_after_first_receiving) %>%
	summarise(n = n())

# Define the number of colors you want
color_count <- 24
getPalette <- colorRampPalette(brewer.pal(8, "Set1"))

fig_df_stove_exclusive_use <-
	df_stove_exclusive_use_by_month %>%
	filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = months_after_first_receiving, y = mean, color = as.factor(months_after_first_receiving))) +
	geom_jitter(alpha = 0.7) + 
	stat_summary(aes(group = months_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	stat_summary(aes(group = months_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	# scale_color_brewer(palette = "Paired") +
	# scale_color_manual(values = mycolors) +
	scale_color_manual(values = getPalette(color_count)) +
	
	scale_y_continuous(labels = scales::percent) +
	geom_text(
		inherit.aes = FALSE,
		aes(
			x = 0, 
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
	)

fig_df_stove_exclusive_use


ggsave(
	here::here("6_figures/fig_exclusive.eps"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "eps"
)








# There appear to be a number of intervention hh that were using LPG before the intervention 
# evern though using LPG was an exclusion criteria for hh in the intervention arm
df_exclusive_use_pre_intervention_in_intervention_arm <-
	df_stove_exclusive_use_fraction %>%
	filter(
		study_arm_overall == "intervention",
		lpg_enrolled_and_receiving == "not yet receiving LPG through distribution program",
		# hh that did not exclusively use biomass
		mean > 0
	)

write_csv(df_exclusive_use_pre_intervention_in_intervention_arm, file_out_errors_2)






#===============================================================================
# Number of stoves monitored each day before and after first receiving LPG (biomass vs LPG)
#===============================================================================

# asses only those days that had over 10 stoves monitored per day
df_stove_monitored_per_day <-
	df_events_stove_on_per_day_long %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	select(fcn_id, fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving) %>%
	unique() %>%
	group_by(fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving) %>%
	count()

df_days_over_10_stoves_monitored <-
	df_stove_monitored_per_day %>%
	filter(n > 10) %>%
	select(fuel_type, lpg_enrolled_and_receiving, days_after_first_receiving)


fig_stove_monitored_per_day <-
	df_days_over_10_stoves_monitored %>%
left_join(df_stove_monitored_per_day, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = n, color = fuel_type)) +
	# geom_point() +
	geom_jitter(alpha = 0.7) +
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	# scale_color_brewer(palette = "Paired") +
	# scale_color_manual(values = mycolors) +
	# scale_color_manual(values = getPalette(color_count)) +
	
	# scale_y_continuous(labels = scales::percent) +
	# geom_text(
	# 	inherit.aes = FALSE,
	# 	aes(
# 		x = 0, 
# 		y = 1.05, 
# 		label = "n ="
# 	)
# ) +
# geom_text(
# 	data = fig_df_stove_exclusive_use_label_group,
# 	inherit.aes = FALSE,
# 	aes(
# 		x = days_after_first_receiving, 
# 		y = 1.05, 
# 		label = n
# 		# label = paste("n = ", n)
# 	)
# ) +
theme_bw() + 
	# theme(
	# 	legend.position = "none"
	# ) + 
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "Number of stoves monitored pre day"
	) # + 
# facet_wrap(~ lpg_enrolled_and_receiving, scales = "free")

fig_stove_monitored_per_day

# Many have -30 days before LPG --- this was an artifact for how I measured days. should fix

ggsave(
	here::here("6_figures/stoves_monitored_per_day.eps"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "eps"
)

#===============================================================================
# Determine average mintues of daily use
#===============================================================================
# Define the number of colors you want
color_count <- 24
getPalette <- colorRampPalette(brewer.pal(8, "Set1"))

fig_time_stove_on_per_day <-
	df_days_over_10_stoves_monitored %>%
	left_join(	df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = stove_on_min_sum, color = fuel_type)) +
	geom_point() +
	# geom_jitter(alpha = 0.7) + 
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	# scale_color_brewer(palette = "Paired") +
	# scale_color_manual(values = mycolors) +
	# scale_color_manual(values = getPalette(color_count)) +
	
	# scale_y_continuous(labels = scales::percent) +
	# geom_text(
	# 	inherit.aes = FALSE,
	# 	aes(
	# 		x = 0, 
	# 		y = 1.05, 
	# 		label = "n ="
	# 	)
	# ) +
	# geom_text(
	# 	data = fig_df_stove_exclusive_use_label_group,
	# 	inherit.aes = FALSE,
	# 	aes(
	# 		x = days_after_first_receiving, 
	# 		y = 1.05, 
	# 		label = n
	# 		# label = paste("n = ", n)
	# 	)
	# ) +
	theme_bw() + 
	# theme(
	# 	legend.position = "none"
	# ) + 
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "Minutes of use"
	) # + 
	# facet_wrap(~ lpg_enrolled_and_receiving, scales = "free")

fig_time_stove_on_per_day

# Many have -30 days before LPG --- this was an artifact for how I measured days. should fix

ggsave(
	here::here("6_figures/stove_used_min_per_day.eps"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "eps"
)





#===============================================================================
# Percent of daily use
#===============================================================================


fig_pc_time_stove_on_per_day <-
	df_days_over_10_stoves_monitored %>%
	left_join(	df_events_stove_on_per_day_long, by = c("fuel_type", "lpg_enrolled_and_receiving", "days_after_first_receiving")) %>%
	filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	ggplot(aes(x = days_after_first_receiving, y = stove_on_min_pc, color = fuel_type)) +
	geom_point() +
	# geom_jitter(alpha = 0.7) + 
	# stat_summary(aes(group = days_after_first_receiving), geom = "linerange", fun.data = "mean_cl_boot", colour = "red") +
	# stat_summary(aes(group = days_after_first_receiving), geom = "point", fun.data = "mean_cl_boot", colour = "black", shape = 17, size = 2) +
	# scale_color_brewer(palette = "Paired") +
	# scale_color_manual(values = mycolors) +
	# scale_color_manual(values = getPalette(color_count)) +
	
	# scale_y_continuous(labels = scales::percent) +
	# geom_text(
	# 	inherit.aes = FALSE,
	# 	aes(
# 		x = 0, 
# 		y = 1.05, 
# 		label = "n ="
# 	)
# ) +
# geom_text(
# 	data = fig_df_stove_exclusive_use_label_group,
# 	inherit.aes = FALSE,
# 	aes(
# 		x = days_after_first_receiving, 
# 		y = 1.05, 
# 		label = n
# 		# label = paste("n = ", n)
# 	)
# ) +
theme_bw() + 
	# theme(
	# 	legend.position = "none"
	# ) + 
	labs(
		x = "Days after first receiving LPG through free distribution program",
		y = "Percent of daily cooking time"
	) # + 
# facet_wrap(~ lpg_enrolled_and_receiving, scales = "free")

fig_pc_time_stove_on_per_day

# Many have -30 days before LPG --- this was an artifact for how I measured days. should fix
# maybe limit to hh that have 10 days of readings so that we can see trends?


ggsave(
	here::here("6_figures/stove_used_min_pc_per_day.eps"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "eps"
)

