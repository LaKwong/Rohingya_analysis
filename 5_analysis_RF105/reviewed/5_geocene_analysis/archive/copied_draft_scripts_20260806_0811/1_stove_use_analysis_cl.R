################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Stove use data analysis
# @Version: 3.6.2
# @Date: 240816

#To do 
# Learn hh that still had LPG by the time that the monitors were placed 
################################################################################
rm(list = ls())

#install.packages("binom")
source(here::here("0_config.R"))


## mission_name In mission Name _0_ is intervention and1 is _1_ is comparison group 

# ============================================================================

# files_in for merging

# #Depreciated 
#  file_events_bl <- here::here("2_data_raw/Geocene_210204/events.csv")
#  file_mission_logs_bl <- here::here("2_data_raw/Geocene_210204/mission_logs.csv")
#  file_missions_bl <- here::here("2_data_raw/Geocene_210204/missions.csv")
#  file_sensors_bl <- here::here("2_data_raw/Geocene_210204/sensors.csv")
#  file_tags_bl <- here::here("2_data_raw/Geocene_210204/tags.csv")

file_events_end <- here::here("2_data_raw/Geocene_241202/events.csv")
file_mission_logs_end <- here::here("2_data_raw/Geocene_241202/mission_logs.csv")
file_missions_end <- here::here("2_data_raw/Geocene_241202/missions.csv")
file_sensors_end <- here::here("2_data_raw/Geocene_241202/sensors.csv")
file_tags_end <- here::here("2_data_raw/Geocene_241202/tags.csv")
 # df_events_bl <- read.csv(file_events_bl)
 # df_mission_logs_bl <- read.csv(file_mission_logs_bl)
 # df_missions_bl <- read.csv(file_missions_bl)
 # df_sensors_bl <- read.csv(file_sensors_bl)
 # df_tags_bl <- read.csv(file_tags_bl)
# 
df_events_end <- read.csv(file_events_end)
df_mission_logs_end <- read.csv(file_mission_logs_end)
df_missions_end <- read.csv(file_missions_end)
df_sensors_end <- read.csv(file_sensors_end)
df_tags_end <- read.csv(file_tags_end)

# df_events <- df_events_bl %>% bind_rows(df_events_end)
# df_mission_logs <- df_mission_logs_bl %>% bind_rows(df_mission_logs_end)
# df_missions <- df_missions_bl %>% bind_rows(df_missions_end)
# df_sensors <- df_sensors_bl %>% bind_rows(df_sensors_end)
# df_tags <- df_tags_bl %>% bind_rows(df_tags_end)

df_events <- df_events_end
df_mission_logs <- df_mission_logs_end
df_missions <- df_missions_end
df_sensors <- df_sensors_end
df_tags <- df_tags_end


min(df_events$start_time)

list_of_file_path_geocene <- here::here("2_data_raw/Geocene_220705/metrics") 

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_clean.rds")
file_stove_on_day <- here::here("4_data/RohingyaFuel_stove_on_day.csv")

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

# df_events <- read.csv(file_events)
# df_mission_logs <- read.csv(file_mission_logs)
# df_missions <- read.csv(file_missions)
# df_sensors <- read.csv(file_sensors)
# df_tags <- read.csv(file_tags)

survey_data <- 
	read_rds(file_survey_data_base) %>% # group_by(study_arm_overall) %>% filter(year(start_date)== 2019) %>% count(month(start_date))
	select(timepoint, study_arm_overall, fcn_id, camp_id, block_id, subblock_id, first_enrolled_lpg, first_receive_lpg)

survey_data %>% 
	count(first_enrolled_lpg, timepoint, study_arm_overall) 
	#view()
#===============================================================================
# Create dataset
#===============================================================================

# processors
# cooking_time_42_65C_2_10min

# - for this select only stove_on


df_mission_id_practice <-
	df_tags %>%
	filter(tag %in% c("practice", "not_normal", "empty")) %>%
	pull(mission_id) %>%
	unique()

df_tags %>% 
	count(tag)

df_mission_id_to_analyze_base <-  #This sets the study arm and the fuel type of the data 
	df_tags %>%
	filter(
	  mission_id %notin% df_mission_id_practice, ) %>%
	mutate(
		study_arm_overall =
			case_when(
				tag == "study_arm:comparison" ~ "comparison",
				tag == "study_arm:intervention" ~ "intervention"),
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
	select(mission_id, hh_id, fcn_id, date) 

df_missions_hh_id %>% 
  count(mission_id) %>% arrange((mission_id))
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

# In the midline surveys the enrollment date was saved as mdy
df_first_enrolled_lpg_midline_intervention <- 
	survey_data %>%
	filter(timepoint == "midline", study_arm_overall == "intervention") %>%
	filter(!is.na(first_enrolled_lpg)) %>%
	mutate(
		first_enrolled_lpg_ymd = mdy(as.character(first_enrolled_lpg)),
		first_receive_lpg_ymd = mdy(as.character(first_receive_lpg))
	) %>%
	select(fcn_id, first_enrolled_lpg_ymd, first_receive_lpg_ymd)
# filter(fcn_id %in% c(no_exclusive_use_fcn_id))

#### Combine datasets ####
df_first_enrolled_lpg <-
  bind_rows(df_first_enrolled_lpg_baseline_comparison, df_first_enrolled_lpg_midline_intervention)

df_first_enrolled_lpg %>% 
  write_csv(here::here("2_data_raw/list_first_enrolled_lpg.csv"))
## This data was not collected at endline and so only baseline and midline used here 

#glimpse(df_first_enrolled_lpg)

## This looks at the dates at which households were enrolled and started receiving LPG. Placed as draft figure to RF106 1/23/24
df_first_enrolled_lpg %>% 
  filter(year(first_enrolled_lpg_ymd)>2000 ) %>% 
  ggplot() +
  geom_histogram(aes(first_enrolled_lpg_ymd), fill = "blue") +
  geom_histogram(aes(first_receive_lpg_ymd), fill = "red", alpha = 0.5) + 
  labs(title = "Date LPG ")


#### Dataset for cooking events #######

  as_tibble(df_missions_hh_id) %>% select(mission_id) %>% distinct() %>% nrow()  # 535
  as_tibble(df_events) %>% select(mission_id) %>% distinct() %>% nrow()  # 547
  
  #min(df_events$start_time)
  

## Make sure for this to run all prior code has been run 
df_events_stove_on <-
  df_events %>% #1/23/24 switched the ordering of this code so that it captures each mission id 
  left_join(df_missions_hh_id, by = "mission_id") %>%  # add processor_name, model_name, event_kind, start_time, stop_time. 
  #This is giving a many to many warning 
  right_join(df_mission_id_to_analyze, by = c("mission_id")) %>% # keep only those mission_ids that have the correct tags
  #separate(mission_id, into = c("fcn_id", "nothing_1", "nothing_2", "nothing_3", "sensor"), sep = "-", extra = "merge", remove = FALSE
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
  ungroup() %>% 
  mutate(
	lpg_enrolled_and_receiving = 
		case_when(
			date < first_receive_lpg_ymd | is.na(first_receive_lpg_ymd) ~ "not yet receiving LPG through distribution program",
			date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
 			study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
 			TRUE  ~ "not yet receiving LPG through distribution program" # default to false
		)
 ) %>% 
  distinct() #some rows repeated between base and endline - this makes sure only one set of data remains 

#and to tell difference before and after receiving LPG 
#view(df_events_stove_on)# %>% 
#   arrange(desc(stove_on_min)) %>% select(stove_on_min)
# 
# df_events_stove_on %>% 
#   filter(stove_on_min < 1000) %>% 
#   mutate(stove_on_min = as.numeric(stove_on_min)) %>% 
#   group_by(lpg_enrolled_and_receiving, study_arm_overall, fuel_type) %>% 
#   summarize(median(stove_on_min,na.rm = TRUE))
####################################################################################


##### check dataset ####
df_have_geocene_but_no_survey_data <-
  df_events_stove_on %>%
  filter(is.na(first_receive_lpg_ymd)) %>% ## Use the receive lpg_ymd data as the survey data
  distinct(fcn_id, .keep_all = TRUE) %>%
  select(hh_id, fcn_id, date) %>% 
  mutate(timepoint = case_when(
    year(date) < 2021 ~ "baseline", 
    year(date) == 2021 ~ "midline", 
    year(date) > 2021 ~ "endline", 
    .default = "NA"
  ))

df_have_geocene_but_no_survey_data 
####################################################################################

#Checking which hh had started receiving LPG before we got the monitors in place 

df_events_stove_on %>% 
  group_by(study_arm_overall) %>% 
  filter(year(start_time) < 2020, lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>% 
  view()


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


#Finds 3 months from start of mission -- time when batteries had not died 
max_date_mission <- 
 df_events_stove_on %>% 
   group_by(mission_id) %>% 
   summarise(start = min(start_time)) %>% 
   mutate(max_date = lubridate::add_with_rollback(start, months(3))) %>%  #finds 3 months from start 
  select(-start)
   

#Restricts data to just those within 3 months of start 
df_events_stove_on_lt_3mo <- 
  df_events_stove_on %>% 
  left_join(max_date_mission, by = "mission_id") %>% 
  filter(stop_time < max_date) #filters all data less than 3 months from start 
  
#Code for checking that no longer any kept longer than 3 months 
  # group_by(mission_id) %>% 
  # summarise(min = min(start_time), max = max(stop_time)) %>% 
  # mutate(ch_time = difftime(max, min)) %>% 
  # arrange(desc(ch_time))

df_events_stove_on_lt_3mo %>% 
  group_by(fuel_type) 

### Checking data to make sure we believe it and adding in a stove use monitor 
df_events_stove_on_per_day <- #This calculates a daily number of stove uses per household per date 
  df_events_stove_on_lt_3mo %>%
  filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
  group_by(study_arm_overall, hh_id, fcn_id, fuel_type, date, first_receive_lpg_ymd, lpg_enrolled_and_receiving) %>% # study_arm_overall
  summarise(n = n(), stove_on_min_sum = sum(stove_on_min)) %>% 
  # filter(stove_on_min_sum > 1200) %>%   #This filters all stove use >20 hrs 
  # write_csv("stove_use_gt20hr_day.csv")
  # 
  
  # count number of cooking events for each fuel type
  pivot_wider(names_from = fuel_type, values_from = c(n, stove_on_min_sum)) %>%
  
#The following two lines of code turn all NA's to 0's am going to run again turning that off
   mutate(
    stove_on_min_sum_biomass = ifelse(!is.na(stove_on_min_sum_biomass), as.numeric(as.character(stove_on_min_sum_biomass)), NA),
    stove_on_min_sum_lpg = ifelse(!is.na(stove_on_min_sum_lpg), as.numeric(as.character(stove_on_min_sum_lpg)), NA)
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
    #days_after_first_receiving = ifelse(days_after_first_receiving < 0, -1, days_after_first_receiving), #is.na(days_after_first_receiving) 
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
  ) %>% 
  filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") #adding this to remove ones we know temp is bad for the ones we took before enrollement 
    #is temp also bad for comparison arms at baselne ... idk 


##################################
# Save the stove on min events per day dataset as it is the basis of all analysis for stove use
write_csv(df_events_stove_on_per_day, file_stove_on_day)



##############
### RF105 ANALYSIS
##############


#Differences before and after receiving LPG 

#Find longest time since receive tested 
max(df_events_stove_on_per_day$days_after_first_receiving)

#Fnd Number of hh included in the stove use analysis 
table(df_events_stove_on_per_day$hh_id)

#Number of total days included == the number of rows in the dataset 
  #7627
  
#Number of exclusive use days 

### LPG exclusive use days
df_events_stove_on_per_day %>% 
  #group_by(year(date)) %>% 
  # count(exclusive_lpg)
  summarise(
    b_use = sum(exclusive_biomass), lpg_use = sum(exclusive_lpg), mix_use = sum(mixed_use)
  )
  
total_days <- 6884+552+153

#Exclusive LPG days 
6884/total_days
#exclusive biomass days 
552/total_days
#mixed days 
153/total_days

### Use time and number of uses for each type of exclusive use day 
df_events_stove_on_per_day %>% 
  filter(exclusive_lpg ==1) %>% 
  summarise(
    n(),
    mean(cooking_events_with_lpg),
    mean(cooking_events_with_biomass),
    mean(stove_on_min_sum_lpg), 
    sd(stove_on_min_sum_lpg), 
    mean(stove_on_min_sum_biomass), 
    sd(stove_on_min_sum_biomass)) %>% 
  view()

df_events_stove_on_per_day %>% 
  filter(exclusive_biomass ==1) %>% 
  summarise(
    n(),
    mean(cooking_events_with_lpg),
    mean(cooking_events_with_biomass),
    mean(stove_on_min_sum_lpg), 
    sd(stove_on_min_sum_lpg), 
    mean(stove_on_min_sum_biomass), 
    sd(stove_on_min_sum_biomass)) %>% 
  view()
  
df_events_stove_on_per_day %>% 
  filter(mixed_use == 1) %>% 
  summarise(
    n(),
    mean(cooking_events_with_lpg),
    mean(cooking_events_with_biomass),
    mean(stove_on_min_sum_lpg), 
    sd(stove_on_min_sum_lpg), 
    mean(stove_on_min_sum_biomass), 
    sd(stove_on_min_sum_biomass)) %>% 
  view()

library(ggpubr)

### Test if amounts of time different between the two groups 

#1) Make dataset long
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


t.test(stove)


###### What do I ue for the denominator here bc exclusice use only counts based on stoves with monitors 





df_events_stove_on_per_day %>% 
  ggplot() +
  geom_histogram(aes(stove_on_min_sum_biomass))

df_events_stove_on_per_day %>% 
  mutate(timepoint = case_when(date < ymd("2020-05-01") ~ "baseline",
                              year(date) == 2022 ~ "endline",
                              .default = "midline"))  %>% 
  group_by(study_arm_overall, timepoint) %>% 
  filter(stove_on_min_sum_biomass > 0) %>% 
  summarise(
    n(),
    mean(stove_on_min_sum_biomass),
    median(stove_on_min_sum_biomass),
    sd(stove_on_min_sum_biomass))

df_events_stove_on_per_day %>% 
  mutate(timepoint = case_when(date < ymd("2020-05-01") ~ "baseline",
                              year(date) == 2022 ~ "endline",
                              .default = "midline"))  %>% 
  group_by(study_arm_overall, timepoint) %>% 
  filter(stove_on_min_sum_lpg > 0) %>% 
  summarise(
    n(),
    mean(stove_on_min_sum_lpg),
    median(stove_on_min_sum_lpg),
    sd(stove_on_min_sum_lpg))

df_events_stove_on_per_day %>% 
  mutate(timepoint = case_when(date < ymd("2020-05-01") ~ "baseline",
                              year(date) == 2022 ~ "endline",
                              .default = "midline"))  %>% 
  ggplot(aes(stove_on_min_sum_lpg, fill = study_arm_overall)) + 
  geom_histogram(position = "dodge") +
  facet_grid(vars(timepoint))
##This shows that from stove use monitors cook time seems to increase -- maybe just the processor that we are using 



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


df_events_stove_on_per_day_long %>% 
  mutate(timepoint = case_when(date < ymd("2020-06-01") ~ "baseline",
                               year(date) == 2022 ~ "endline",
                               .default = "midline"))  %>% 
  ggplot(aes(stove_on_min_sum, fill = study_arm_overall)) + 
  geom_histogram(position = "dodge") +
  facet_grid(vars(timepoint), vars(fuel_type))



df_events_stove_on_per_day %>% 
  filter(date< first_receive_lpg_ymd) %>% 
  view()




### LPG exclusiveuse households 
df_events_stove_on_per_day %>% 
  group_by(study_arm_overall, fcn_id) %>% 
  summarise(
   b_use = sum(exclusive_biomass), lpg_use = sum(exclusive_lpg), mix_use = sum(mixed_use)
  ) %>% 
  mutate(
  exclusive_lpg_hh = if_else(b_use == 0 & mix_use == 0, 1, 0)) %>% 
  ungroup() %>% 
  group_by(study_arm_overall) %>% 
  count(exclusive_lpg_hh)
  

#===============================================================================
# Determine percent of days that exclusivity of use by 
#===============================================================================

# get the number of days the hh was monitored
df_stove_exclusive_use_num_days_measured <-
	df_events_stove_on_per_day %>%
#	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
	group_by(fcn_id) %>%
	summarise(n = n ()) %>%
	arrange(n)

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

# df_events_stove_on_per_day %>%
# 	filter(fcn_id == "286053") %>%
# 	View()
	





# Define the number of colors you want
color_count <- 33
getPalette <- colorRampPalette(brewer.pal(8, "Set1"))


fig_df_stove_exclusive_use <-
	df_stove_exclusive_use_by_month %>%
	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
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
df_events_stove_on_per_day %>%
	filter(
		study_arm_overall == "intervention",
		date < as_date("2020-06-01"),
		stove_on_min_sum_lpg > 0
	) %>% view()
# 
# write_csv(df_exclusive_use_pre_intervention_in_intervention_arm, file_out_errors_2)

df_events_stove_on_per_day %>% filter(str_detect(hh_id, "^10")) %>% count(study_arm_overall) ### Need to change the 





#===============================================================================
# Number of stoves monitored each day before and after first receiving LPG (biomass vs LPG)
#===============================================================================

df_stove_monitored_per_day <-
	df_events_stove_on_per_day_long %>%
	filter(fcn_id %notin% c(df_have_geocene_but_no_survey_data_fcn_id)) %>%
	# filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program") %>%
	#select(fcn_id, fuel_type,days_after_first_receiving) 
	# unique() %>%
	# group_by(fuel_type,days_after_first_receiving) %>%
	# count()


#===============================================================================
# Determine average mintues of daily use
#===============================================================================

df_stove_monitored_per_day  %>%
	group_by(fuel_type,study_arm_overall) %>%
	summarise(mean = mean(stove_on_min_sum, na.rm = TRUE))
# Over all the stove tested on all the days (after 30 days since "receiving" since some didn't actually receive during this time), mean percent of time using biomass was 3.7 min and 93.5 min


fig_time_stove_on_per_day <-
	df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type","days_after_first_receiving")) %>%
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
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "days_after_first_receiving")) %>%
	filter(days_after_first_receiving > 30) %>%
	group_by(fuel_type) %>%
	summarise(mean = mean(stove_on_min_pc, na.rm = TRUE))
# Over all the stove tested on all the days (after 30 days since "receiving" since some didn't actually receive during this time), mean percent of time using biomass was 4.7% and lpg was 95.3%


fig_pc_time_stove_on_per_day <-
	df_days_8_plus_stoves_monitored %>%
	left_join(df_events_stove_on_per_day_long, by = c("fuel_type", "days_after_first_receiving")) %>%
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


### Time spent on each cooking method 
df_days_8_plus_stoves_monitored %>%
  left_join(df_events_stove_on_per_day_long, by = c("fuel_type","days_after_first_receiving")) %>% 
  group_by(fuel_type) %>% 
  filter(stove_on_min_sum > 0) %>% 
  summarise(
    n = n(),
    median(stove_on_min_sum, na.rm = TRUE), 
    max(stove_on_min_sum)
    )

df_days_stoves_monitored %>%
  left_join(df_events_stove_on_per_day_long, by = c("fuel_type","days_after_first_receiving")) %>% 
  group_by(fuel_type) %>% 
  filter(stove_on_min_sum > 0) %>% 
  ggplot()+
  geom_histogram(aes(stove_on_min_sum, fill = fuel_type), position = "dodge") + 
  labs(title = "Amount of time spent cooking by fuel type")


## Change in number of minutes stoves were used over time 


