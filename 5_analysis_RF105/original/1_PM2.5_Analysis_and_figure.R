################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong / Chris LeBoa 
# @Description: Firewood collection, cost of wood in the market, reasons for using wood

   #Look at line 92 adding in variable to demarkate LPG timing 
   #We clean the PM data at line 437  

# @Date: 221005
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

lpg_receive_date <- read_csv(here::here("2_data_raw/list_first_enrolled_lpg.csv"))
file_in_pm_data_clean_rds <- here::here("4_data/pm_data_clean.rds")
# source(here::here("3_data_cleaning/1.5_define_vector_columns.R")) #Pulls in all variable group names

## Chris
# source(here::here("1_config.R"))
#install.packages("geomtextpath")
library(tidyquant)
library(grid)
library(geomtextpath)
# library(R.utils)
# library(googledrive)



# https://cran.r-project.org/web/packages/MakefileR/vignettes/demo.html
# 
# Outdoor monitor placement 
# All the Outdoor monitors at the same place and same school at Intervention but
#Pre-Intervention At 2 Mosque(camp 8W and camp 10). All the Baseline outdoor monitor
#and midline outdoor monitor setup as the same location.Note that no different 
#location in school and Mosque. Always had collected one location PM 2.5 data.




#Input Files



### Output Files RDS 
# all_pm_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA/PATS+_Data_20210114.rds"

# # Did HAPEX at baseline and only 8 women at midline. Dropped HAPEX because some were damaged and it wasn't very accurate anyway
# all_hapex_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.rds"
# 
# all_hapex_data_path_csv <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.csv"


pm_data_min_rds <- here::here("4_data/pm_data_min.rds")

pm_data_nearest_min_rds <- here::here("4_data/pm_data_nearest_min.rds")

pm_data_hour_rds <- here::here("4_data/pm_data_hour.rds")

pm_data_nearest_hour_rds <- here::here("4_data/pm_data_nearest_hour.rds")

pm_data_min_csv <- here::here("4_data/pm_data_min.csv")

pm_data_nearest_min_csv <- here::here("4_data/pm_data_nearest_min.csv")

pm_data_hour_csv <- here::here("4_data/pm_data_hour.csv")

pm_data_nearest_hour_csv <- here::here("4_data/pm_data_nearest_hour.csv")


# Manually move the compiled file into the raw folder of the project
all_pm_data_path_rds_project <- here::here("2_data_raw/PATS+_Data_20220703.rds")

file_pm_data_hour_av_by_hh <- here::here("4_data/pm_data_hour_av_by_hh.rds")
# file_HAPEX_data_base <- here::here("2_data_raw", "HAPEX_Data_20220703.rds")


##############################################################################
# Read file
##############################################################################

pm_data_full <- 
  read_rds(file_in_pm_data_clean_rds) %>% #Read in data file of all PM2.5 data 
	mutate(date = as_date(date)) 

#To create teh fcn_id from the hh id we pull the last digits of the hh id 

pm_data_full <- pm_data_full %>% 	mutate(fcn_id = as.numeric(str_extract(hh_id, ".{6}$") ))

pm_data_full <-  
  pm_data_full %>% left_join(lpg_receive_date, by = "fcn_id") #bind data to when hh starteds receiving lpg


###### Edit dataset to add in the variable for who was a recent receipt 
pm_data_full <- 
  pm_data_full %>%  
  mutate(date = as.Date(dateTime)) %>% 
  #filter(date < first_receive_lpg_ymd ).             #This adds in a variable to determine LPG receipt from
  mutate(
    lpg_enrolled_and_receiving = case_when(
    date < first_receive_lpg_ymd ~ "not yet receiving LPG through distribution program", 
    date < "2020-04-15" & date > first_receive_lpg_ymd ~ "recent receipt lpg", 
    date > "2020-04-15" & date < "2021-01-15" ~ "midline", 
    date > "2022-01-15" ~ "endline", 
    .default = NA, 
    ), 
    study_arm_update = case_when(
      study_arm == "intervention" & lpg_enrolled_and_receiving == "not yet receiving LPG through distribution program" ~ "pre-receipt lpg", 
      study_arm == "intervention" & lpg_enrolled_and_receiving == "recent receipt lpg" ~ "recent receipt lpg", 
      study_arm == "intervention" & is.na(lpg_enrolled_and_receiving)  ~ "unknown receipt lpg", 
      .default = study_arm
    ))

################################################################################
## QC:  Detection of high monitor values
################################################################################
pm_data_full %>%
	filter(timepoint == "baseline") %>%
	filter(study_arm == "intervention") %>%
	ggplot(aes(dateTime, PM_Estimate, color = hh_id_note)) + # aes(nearest_min, PM_Estimate, color = hh_id_note)
	# geom_point(na.rm = TRUE) +
	geom_line() + 
	# 	geom_text(aes(label = hh_id, x = as_datetime(Inf), y = PM_Estimate), hjust = -.1) + # how do I do x = Inf for datetime
	# 	scale_colour_discrete(guide = 'none')  +    
	#   theme(plot.margin = unit(c(1,3,1,1), "lines")) +
	scale_x_datetime(labels = scales::date_format("%Y-%m-%d")) + # "%Y-%m-%d %H:%M" # labels = scales::date_format("%H:%M")
	theme(
		legend.position = "none"
	) + 		
	labs(
		title = "midline_intervention"
	) +
	facet_wrap(date ~ ., scales = "free")

pm_data_high_monitors <-
	pm_data_full %>%
	filter(PM_Estimate > 3000) %>%
	select(timepoint, study_arm, hh_id) %>%
	unique()

pm_data_very_high_monitors <-
	pm_data_full %>%
	filter(PM_Estimate > 10000) %>%
	select(timepoint, study_arm, hh_id) %>%
	unique()


###############################################################
## Create overall analysis dataset 
#############################################################

# drop the endline data that was during the cold season --> we'll analyze this separately
pm_data <-
	pm_data_full %>%
	filter(
	  !(date > as_date("2022-02-02") & date < as_date("2022-03-24")),
	  note %notin% c("qc", "insufficient data")
	  ) %>% 
  mutate(PM_Estimate = if_else(PM_Estimate < 10, 10, PM_Estimate )) #Replacing all # below the lower detection limit with 10 


# Sample size
# Now that we know the quality of the files is fine, determine the size of the data analysis sets
pm_data_hour_per_monitor <-
	pm_data %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note, dateTime_hour) %>%
	summarise_at(
		vars(PM_Estimate),
		list(mean),
		na.rm = TRUE
	) %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note) %>%
	count() %>%
	ungroup() %>%
	rename(hours_recording = n)

pm_data_hour_per_monitor %>%
	filter(note %notin% c("qc", "insufficient data")) %>%
	group_by(timepoint, study_arm) %>%
	summarise_at(
		vars(hours_recording),
		list(total_hours = sum, mean_hours = mean, median_hours = median)
	) %>%
	left_join(
		pm_data %>%
			filter(note %notin% c("qc", "insufficient data")) %>%
			distinct(timepoint, study_arm, hh_id) %>%
			group_by(timepoint, study_arm) %>%
			count(),
		by = c("timepoint", "study_arm")
	) %>%
	select(timepoint, study_arm, n, everything())



# Baseline
# Intervention group: n = 109, hr = 5709, mean = 52 hr/hh
# Comparison group: n = 63, hr = 3289, mean = 52 hr/hh
# Outdoor group: n = 3, hr = 856, mean = 285 hr/location
# 
# midline
# Intervention group: n = 48, hr = 2413, mean = 50.3 hr/hh
# Comparison group: n = 39, hr = 2046, mean = 52.5 hr/hh
# Outdoor group: n = 3, hr = 369, mean = 123 hr/location
# 
# endline if drop intervention between  > 2022-02-1 to < 2022-04-01 (last date collecte was 3-24)
# Intervention group: n = 32, hr = 1723, mean = 52.0 hr/hh
# Comparison group: n = 48, hr = 2541, mean = 52.9 hr/hh
# Outdoor group: n = 2, hr = 488, mean = 244 hr/location # were some of the outdoor mislabeled? should still only have 3 locations


# endline if drop intervention between  > 2022-02-1 to < 2022-03-15
# Intervention group: n = 58, hr = 3100, mean = 54.3 hr/hh
# Comparison group: n = 48, hr = 2541, mean = 52.9 hr/hh
# Outdoor group: n = 3, hr = 649, mean = 216 hr/location # were some of the outdoor mislabeled? should still only have 3 locations

# Create analysis datasets

# uses the actual minute (always rounds down to the nearest min)
pm_data_min <-
	pm_data %>%
	filter(note != "insufficient data") %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note, dateTime_min) %>%
	summarise_at(
		vars(PM_Estimate),
		list(mean),
		na.rm = TRUE
	) %>%
	ungroup()

# rounds to the nearest min (up or down)
pm_data_nearest_min <-
	pm_data %>%
	filter(note != "insufficient data") %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note, nearest_min) %>% # collapses all if they have the same date label, so I hope they don't!
	summarise_at(
		vars(PM_Estimate),
		list(mean),
		na.rm = TRUE
	) %>%
	ungroup()


# uses the actual hour (always rounds down to the nearest hour)
pm_data_hour <-
	pm_data %>%
	filter(note != "insufficient data") %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note, dateTime_hour) %>%
	summarise_at(
		vars(PM_Estimate),
		list(mean),
		na.rm = TRUE
	) %>%
	ungroup() %>%
	mutate(
		over_100 = if_else(PM_Estimate > 100, 1, 0),
		over_500 = if_else(PM_Estimate > 500, 1, 0)
	)

# rounds to the nearest hour (up or down)
pm_data_nearest_hour <-
	pm_data %>%
	filter(note != "insufficient data") %>%
	group_by(timepoint, study_arm, note, hh_id, hh_id_note, nearest_hour) %>%
	summarise_at(
		vars(PM_Estimate),
		list(mean),
		na.rm = TRUE
	) %>%
	ungroup() %>%
	mutate(
		over_100 = if_else(PM_Estimate > 100, 1, 0),
		over_500 = if_else(PM_Estimate > 500, 1, 0)
	)

###############################################################################
## Save data
###############################################################################

pm_data_min %>%
	saveRDS(pm_data_min_rds)

pm_data_nearest_min %>%
	saveRDS(pm_data_nearest_min_rds)

pm_data_hour %>%
	saveRDS(pm_data_hour_rds)

pm_data_nearest_hour %>%
	saveRDS(pm_data_nearest_hour_rds)

write.csv(pm_data_min, pm_data_min_csv)

write.csv(pm_data_nearest_min, pm_data_nearest_min_csv)

write.csv(pm_data_hour, pm_data_hour_csv)

write.csv(pm_data_nearest_hour, pm_data_nearest_hour_csv)


#############################################
#Figure of PM data 
##############################################

# Step 1: Aggregate data by 60-minute intervals and calculate means and standard errors
pm_summary <- 
  pm_data %>%
  mutate(nearest_min_60 = floor_date(nearest_min, "60 minutes")) %>%  # Round to nearest 10-minute interval
  group_by(timepoint, study_arm_update, nearest_min_60) %>%
  summarise(
    PM_Estimate_av = mean(PM_Estimate, na.rm = TRUE),
    se_pm_estimate = sd(PM_Estimate, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    lower_ci = PM_Estimate_av - 1.96 * se_pm_estimate,
    upper_ci = PM_Estimate_av + 1.96 * se_pm_estimate
  )

# Step 2: Create the plot with geom_smooth and error bars
fig_pm_day <- 
  ggplot(pm_summary, aes(x = nearest_min_60, y = PM_Estimate_av, group = study_arm_update)) +
  geom_line(aes(color = study_arm_update), linewidth = 1) +  # Smooth the line without showing its own confidence interval
  # geom_point(alpha = 0.5) + # Optional: Uncomment if you'd like points on the graph
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = study_arm_update), alpha = 0.4) +
  
  # Horizontal reference lines
  geom_hline(yintercept = 35, color = "red", linetype = 2) +
  geom_hline(yintercept = 75, color = "black", linetype = 4) +
  geom_hline(yintercept = 500, color = "black", linetype = 6) +
  
  # Improved x-axis readability
  scale_x_datetime(
    labels = scales::date_format("%H:%M", tz = "Asia/Dhaka"),  # Format to show hour and minute
    breaks = scales::date_breaks("6 hours"),                   # Increase interval between labels
    expand = c(0, 0)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels for readability
  
  # Y-axis scaling
  scale_y_log10(breaks = c(10, 25, 100, 300, 500, 1000), labels = c(10, 25, 100, 300, 500, 1000)) +
  annotation_logticks() +
  
  # Labels and theme
  labs(
    title = "PM2.5 measurement by hour of the day",
    x = "Time",
    y = "PM2.5 (ug/m3)"
  ) +
  theme_classic() +
  theme(
    legend.background = element_blank(),
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(color = "black", fill = NA),  # Adds a black border to each facet panel
    strip.background = element_blank()   
  ) +
  
  # Faceting by timepoint
  facet_wrap(~ timepoint)

fig_pm_day ## This produces rthe graphs by time of day 

ggsave(
	here::here("6_figures", "PM_by_hour_of_day_log_smooth_span_0.3.png"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "png"
)




################################
### Temperature analysis 
##############################

temp_data_hour_av_by_hh <-
	pm_data_full %>% 
	arrange(timepoint) %>%
#	group_by(timepoint) %>% 
	# Take only the first 48 hours of data for each hh (so that we don't have some hours that are measured more than twice)
	#slice(n = 1:49) %>% 
	summarise(
	#	hours_monitored = as.numeric(max(dateTime_hour) - min(dateTime_hour)) * 24,
		Temp_Estimate_48_hr_mean = mean(degC_air), 
		Temp_Estimate_48_hr_max = max(degC_air), 
		Temp_Estimate_48_hr_sd = sd(degC_air),
	# 95th and 75th percentiles
	Temp_Estimate_48_hr_95th = quantile(degC_air, 0.95),
	Temp_Estimate_48_hr_75th = quantile(degC_air, 0.75)
	) %>% 
	ungroup() %>% 
  view()

### 95% of daily max 

pm_data_full %>% 
  group_by(timepoint, date) %>%
  summarise(max_day_temp = max(degC_air)) %>% 
  ungroup() %>% 
 # group_by(timepoint) %>% 
  #	group_by(timepoint) %>% 
  # Take only the first 48 hours of data for each hh (so that we don't have some hours that are measured more than twice)
  #slice(n = 1:49) %>% 
  summarise(
    Temp_Estimate_48_hr_95th = quantile(max_day_temp, 0.95),
    Temp_Estimate_48_hr_75th = quantile(max_day_temp, 0.75)
  ) %>% 
  ungroup() %>% 
  view()


######################################
## Temperature Analysis 
#####################################

# Calculate mean for each group
means <- pm_data_full %>%
  filter(note %notin% c("qc", "insufficient data")) %>%
  group_by(study_arm_update, timepoint) %>%
  summarise(mean_temp = mean(degC_air), 
            median_temp = median(degC_air)) 



temp_table <- 
  pm_data %>% 
  filter(note %notin% c("qc", "insufficient data")) %>%
	ggplot(aes(y = degC_air, x = timepoint, fill = study_arm_update)) +
  geom_boxplot() + 
  # geom_vline(data = means, aes(xintercept = mean_value, color = group), 
  #           linetype = "dashed", size = 1) +
	#annotate( "text", x = 31, y = 10000, label = "29.8") +
	labs(title = "Temp distribbution and mean", x = "Timepoint", y = "Temperature deg. C") 

######################################
## Pm2.5 Analysis 
#####################################

# Calculate mean for each group
pm2.5_table<- pm_data %>%
  filter(note %notin% c("qc", "insufficient data")) %>%
  group_by(study_arm_update, timepoint) %>%
  summarise(mean_pm2.5 = mean(PM_Estimate), 
            median_pm2.5 = median(PM_Estimate)) 



pm_data %>% 
  filter(note %notin% c("qc", "insufficient data")) %>%
  ggplot(aes(y = PM_Estimate, x = timepoint, fill = study_arm_update)) +
  geom_boxplot() + 
  # geom_vline(data = means, aes(xintercept = mean_value, color = group), 
  #           linetype = "dashed", size = 1) +
  #annotate( "text", x = 31, y = 10000, label = "29.8") +
  labs(title = "PM2.5 distribbution", x = "Timepoint", y = "PM2.5 ug/m^3") 




#################################################################################
#Percentage of data points at different thresholds by study arm 
  #Give the % of data points that fall below 75, 50, 35
  #I need to use pm_data instead of pm_data_full to remove cold times because different season for some of data 

pm_data_full %>% 
  filter(note %notin% c("qc", "insufficient data")) %>%
  mutate(
    pm_u400 = if_else(PM_Estimate <400, 1, 0), 
    pm_u150 = if_else(PM_Estimate <150, 1, 0), 
    pm_u75 = if_else(PM_Estimate <75, 1, 0), 
    pm_u50 = if_else(PM_Estimate <50, 1, 0), 
    pm_u35 = if_else(PM_Estimate <35, 1, 0), 
  ) %>% 
  group_by(timepoint, study_arm_update) %>% 
  summarise(
    count_measure = n(), 
    pct_u_400 = sum(pm_u400) / count_measure, 
    pct_u_150 = sum(pm_u150) / count_measure, 
    pct_u_75 = sum(pm_u75) / count_measure, 
    pct_u_50 = sum(pm_u50) / count_measure,
    pct_u_35 = sum(pm_u35) / count_measure,
  )
  
pm_data %>% # 1,039,924 rows of data (each is every 10 seconds)
  filter(note %notin% c("qc", "insufficient data")) %>% 
  # Each hh will get get only one observation, which is the average of the PM at that min for however many days they were measured
  group_by(timepoint, study_arm_update, hh_id, nearest_min) %>%
  summarise(PM_Estimate_av = mean(PM_Estimate, na.rm = TRUE)) %>%
  mutate(
    pm_u400 = if_else(PM_Estimate_av <400, 1, 0), 
    pm_u150 = if_else(PM_Estimate_av <150, 1, 0), 
    pm_u75 = if_else(PM_Estimate_av <75, 1, 0), 
    pm_u50 = if_else(PM_Estimate_av <50, 1, 0), 
    pm_u35 = if_else(PM_Estimate_av <35, 1, 0), 
  ) %>% 
  ungroup() %>% 
  group_by(timepoint, study_arm_update) %>% 
  summarise(
    count_measure = n(), 
    pct_u_400 = sum(pm_u400) / count_measure, 
    pct_u_150 = sum(pm_u150) / count_measure, 
    pct_u_75 = sum(pm_u75) / count_measure, 
    pct_u_50 = sum(pm_u50) / count_measure,
    pct_u_35 = sum(pm_u35) / count_measure,
  )

#################################################
## Data Check for high readings 
#####################################

pm_data %>% group_by(study_arm_update, timepoint) %>% distinct(hh_id) %>% count(timepoint)

pm_data_results <- 
  pm_data %>% # 1,039,924 rows of data (each is every 10 seconds)
  filter(note %notin% c("qc", "insufficient data")) %>% 
  # Each hh will get get only one observation, which is the average of the PM at that min for however many days they were measured
  #group_by(timepoint, study_arm_update, hh_id, nearest_min) %>%
  # summarise(PM_Estimate_av = mean(PM_Estimate, na.rm = TRUE),
  #           median_PM_Estimate_av = median(PM_Estimate, na.rm = TRUE)) %>%
  mutate(
    pm_u50000 = if_else(PM_Estimate >50000, 1, 0), 
    pm_u30000 = if_else(PM_Estimate >30000, 1, 0), 
    pm_75 = if_else(PM_Estimate > 75, 1, 0), 
    pm_65 = if_else(PM_Estimate > 65, 1, 0), 
    pm_35 = if_else(PM_Estimate >35, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(timepoint, study_arm_update) %>% 
  summarise(
    count_measure = n(), 
    mean = mean(PM_Estimate), 
    sd(PM_Estimate), 
    median(PM_Estimate),
    IQR(PM_Estimate), 
    min(PM_Estimate), 
    max(PM_Estimate), 
    pm_ab35 = sum(pm_35) / count_measure,
    pm_ab65 = sum(pm_65) / count_measure,
    pm_ab75 = sum(pm_75) / count_measure
  ) %>% 
  mutate(se = `sd(PM_Estimate)` / sqrt(count_measure), 
         degrees.freedom = count_measure - 1, 
         t.score = qt(p=.05/2, df=degrees.freedom,lower.tail=F), 
         margin.error = t.score * se,
         lower.bound = mean - `margin.error`,
         upper.bound = mean + `margin.error`, 
         )

write_csv(pm_data_results, "pm_data_results_241103.csv")

pm_data_results %>% 
  view()

## Email dated 10/27 Agreed that the distribution looked ok and suggested the following 
#1/ For reporting, I'd report means, SDs, medians, IQRs, min, and max. Enables the reader to make some conclusions about the shape of the distribution. For comparison with standards, use the means. 
#2/ My prior response sounds good on limits. Anything <10 should be set to 10. If you are not seeing anything above 50, don't set an upper ceiling. You can do some sensitivity analyses where you trim the top 2.5% of values and re-run your models to see if things change. The mean values in your table look reasonable. 
#One additional idea: you could subset to device-days with values > 10mg/m3 or 20mg/m3 and visually inspect a few traces to check for evidence of saturation (e.g. pm values go up up and then plateau and stay that way and then drop down). 
#3/ The standard comparison choice is complicated. If you claim potential health benefits, those are all estimated based on longer-term exposures, not short-term 24h exposures. I would thus compare with the annual standard. You are in some ways assuming your measurements represent longer-term trends. 

