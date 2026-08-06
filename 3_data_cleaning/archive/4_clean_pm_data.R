################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Clean the PATS+ PM2.5 data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

################################################################################

#Input Files
all_pm_data_path_rds_project <- here::here("2_data_raw/PATS+_Data_20220703.rds")



### Output Files RDS 
file_out_pm_data_not_censored_raw <- here::here("4_data/pm_data_not_censored_raw.rds")
file_out_pm_data_not_censored <- here::here("4_data/pm_data_not_censored.rds")

file_out_pm_data_base <- here::here("4_data/pm_data_base.rds")
file_out_pm_data_clean_rds <- here::here("4_data/pm_data_clean.rds")
file_out_pm_data_qc_rds <- here::here("4_data/pm_data_qc.rds")

################################################################################

all_pm_data_base <- readRDS(file = all_pm_data_path_rds_project)


################################################################################

# Still not clean, must look at google sheet for other adjustments that need to be made. 
#
################################################################################



all_pm_data <- 
	all_pm_data_base %>%
	filter(V_power > 3.6) # If the battery voltage is not more than 3.6, the PM manual notes that the reading may not be accurate -> only keep values with V > 3.6




pm_data_not_censored_raw <- 
	all_pm_data %>% 
	
	# some files in midline and endline are in the form of "68_PM07745X_20220316_0_10GG09111956" rather than "PM07745X_20220316_0_10GG09111956" - not sure why Jamil labeled them like this.
	
	mutate(file_name = str_remove(file_name, "^[^_]{2,3}_")) %>%
	
	separate(file_name, into = c("PM_monitor", "date", "study_arm", "hh_id_note"), sep = "_", extra = "merge", remove = FALSE) 
# for some reason if I sep into five columns right away, those with no "note" are missing a study arm

saveRDS(pm_data_not_censored_raw, file_out_pm_data_not_censored_raw)

# pm_data_not_censored_raw %>%
# 	select(hh_id_note) %>%
# 	unique() %>%
# 	arrange() %>% View()

pm_data_not_censored <-
	pm_data_not_censored_raw %>%
	
	# There is an "error" because some file names don't have "QC" after the test name - the space where "QC" would be is replaced with an NA. This is good because the files were actually not "QC" file - NA is how they should be marked!
	mutate(
		
		dateTime = 
			if_else(
				is.na(ymd_hms(dateTime)) == FALSE, 
				ymd_hms(dateTime),  # 2019-09-14 07:22:20 yy-mm-dd
				mdy_hm(dateTime)    # 10/1/2020 12:51 mm/ddy/yyy
			),
		dateTime_min = round_date(dateTime, unit = "minute"),
		dateTime_min_dhaka = as.POSIXct(dateTime_min, tz = "Asia/Dhaka"), # + 6 * 60 * 60, # , tz = "Asia/Dhaka"
		dateTime_hour = round_date(dateTime, unit = "hour"),
		dateTime_hour_dhaka = as.POSIXct(dateTime_hour, tz = "Asia/Dhaka"), #+ 6 * 60 * 60, # , tz = "Asia/Dhaka"
		
		timepoint = ifelse(dateTime < "2020-01-01", "baseline", ifelse(dateTime > "2022-01-01", "endline", "midline")), # midline started 201001 and endline started 220203
		
		timepoint = 
			ordered(
				timepoint,
				levels = c("baseline", "midline", "endline")
			),
		
		# 4FPP11_school #baseline
		hh_id_note = ifelse(hh_id_note %in% c("4FPP11_School", "4FPP11_school"), "4EPP11_school", hh_id_note),
		
		# 8WDI18 should be 8WI18; xxx_mosjid should be xxx_mosque instead of mosjid #endline
		hh_id_note = ifelse(hh_id_note %in% c("8WI18_Mosjid", "8WI18_mosjid", "8WI18_Mosque", "8WI18_mosque"), "8WI18_mosque", hh_id_note),
		hh_id_note = ifelse(hh_id_note %in% c("8WDI18_Mosjid", "8WDI18_mosjid", "8WDI18_Mosque", "8WDI18_mosque"), "8WI18_mosque", hh_id_note),
		
		
		# 10D12_mosque is the old block name for 10GG9_mosque  #endline
		# note = ifelse(timepoint == "baseline" & hh_id_note == "10D12_mosque", "mosque", note),
		hh_id_note = ifelse(hh_id_note %in% c("10D12_Mosque", "10D12_mosque", "10DD12_Mosque", "10DD12_mosque", "10GG09_mosque"), "10GG9_mosque", hh_id_note)
		
		
		
	) %>%
	separate(hh_id_note, into = c("hh_id", "note"), sep = "_", extra = "merge", remove = TRUE) %>% # I do want a variable hh_id_note but first clean up the case (str_to_lower) of the note then recombine.
	mutate(
		note = 
			if_else(is.na(note), "normal", str_to_lower(note)),
		study_arm =
			# if_else(note %in% c("school", "mosque"), "outdoor", study_arm),
		if_else(note %in% c("school"), "outdoor_comparison", if_else(note %in% c("mosque"), "outdoor_intervention"), study_arm), # the school was the outdoor location in the comparison site and the mosques were the outdoor  location in the intervention site 
		study_arm =
			ordered(
				as.character(study_arm),
				levels = c("0", "1", "I", "outdoor"), # during endline, Jamil started labelling the files from the hh that had stoves as baseline "I" instead of "1"
				labels = c("intervention", "comparison", "comparison", "outdoor")
			)
	) %>%
	unite(col = "hh_id_note", c(hh_id, note), remove = FALSE) %>%
	# # Using separate makes the resulting time into characters, which is not what I want; 
	# # using hms to convert them out of character format produced a period, not a time of day
	# separate(dateTime_min, into = c("dateTime_min_extraDate", "nearest_min"), sep = " ", remove = FALSE) %>%
	# separate(dateTime_hour, into = c("dateTime_hour_extraDate", "nearest_hour"), sep = " ", remove = FALSE) %>%
	
	# Instead, keep the values in POSIXct and use trunc to chop off the days
	mutate(
		nearest_min =  as.POSIXct(paste("2000-01-01", paste(hour(ymd_hms(dateTime_hour)), minute(ymd_hms(dateTime_min)), sep = ":")), "Asia/Dhaka"),
		nearest_hour = as.POSIXct(paste("2000-01-01", paste(hour(ymd_hms(dateTime_hour)), "00", sep = ":")), tz = "Asia/Dhaka")
	) %>%
	
	select(timepoint, study_arm, hh_id, note, hh_id_note, dateTime, dateTime_min, nearest_min, dateTime_hour, nearest_hour, everything())


################################################################################


# I can't figure out what is not parsing.

# 4: Expected 2 pieces. Missing pieces filled with `NA` in 1356806 rows [3564, 3565, 3566, 3567, 3568, 3569, 3570, 3571, 3572, 3573, 3574, 3575, 3576, 3577, 3578, 3579, 3580, 3581, 3582, 3583, ...]. # pm_data_not_censored %>% slice(3570) %>% View()
# 
# pm_data_not_censored %>%
# 	select(dateTime, dateTime_hour, dateTime_min)
# 
# pm_data_not_censored_raw  %>% 
# 	# filter(is.na(dateTime)) %>%
# 	select(dateTime) %>%
# 	unique() %>%
# 	View()

################################################################################
saveRDS(pm_data_not_censored, file_out_pm_data_not_censored)



# Censor data
# https://berkeleyair.com/monitoring-instruments-sales-rentals/particle-and-temperature-sensor-pats/
# 	Lower particulate matter detection limit (PM2.5): 10 to 20 μg/m3
# Upper particulate matter detection limit (PM2.5): 30,000 to 50,000 μg/m3
# 
# --> exclude anything below 20 ug/m3 and above 30,000 ug/m3
# --> currently excluding anything below 10 ug/m3 and above 50,000 ug/m3

# Advice from Ajay 5 July 2022 via email to lakwong@berkeley.edu
# Typically we’d estimate an LOD for gravimetric samples (based on the SD of blank mass depositions) and apply this cutoff as the LOD for the monitoring campaign (sometimes sliced up by country, stove type, etc). If you have collocated filters, that is likely the way to go. 
# 
# If not, you could use the BAMG recommendation of 10µg/m3 and replace any values <10 with 10. That value (~10) was determined by, as I recall, exposing the sensor to "particle free" air (HEPA filtered), evaluating noise, and using a standard LOD metric. For higher values, it might be best to first estimate the frequency of values greater than 30 and 50 mg/m3 for your sensor runs. Values above 50 are plausible, but run into issues with the sensor’s response to pollution (it becomes somewhat non-linear at those levels). 
# 
# If you have a lot of values > 50 mg/m3, you could compare how averages react to (1) removing those values, (2) replacing them with 50, or (3) leaving as is. 

pm_data_base %>% 
	ggplot() +
	geom_histogram(aes(x = PM_Estimate)) + 
	coord_cartesian(ylim = c(0, 100))

pm_data_base %>%
	filter(PM_Estimate > 49000) %>%
	count()

# There are 486 of 819,041 measurements that are > 3000 ppm
# There are 15 of 819,041 measurements that are > 4900 ppm

# There are so few of these very high measurements that they are probably mistakes / true but momentary so I will remove them so they don't distort the averages. 

pm_data_base <-
	pm_data_not_censored %>% #	read_rds(here::here("4_data/pm_data_not_censored.rds")) %>%
	mutate(PM_Estimate = ifelse(PM_Estimate < 10, 10, PM_Estimate)) %>% 
	mutate(PM_Estimate = ifelse(PM_Estimate >30000, 30000, PM_Estimate))   # use 10 ug/m3 as lower limit and 30000 ug/m3 as upper limit; https://berkeleyair.com/platform-for-integrated-cookstove-assessment/
# The PM monitor max is 50,000 ppm; anything above this is a fluke. Checked with Allie Sherris and she said that sometime when smoke directly hits the monitor the PM reading can go above 50,000

saveRDS(pm_data_base, file_out_pm_data_base)







################################################################################

# Check data quality

################################################################################

# ## Insufficient data
# Were any hh recorded for less than 48 hours but not recorded as "Insufficient data"?


pm_data_hour_per_monitor_pre_revision <-
	pm_data_base %>%
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

pm_data_hour_per_monitor_pre_revision %>%
	filter(timepoint == "baseline") %>%
	filter(hours_recording < 24) %>%
	arrange(hours_recording)

pm_data_hour_per_monitor_pre_revision %>%
	filter(timepoint == "midline") %>%
	# ggplot(aes(x = hours_recording))+
	# geom_histogram(binwidth = 5)
	filter(hours_recording < 24) %>%
	arrange(hours_recording)

pm_data_hour_per_monitor_pre_revision %>%
	filter(timepoint == "endline") %>%
	# ggplot(aes(x = hours_recording))+
	# geom_histogram(binwidth = 5)
	filter(hours_recording < 24) %>%
	filter(note != "qc") %>%
	arrange(hours_recording)

#20 of the 174 observations are not qc or school and <24 hours - not sure if we should throw these out like we did for baseline or not; for now I will NOT toss them out

# baseline
# timepoint study_arm    note              hh_id        hh_id_note                     hours_recording
# <ord>     <ord>        <chr>             <chr>        <chr>                                    <int>
# 	1 baseline  intervention normal            10G108351    10G108351_normal                             7
# 2 baseline  comparison   normal            4E277068     4E277068_normal                              8
# 3 baseline  intervention normal            8wDI21101274 8wDI21101274_normal                         10
# 4 baseline  intervention qc                8wDI20101777 8wDI20101777_qc                             13
# 5 baseline  intervention insufficient data 8wDH21291216 8wDH21291216_insufficient data              14 # actually this is okay but I don't have time to change it right now
# 6 baseline  intervention qc                10G108169    10G108169_qc                                14
# 7 baseline  comparison   normal            4E277017     4E277017_normal                             14
# 8 baseline  intervention normal            10G193127    10G193127_normal                            16
# 9 baseline  intervention normal            8wDI21101459 8wDI21101459_normal                         18
# 10 baseline  intervention normal            8wDI21101214 8wDI21101214_normal                         19
# 11 baseline  intervention normal            10G192754    10G192754_normal                            21
# 12 baseline  intervention normal            8wDI20101392 8wDI20101392_normal                         21
# 13 baseline  comparison   normal            4E175475     4E175475_normal                             21
# 14 baseline  intervention normal            10G108132    10G108132_normal                            22


# # midline
# timepoint study_arm    note   hh_id        hh_id_note          hours_recording
# <ord>     <ord>        <chr>  <chr>        <chr>                         <int>
# 	1 midline   comparison   normal 4EPP15184706 4EPP15184706_normal               2
# 2 midline   comparison   normal 4EPP11277012 4EPP11277012_normal               9
# 3 midline   comparison   normal 4EPP10188395 4EPP10188395_normal              11
# 4 midline   intervention normal 8WDI20101573 8WDI20101573_normal              12
# 5 midline   comparison   normal 4EPP10280792 4EPP10280792_normal              13
# 6 midline   comparison   normal 4EPP15175477 4EPP15175477_normal              16
# 7 midline   comparison   normal 4EPP12500391 4EPP12500391_normal              19
# 8 midline   comparison   normal 4EPP15152796 4EPP15152796_normal              19
# 9 midline   intervention normal 10F40181413  10F40181413_normal               20
# 10 midline   intervention normal 8WDI20101415 8WDI20101415_normal              20
# 11 midline   comparison   normal 4EPP11302711 4EPP11302711_normal              20
# 12 midline   intervention normal 10F40201621  10F40201621_normal               22
# 13 midline   intervention normal 8WDI18101172 8WDI18101172_normal              22
# 14 midline   comparison   normal 4EPP11184266 4EPP11184266_normal              22
# 15 midline   comparison   normal 4EPP10184125 4EPP10184125_normal              23
# 16 midline   comparison   normal 4EPP15175472 4EPP15175472_normal              23
# 17 midline   comparison   normal 4EPP15175475 4EPP15175475_normal              23


# endline
# timepoint study_arm    note   hh_id        hh_id_note          hours_recording
# <ord>     <ord>        <chr>  <chr>        <chr>                         <int>
# 	1 endline   intervention normal 10HH55286069 10HH55286069_normal              23
# 2 endline   comparison   normal 4EPP11277068 4EPP11277068_normal              23
# 3 endline   intervention normal 10FF25287192 10FF25287192_normal              22
# 4 endline   intervention normal 9GG29123657  9GG29123657_normal               22
# 5 endline   intervention normal 8WDI21101493 8WDI21101493_normal              21
# 6 endline   intervention normal 9GG29115371  9GG29115371_normal               21
# 7 endline   comparison   normal 4EPP12283841 4EPP12283841_normal              21
# 8 endline   comparison   normal 4EPP12283059 4EPP12283059_normal              20
# 9 endline   intervention normal 8WDH21291234 8WDH21291234_normal              19
# 10 endline   comparison   normal 4EPP13179585 4EPP13179585_normal              19
# 
# 1 endline   comparison   normal 4FUU14293862 4FUU14293862_normal               6
# 2 endline   intervention normal 10FF31201201 10FF31201201_normal               7
# 3 endline   comparison   normal 4EPP11302711 4EPP11302711_normal               7
# 4 endline   comparison   normal 4EPP15175472 4EPP15175472_normal               8
# 5 endline   intervention normal 9GG29123665  9GG29123665_normal               10
# 6 endline   intervention normal 9GG29115465  9GG29115465_normal               11
# 7 endline   comparison   normal 4FUU14296441 4FUU14296441_normal              11
# 8 endline   intervention normal 9GG29123570  9GG29123570_normal               14
# 9 endline   intervention normal 9GG29123847  9GG29123847_normal               15
# 10 endline   comparison   normal 4EPP11286097 4EPP11286097_normal              15
# 11 endline   intervention  normal 9GG29123656 9GG29123656_normal              18
# 12 endline   comparison   normal  4EPP15175475 4EPP15175475_normal            18



# Only exclude hh data if hr recorded was < 12 hours (this is arbitrary), (thought about 10 hr, also arbitrary, there is only 1 hh that has 11 hr)
# <12 hr
# 
# hh_id with <12 hours of data
# baseline: 10G108351_normal, 4E277068_normal, 8wDI21101274_normal
# midline: 4EPP15184706_normal, 4EPP11277012_normal, 8WDI20101573_normal
# endline: 4FUU14293862_normal, 10FF31201201_normal, 4EPP11302711_normal, 4EPP15175472_normal, 9GG29123665_normal, 9GG29115465_normal, 4FUU14296441_normal





# Label files will insufficient data
pm_data <- 
	pm_data_base %>%
	mutate(
		
		# Baseline (16 hh insufficient <24 hours; do not remove the qc and school that are <24 hours)
		# # 10G108351_normal
		note = ifelse(timepoint == "baseline" & hh_id_note == "10G108351_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "baseline" & hh_id_note == "10G108351_normal", "10G108351_insufficient data", hh_id_note),
		# # 8wDI21101274_normal
		note = ifelse(timepoint == "baseline" & hh_id_note == "8wDI21101274_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "baseline" & hh_id_note == "8wDI21101274_normal", "8wDI21101274_insufficient data", hh_id_note),
		# # 4E277068_normal
		note = ifelse(timepoint == "baseline" & hh_id_note == "4E277068_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "baseline" & hh_id_note == "4E277068_normal", "4E277068_insufficient data", hh_id_note),
		
		
		
		# midline
		# # 4EPP15184706_normal - less than 24 hr
		note = ifelse(timepoint == "midline" & hh_id_note == "4EPP15184706_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "midline" & hh_id_note == "4EPP15184706_normal", "4EPP15184706_insufficient data", hh_id_note),
		# # 4E188395_normal - less than 24 hr
		note = ifelse(timepoint == "midline" & hh_id_note == "4EPP11277012_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "midline" & hh_id_note == "4EPP11277012_normal", "4EPP11277012_insufficient data", hh_id_note),
		# # 44E302711_normal  - less than 24 hr
		note = ifelse(timepoint == "midline" & hh_id_note == "8WDI20101573_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "midline" & hh_id_note == "8WDI20101573_normal", "8WDI20101573_insufficient data", hh_id_note),

		
		# endline
		# 		# 4FUU14293862_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "4FUU14293862_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "4FUU14293862_normal", "4FUU14293862_insufficient data", hh_id_note),
		# 		# 10FF31201201_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "10FF31201201_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "10FF31201201_normal", "10FF31201201_insufficient data", hh_id_note),
		# 		# 4EPP11302711_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "4EPP11302711_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "4EPP11302711_normal", "4EPP11302711_insufficient data", hh_id_note),
		# 		# 4EPP15175472_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "4EPP15175472_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "4EPP15175472_normal", "4EPP15175472_insufficient data", hh_id_note),
		# 		# 9GG29123665_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "9GG29123665_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "9GG29123665_normal", "9GG29123665_insufficient data", hh_id_note),
		# 		# 9GG29115465_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "9GG29115465_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "9GG29115465_normal", "9GG29115465_insufficient data", hh_id_note),
		# 		# 4FUU14296441_normal
		note = ifelse(timepoint == "endline" & hh_id_note == "4FUU14296441_normal", "insufficient data", note),
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "4FUU14296441_normal", "4FUU14296441_insufficient data", hh_id_note),
		
		# other problem to fix
		# 10GG09
		
		hh_id_note = ifelse(timepoint == "endline" & hh_id_note == "10GG09_mosque", "10GG9_mosque", hh_id_note),
		hh_id = ifelse(timepoint == "endline" & hh_id_note == "10GG09_normal", "10GG9", hh_id)
	)


# Baseline: 8WI18 14-16 Sept; 17-19 Sept; 21-23 Sept, 24-26 Sept 2019
# Baseline: 10GG9 12-14 Oct; 15-17 Oct; 20-22 Oct; 23-25 Oct, 27-29 Oct 2019
# midline: 4EPP11 1-3 Oct, 4-6 Oct, 7-9 Oct, 10-12 Oct, 13-15 Oct, 20-22 Oct, 24-26 Oct, 27-29 Oct 2020

# pm_data %>%
# 	mutate(
# 			PM_Estimate = 
# 			case_when(
# 				dateTime > "2019-09-27" & dateTime < "2020-09-01" & hh_id == "8WI18" ~ 0,
# 				dateTime > "2019-10-30" & dateTime < "2020-09-01" & hh_id == "10GG9" ~ 0,
# 			 dateTime > "2020-10-30" & hh_id == "4EPP11" ~ 0
# 		)
# 	) %>%
# 	filter(hh_id %in% c("8WI18", "10GG9", "4EPP11")) %>%
# 	arrange(desc(dateTime))

# pm_data %>%
# 	filter(hh_id %in% "8WI18", dateTime > "2019-09-26") %>%
# 	select(dateTime, PM_Estimate)


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
	filter(hours_recording < 24)




### File naming?
# Are files named correctly? (above we fixed names for "insufficient data" here check "qc" - are qc files named qc, not qc files not qc?)

# 40 QC files
hhid_with_qc <- 
	pm_data %>%
	filter(note == "qc") %>%
	distinct(hh_id)

# pm_data_not_censored %>%
# 	filter(hh_id %in% hhid_with_qc$hh_id) %>%
# 	filter(hh_id == "4E179029") %>%
#   select(note) %>% unique()
# 	View()

# 4E179029 is labeled as "qc" but there's no accompanying file to be the not qc file, so rename 4E179029_qc to 4E179029

pm_data <- 
	pm_data %>%
	mutate(
		note = ifelse(hh_id_note == "4E179029_qc", "normal", note),
		hh_id_note = ifelse(hh_id_note == "4E179029_qc", "4E179029_normal", hh_id_note)
	)





# Now how many QC files are there? 39 files
pm_data_qc_files <-
	pm_data %>%
	filter(note == "qc") %>%
	distinct(timepoint, hh_id, PM_monitor)



saveRDS(pm_data, file_out_pm_data_clean_rds)

saveRDS(pm_data_qc_files, file_out_pm_data_qc_rds)




