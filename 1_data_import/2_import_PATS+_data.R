################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: merge different versions of baseline and endline hh survey data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

################################################################################

#Input Files
list_of_file_path_pats_intervention_baseline <- "H:/.shortcut-targets-by-id/1Ed4fav2PP395AS4U4w0aAZnBCEUym8XG/ALL DATA_BASELINE_2020/Intervention/PATS+"
# list_of_file_path_pats_intervention_baseline <- here::here("2_data_raw/ALL DATA_BASELINE_2020_220703/Intervention/PATS+")

list_of_file_path_pats_comparison_baseline <- "H:/.shortcut-targets-by-id/1Ed4fav2PP395AS4U4w0aAZnBCEUym8XG/ALL DATA_BASELINE_2020/Comparison/PATS+"


list_of_file_path_pats_intervention_midline <- "H:/.shortcut-targets-by-id/1TPfOuuXHpcynwDGFP8R-HFUesipMSRAO/ALL DATA_MIDLINE_2021/Intervention/PATS+"


list_of_file_path_pats_comparison_midline <- "H:/.shortcut-targets-by-id/1TPfOuuXHpcynwDGFP8R-HFUesipMSRAO/ALL DATA_MIDLINE_2021/Comparison/PATS+"

list_of_file_path_pats_intervention_endline <- "H:/.shortcut-targets-by-id/16aNynESRbG2uyimkLnjz3Fr4L-fWtFa6/ALL DATA_ENDLINE_2022/Intervention/PATS+"


list_of_file_path_pats_comparison_endline <- "H:/.shortcut-targets-by-id/16aNynESRbG2uyimkLnjz3Fr4L-fWtFa6/ALL DATA_ENDLINE_2022/Comparison/PATS+"


# # Did HAPEX at baseline and only 8 women at midline. Dropped HAPEX because some were damaged and it wasn't very accurate anyway
# 
# list_of_file_path_hapex_intervention_baseline <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA_BASELINE/intervention_baseline/HAPEX"
# 
# list_of_file_path_hapex_comparison_baseline <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA_BASLEINE/Intervention/HAPEX"
# 
# list_of_file_path_hapex_intervention_midline <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA_ENDLINE/intervention_midline/HAPEX"
# 
# list_of_file_path_hapex_comparison_midline <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA_ENDLINE/Intervention follow-up/HAPEX"
# 


### Output Files RDS 
# all_pm_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA/PATS+_Data_20210114.rds"
all_pm_data_path_rds_project <- here::here("2_data_raw/PATS+_Data_20220703.rds")

# # Did HAPEX at baseline and only 8 women at midline. Dropped HAPEX because some were damaged and it wasn't very accurate anyway
# all_hapex_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.rds"
# 
# all_hapex_data_path_csv <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.csv"
# file_HAPEX_data_base <- here::here("2_data_raw", "HAPEX_Data_20220703.rds")

################################################################################



## File of Pats+ Data

list_of_files_pats_intervention_baseline <-   
	list.files(
		path = list_of_file_path_pats_intervention_baseline,
		recursive = TRUE,
		pattern = "*.csv",
		# pattern = "*.xlsx"#,
		full.names = TRUE
	)

length(list_of_files_pats_intervention_baseline) #134

list_of_files_pats_comparison_baseline <-   
	list.files(
		path = list_of_file_path_pats_comparison_baseline,
		recursive = TRUE, 
		pattern = "*.csv", 
		full.names = TRUE 
	)

length(list_of_files_pats_comparison_baseline) #95

list_of_files_pats_intervention_midline <-   
	list.files(
		path = list_of_file_path_pats_intervention_midline,
		recursive = TRUE, 
		pattern = "*.csv", 
		full.names = TRUE 
	)

length(list_of_files_pats_intervention_midline) #60

list_of_files_pats_comparison_midline <-   
	list.files(
		path = list_of_file_path_pats_comparison_midline,
		recursive = TRUE, 
		pattern = "*.csv", 
		full.names = TRUE 
	)

length(list_of_files_pats_comparison_midline) #48

list_of_files_pats_intervention_endline <-   
	list.files(
		path = list_of_file_path_pats_intervention_endline,
		recursive = TRUE, 
		pattern = "*.csv", 
		full.names = TRUE 
	)

length(list_of_files_pats_intervention_endline) #

list_of_files_pats_comparison_endline <-   
	list.files(
		path = list_of_file_path_pats_comparison_endline,
		recursive = TRUE, 
		pattern = "*.csv", 
		full.names = TRUE 
	)

length(list_of_files_pats_comparison_endline) #

list_of_files_pats <- 
	c(
		list_of_files_pats_intervention_baseline, list_of_files_pats_comparison_baseline, 
		list_of_files_pats_intervention_midline, list_of_files_pats_comparison_midline,
		list_of_files_pats_intervention_endline, list_of_files_pats_comparison_endline
	)

all_pm_data <- 
	list_of_files_pats %>%
	set_names(sub(".*/", "", list_of_files_pats)) %>% # added file_name as first column value
	map_dfr(
		~ read_csv(
			.x,
			col_types = 
				cols(
					"c", "d", "d", "d", "d", "d", "d", 
					"d", "d", "d", "d", "d", "d", "d", "d", "d"
				),
			skip = 31, 
			col_names = FALSE, 
		),
		.id = "file_name" # labeled first column with as "file_name"
	) %>% 
	# select(-X) %>% # I don't know why there is an empty column X at the end
	mutate(
		# study_arm = str_extract(str_extract(file_name, "^(?:[^_]*_){2}[0-9]{1}"), "[0-9]$")
		file_name = str_replace(file_name, "\\..*","")  # takes off the .csv
	) %>%
	`colnames<-`( # must be exactly `colnames<-`
		c(
			"file_name", "dateTime",	
			"V_power",	"degC_sys",	"degC_air",	"RH_air",	
			"degC_CO",	"CO_PPM",	"status",	"ref_sigDel",	
			"low20avg",	"high320avg",	"motion",	"CO_mV",	
			"ignore",	"iButton_Temp",	"PM_Estimate"#, # "study_arm"
		)
	) 
#  mutate(dateTime_lubridated = lubridate::ymd_hms(dateTime, tz = "Asia/Dhaka", locale = Sys.getlocale("LC_TIME"), truncated = 3)) %>%

# all_pm_data %>%
# 	saveRDS(all_pm_data_path_rds)

all_pm_data %>%
	saveRDS(all_pm_data_path_rds_project)




# Hapex Data 

# list_of_files_hapex_intervention_baseline <-   
# 	list.files(
# 		path = list_of_file_path_hapex_intervention_baseline,
# 		recursive = TRUE, 
# 		pattern = "*.csv", 
# 		full.names = TRUE 
# 	)
# 
# list_of_files_hapex_intervention <-   
# 	list.files(
# 		path = list_of_file_path_hapex_comparison_baseline,
# 		recursive = TRUE, 
# 		pattern = "*.csv", 
# 		full.names = TRUE 
# 	)
# 
# list_of_files_hapex <- c(list_of_files_hapex_intervention_baseline, list_of_files_hapex_comparison_baseline)
# 
# 
# all_hapex_data <- 
# 	list_of_files_hapex %>%
# 	set_names(sub(".*/", "", list_of_files_hapex)) %>% # added file_name as first column value
# 	map_dfr(
# 		~ read_csv(
# 			.x,
# 			col_types = 
# 				cols("c", "d", "d"),
# 			skip = 29, 
# 			col_names = FALSE, 
# 		),
# 		.id = "file_name" # labeled first column with as "file_name"
# 	) %>% 
# 	mutate(
# 		# 			study_arm = str_extract(str_extract(file_name, "^(?:[^_]*_){3}[0-9]{1}"), "[0-9]$")
# 		file_name = str_replace(file_name, "\\..*","") # takes off the .csv
# 	) %>%
# 	`colnames<-`(
# 		c(
# 			"file_name", "timestamp",	"mother_compliance",	"mother_pm"# , # "study_arm"
# 		)
# 	) 
# 
# all_hapex_data %>%
# 	saveRDS(all_hapex_data_path_rds)
# 
# all_hapex_data %>%
# 	saveRDS(all_hapex_data_path_rds)
# 
# write.csv(all_hapex_data, all_hapex_data_path_csv)
# 
# #   mutate(dateTime_lubridated = lubridate::ymd_hms(dateTime, tz = "Asia/Dhaka", locale = Sys.getlocale("LC_TIME"), truncated = 3)) %>%



