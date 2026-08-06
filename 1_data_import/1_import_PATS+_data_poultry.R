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
list_of_file_path_pats_poultry <- "J:/My Drive/Poultry biosecurity/Data Raw/PM2.5_data"


### Output Files RDS 
# all_pm_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/ALL DATA/PATS+_Data_20210114.rds"
all_pm_data_path_rds_poultry <- here::here("2_data_raw/PATS+_Data_poultry_240621.rds")

# # Did HAPEX at baseline and only 8 women at midline. Dropped HAPEX because some were damaged and it wasn't very accurate anyway
# all_hapex_data_path_rds <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.rds"
# 
# all_hapex_data_path_csv <- "C:/Users/admin/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/Sensors/Hapex_Data_20220703.csv"
# file_HAPEX_data_base <- here::here("2_data_raw", "HAPEX_Data_20220703.rds")

################################################################################



## File of Pats+ Data

list_of_files_pats_poultry <-   
	list.files(
		path = list_of_file_path_pats_poultry,
		recursive = TRUE,
		pattern = "*.csv",
		# pattern = "*.xlsx"#,
		full.names = TRUE
	)

length(list_of_files_pats_poultry) #54


all_pm_data <- 
  list_of_files_pats_poultry %>% # list_of_files_pats_poultry %>%
  set_names(sub(".*/", "", list_of_files_pats_poultry)) %>% # added file_name as first column value
	map_dfr(
		~ read_csv(
			.x,
			col_types = 
				cols(
					"c", "d", "d", "d", "d", "d", "d", 
					"d", "d", "d", "d", "d", "d", "d", "d", "d"
				),
			skip = 6, #31
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

all_pm_data

all_pm_data %>%
	saveRDS(all_pm_data_path_rds_poultry)

View(all_pm_data)
