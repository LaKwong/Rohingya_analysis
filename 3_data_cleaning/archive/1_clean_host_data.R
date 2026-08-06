################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Clean Rohingya hh survey data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))



# ============================================================================
# Parameters
file_in_1 <- here::here("2_data_raw/RohingyaFuel_survey_data_long_host.rds") 

file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_clean_host.rds")

# ============================================================================

# ============================================================================
####### Host hh survey data #######
# ============================================================================


# study_arm == 4 is host pre-intervention
# study_arm == 5 is host post-intervention

data_long_host <- 
	read_rds(file_in_1) # %>%
	# mutate(
	# 	study_arm_overall = "host",
	# 	timepoint = if_else(
	# 			study_arm == 4, 
	# 			"baseline", 
	# 			"midline")
	# 	)


##################


#################

data_long_host %>% 
	count(SubmissionDate, starttime) %>% 
	view()


data_long_host %>% 
	count(timepoint, study_arm, first_receive_lpg)


survey_data_long_host <-
	data_long_host %>%
	
	# correct "fcn_id" of host hh recorded at endline to match their serial_id from baseline
	# based on "RohingyaFuel_survey_data_long_host_identifiers_Saeed_20210223.csv"
	
	
	######################### there are many hh_id_short that need to be fixed
	
	mutate(
		# change the fcn_id so that is aligns with the serial_id (recorded at baseline)
		fcn_id = 
			case_when(
				fcn_id == "004059" & name_respondent == "Nor bahar" ~ "004053", # serial_id == "T04053" & 
				fcn_id == "003696" & name_respondent == "Sonjida begum" ~ "003596", # serial_id == "T03596" & 
				fcn_id == "003831" ~ "003881", # serial_id == "T03881" &	
				fcn_id == "000000" ~ "003686", # serial_id == "T03686" & 
				fcn_id == "003463" ~ "003436", # serial_id == "T03436" & 
				fcn_id == "003791" ~ "003691", # serial_id == "T03691" & 
				
				TRUE ~ fcn_id
			),
		# change the serial_id so that it aligns with the fcn_id (recorded at endline)
		serial_id = 
			case_when(
				serial_id == "T04531" ~ "T04485", 
				serial_id == "T04485" ~ "T04531", 
				TRUE ~ serial_id
			)
	) %>%
	# more than 2 surveys: "003696"
	# only 1 survey: # "000848" "003176" "003233" "003250" "003450" "003453" "003509" "003610" "003621" "003625" "003878"  "004095"
	
	
	# Create a unique_id
	# Unique ID in the pre-intervention survey was saved as "serial_id" e.g. T03540
	# Unique ID in the post-interveniton survey was saved as "fcn_id" e.g. 3540
	mutate(
		serial_id_original = serial_id,
		fcn_id_baseline = ifelse(!is.na(serial_id), paste0("00", str_extract(serial_id, "[0-9]{4}$")), NA),
		fcn_id_endline = fcn_id,
		unique_id = coalesce(fcn_id_baseline, fcn_id_endline)
	) %>%
	
	# # Check that the formation of the unique ID worked
	# select(study_arm, serial_id_original, serial_id, fcn_id_baseline, fcn_id, fcn_id_endline, unique_id) %>% 
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	# mutate(master_row = X) %>%
	mutate(
		# enumerator = as.factor(enumerator),
		enumerator_name =
			case_when(
				enumerator == 1 ~ "Tunajjina Alam",
				enumerator == 2 ~ "Rayhanul Jannat",
				enumerator == 3 ~ "Shamima Akter",
				enumerator == 4 ~	"Morsida Akter",
				enumerator == 5 ~	"Way May Marma",
				enumerator == 6 ~ "Morselina Akter",
				enumerator == 7 ~ "Farhana Suma",
				enumerator == 8 ~	"Nishat Farjana",
				enumerator == 9 ~	"Arefa Khanam",
				enumerator == 10 ~	"Daliya Akter",
				enumerator == 11 ~	"Md. Jamilur Rahman",
				enumerator == 12 ~	"Md. Razu Ahmed",
				enumerator == 13 ~ "Mohammad Alamgir",
				enumerator == 14 ~	"Nazrin Akter",
				enumerator == 15 ~ "dummy",
				enumerator == 16 ~ "Fatema Akter",
				enumerator == 17 ~ "Tanij Akter"
			)
		
		
		
		# study_arm == 4 is host pre-intervention
		# study_arm == 5 is host post-intervention
		
		# study_arm =
		# 	factor(
		# 		study_arm,
		# 		levels = c(4, 5),
		# 		labels = c("host pre-intervention", "host post-intervention")
		# 	),
	) %>%
	
	# select(-enumerator) %>%
	# mutate_at(
	# 	vars(
	# 		enumerator_name,
	# 		lpg_cylinder_repair, lpg_stove_repair,
	# 		fuel_cant_afford_action, food_cant_afford_action
	# 	),
	# 	list(as.factor)
	# ) %>%
	# select(-X1) %>%
select(SubmissionDate, starttime, endtime, deviceid, start_date, enumerator_name, fcn_id, timepoint, study_arm, everything())

survey_data_long_host_endline <- 
	data_long_host_endline %>% 
	mutate(
		ward_id = as.double(ward_id), 
		lentils_source = as.factor(lentils_source),
		goat_sheep_source = as.factor(goat_sheep_source),
		fuel_cant_afford_action = as.double(fuel_cant_afford_action),
		fuel_type_before_lpg = as.double(fuel_type_before_lpg),
		nursery_own = as.double(nursery_own), 
		serial_id = as.character(serial_id)
	)

survey_data_long_host_combined <- 
	survey_data_long_host %>% 
	bind_rows(survey_data_long_host_endline)



################# Number of responses ################
survey_data_long_host_combined %>%
	count(study_arm_overall, timepoint)

survey_data_long_host_combined %>% count(timepoint, first_receive_lpg)

################# Save files ###########################
write_rds(survey_data_long_host_combined, file_out_1)
