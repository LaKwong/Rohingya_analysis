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
file_in_1 <- here::here("2_data_raw/RohingyaFuel_survey_data_long.rds") # Use the paired dataset instead of this one
file_in_2 <- here::here("2_data_raw/RohingyaFuel_survey_data_long_host.rds") # Use the paired dataset instead of this one


file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_clean.rds") # Use the paired dataset instead of this one
file_out_2 <- here::here("4_data/RohingyaFuel_survey_data_clean_host.rds") # Use the paired dataset instead of this one

# ============================================================================

# Clean data using responses in "C:\Users\admin\Box Sync\Coding in r (lakwong@stanford.edu)\Rohingya_analysis\Data_review_revisions\Rohingya fuel data review and revisions_endline"

####### Rohingya hh survey data ##############
# baseline
# 1 = pre-intervention
# 2 = intervention
# 
# endline
# 3 = post-intervention
# 6 = intervention follow-up


# pre-intervention / post-intervention camp_id %in% ("8w", "9", "10)
# intervention / intervention follow-up camp_id %in% ("8E", "18", "3", "4", "5") # 3,4,5 are UNCHR; others are IOM

data_long <- 
	read_rds(file_in_1) %>%
	mutate(study_arm = as.character(study_arm)) %>%
	# These hh were had study arm of "post-intervention" and "intervention follow-up" - the post-intervention is probably a mistake because 
	mutate(
		study_arm =
			factor(
				study_arm,
				levels = c(1, 3, 2, 6),
				labels = c("pre-intervention", "post-intervention", "intervention", "intervention follow-up")
			),
		study_arm = as.character(study_arm),
		study_arm =
			case_when(
				study_arm == "intervention follow-up" & fcn_id %in% c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767") ~ "post-intervention",
				TRUE ~ study_arm
			)
	)

data_long %>%
	filter(is.na(study_arm)) %>%
	select(fcn_id, study_arm, camp_id)

# data_long %>%
# 	filter(fcn_id %in% c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767")) %>%
# 	select(camp_id, study_arm, SubmissionDate)


# There should be no hh without fcn_ids
missing_fcn_id <- 
	data_long %>%
	select(camp_id, block_id, subblock_id, fcn_id) %>%
	filter(is.na(fcn_id)| fcn_id == "x") 

write_csv(missing_fcn_id, here::here("4_data/missing_fcn_id.csv"))


###################

# fix the baseline data
data_long_baseline <-
	data_long %>%
	filter(study_arm %in% c("pre-intervention", "intervention")) %>% 
	mutate(
		# HH listing errors
		# 
		# 10	g	G12 -many do not have a Rand_nu
		# 8W	b	A14 - several missing Rand_nu
		# 8W	b	A16 - several missing Rand_nu
		# 8W	d	I21	x	NA (no fcn_id or Rand_nu)
		# 8W	d	I21	NA	223 (no fcn_id)
		# 
		# 3	e	DD	187195	1354 - subblock is DD rather than DD13 or DD 19
		
		# 8W	d	I21	x	NA (no fcn_id or Rand_nu)
		fcn_id = 
			ifelse(
				fcn_id == "x" &
					study_arm == "pre-intervention" & 
					camp_id == "8w" & 
					block_id == "D" & 
					subblock_id == "I21" & 
					name_respondent == "Juhura khatun" & 
					target_child_name == "Shahida begum", 
				"100976", 
				fcn_id
			),
		
		# 8W	d	I21	NA	223 (no fcn_id)
		fcn_id = 
			ifelse(
				is.na(fcn_id) &
					study_arm == "pre-intervention" & 
					camp_id == "8w" & 
					block_id == "D" & 
					subblock_id == "I21" & 
					name_respondent == "Samina" & 
					target_child_name == "Mahbub rahman", # DOB 15-Apr-19
				"999999", 
				fcn_id
			)
	) %>%
	# remove hh that we accidentally surveryed twice
	filter(!(study_arm == "pre-intervention" & fcn_id == "117722" & enumerator == 8)) %>%
	filter(!(study_arm == "pre-intervention" & fcn_id == "197654" & enumerator == 1)) %>%
	filter(!(study_arm == "pre-intervention" & fcn_id == "100976" & enumerator == 3)) %>% # So maybe adding this in line 90 was incorrect....
	filter(!(study_arm == "intervention" & fcn_id == "186890" & enumerator == 1)) %>%
	mutate(
		hh_id_original = hh_id,
		hh_id = paste(paste0(camp_id, block_id, subblock_id, fcn_id), Rand_nu, sep = "_" ),
		fcn_id =
			case_when(
				study_arm == "pre-intervention" & camp_id == "8w" & block_id == "B" & subblock_id == "A13" & name_respondent == "Noor Begum" ~ "122063", # fcn_id %in% c("122055") & 
				study_arm == "intervention" & camp_id == "8E" & block_id == "C" & subblock_id == "B33" & name_respondent == "Yeasmin" ~ "114444", # this hh previously had fcn_id =="111198" &
				TRUE ~ fcn_id
			)
	)


data_long_endline <-
	# fix the endline data
	data_long %>%
	filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>% 
	mutate(
		fcn_id =
			case_when(
				study_arm == "post-intervention" & fcn_id == "101841" ~ "101840",
				study_arm == "post-intervention" & fcn_id == "101016" ~ "106615",
				study_arm == "post-intervention" & fcn_id == "109663" ~ "109697",
				# study_arm == "post-intervention" & fcn_id == "290439" ~ "112064", # There is already a post-intervention survey with this fcn_id
				study_arm == "post-intervention" & fcn_id == "122066" ~ "112066",
				study_arm == "post-intervention" & fcn_id == "118211" ~ "112811",
				study_arm == "post-intervention" & fcn_id == "123825" ~ "113825",
				study_arm == "post-intervention" & fcn_id == "115717" ~ "117517",
				# study_arm == "post-intervention" & fcn_id == "122063" ~ "122055", # There is already a post-intervention survey with this fcn_id
				study_arm == "post-intervention" & fcn_id == "133506" ~ "123506",
				study_arm == "post-intervention" & fcn_id == "123970" ~ "123969",
				study_arm == "post-intervention" & fcn_id == "193748" ~ "193738",
				study_arm == "post-intervention" & fcn_id == "106850" ~ "206850",
				study_arm == "post-intervention" & fcn_id == "101096" ~ "225997",
				study_arm == "post-intervention" & fcn_id == "291236" ~ "290437",
				# Couldn't find the what was wrong with the post-intervention fcn_ids that don't have a pre-intervention match:  101144, 109577, 600901, 122503, 123825, 291547
				
				study_arm == "intervention follow-up" & fcn_id == "186294" ~ "285592",
				study_arm == "intervention follow-up" & fcn_id == "199748" ~ "299748",
				study_arm == "intervention follow-up" & fcn_id == "245292" ~ "145292",
				study_arm == "intervention follow-up" & fcn_id == "200677" ~ "200667",
				study_arm == "intervention follow-up" & fcn_id == "194490" ~ "194499",
				study_arm == "intervention follow-up" & fcn_id == "197472" ~ "197422",
				study_arm == "intervention follow-up" & fcn_id == "383686" ~ "283686",
				study_arm == "intervention follow-up" & fcn_id == "297028" ~ "207028",
				study_arm == "intervention follow-up" & fcn_id == "168661" ~ "451059",
				study_arm == "intervention follow-up" & fcn_id == "172965" ~ "172964",
				study_arm == "intervention follow-up" & fcn_id == "183398" ~ "283398",
				study_arm == "intervention follow-up" & fcn_id == "180887" ~ "180837",
				study_arm == "intervention follow-up" & fcn_id == "184810" ~ "184809",
				study_arm == "intervention follow-up" & fcn_id == "185054" ~ "185055",
				study_arm == "intervention follow-up" & fcn_id == "193911" ~ "185415",
				study_arm == "intervention follow-up" & fcn_id == "185979" ~ "185980",
				study_arm == "intervention follow-up" & fcn_id == "157669" ~ "177669",
				study_arm == "intervention follow-up" & fcn_id == "177668" ~ "188577",
				study_arm == "intervention follow-up" & fcn_id == "177772" ~ "157574",
				study_arm == "intervention follow-up" & fcn_id == "177471" ~ "173073",
				study_arm == "intervention follow-up" & fcn_id == "287797" ~ "278797",
				study_arm == "intervention follow-up" & fcn_id == "296727" ~ "296729",
				study_arm == "intervention follow-up" & fcn_id == "650712" ~ "295896",
				study_arm == "intervention follow-up" & fcn_id == "650705" ~ "295894",
				study_arm == "intervention follow-up" & fcn_id == "650711" ~ "295890",
				study_arm == "intervention follow-up" & fcn_id == "451786" ~ "166147",
				study_arm == "intervention follow-up" & fcn_id == "164741" ~ "164742",
				study_arm == "intervention follow-up" & fcn_id == "120733" ~ "120773",
				# study_arm == "intervention follow-up" & fcn_id == "114444" ~ "111198", # There is already an intervention follow-up survey with this fcn_id
				TRUE ~ fcn_id
				# Couldn't find out what was wrong with these intervention follow-up hh that don't have an intervention match: 18295, 450097, 114546
			),
		bread_adults_week =
			case_when(
				fcn_id %in% c("114537", "120722") ~ 2,
				fcn_id %in% c("286043", "125336") ~ 3,
				fcn_id %in% c("111198") ~ 4,
				TRUE ~ bread_adults_week
			),
		clothing =
			case_when(
				fcn_id %in% c("193342") ~ 15000,
				fcn_id %in% c("120564") ~ 20000,
				fcn_id %in% c("114338", "193130") ~ 30000,
				TRUE ~ clothing
			),
		camp_id =
			case_when(
				fcn_id %in% c("165336") ~ as.character("5"), # based on subblock_id == "G56" and enumerator name and date
				fcn_id %in%
					c(
						"100959", "100971", "100972", "100976", "100999",
						"101034", "101038", "101109", "101172",
						"101239", "101251", "101574", "101618",
						"101667", "101705", "101723", "101737",
						"101762", "101777",
						"106976",
						"113898", "117152", "117719", "117729", "119902",
						"122567", "123342", "123677", "123970", "123976",
						"125795",
						"274944",
						"290417", "290439", "290495",
						"291236",
						"291525",
						"296044",
						"300629"
					) ~ as.character("8W"),
				fcn_id %in% c("114476") ~ as.character("8E"),
				fcn_id %in% c("115663") ~ as.character("9"), # based on block_id and subblock_id
				fcn_id %in% c("115815", "110636", "109334", "193582") ~ as.character("10"), #115815 was "CAMP10", 110636 was "G", 109334 and 193582 were 1O instead of 10
				fcn_id %in% c("196929", "245292") ~ as.character("18"), # 196929 was camp 28, 245292 was camp L18
				TRUE ~ as.character(camp_id)
			),
		block_id = 
			case_when(
				fcn_id %in% c("122063") ~ as.character("b"), # was ",b"
				# for fcn_id == 296782 based on submission date and enumerator name, block should be F and subblock_id = UU13
				fcn_id %in% c("296782") ~ as.character("f"), # was "4" 
				# for fcn_id == 115643, 115646, 115767  based on submission date and subblock_id == G36 block should be g
				fcn_id %in% c("115643", "115646", "115767") ~ as.character("g"), # was "4" 
				TRUE ~ as.character(block_id)
			),
		subblock_id = 
			case_when(
				# for fcn_id == 296782 based on submission date and enumerator name, block should be F and subblock_id = UU13
				fcn_id %in% c("296782") ~ as.character("UU13"), # was "F" (which as actually the block)
				fcn_id %in% c("286224") ~ as.character("G30"), # was G3O
				fcn_id %in% c("187195") ~ as.character("DD22"), # was DD, changed to DD22 based on enumerator name, date, camp, block,
				fcn_id %in% c("114741") ~ as.character("B33"), # was "B33.". Checked that there are many other hh in same camp, block, and subblock B33
				fcn_id %in% c("289608", "123657") ~ as.character("G29"), # was "G,29". Checked that there are many other hh in same camp, block, and subblock G29
				TRUE ~ as.character(subblock_id)
			),
		
		debt =
			case_when(
				fcn_id %in% c("116548", "289648") ~ 0,
				fcn_id %in% c("122093", "152796") ~ 3000,
				fcn_id %in% c("152797") ~ 4000,
				fcn_id %in% c("118858") ~ 8000,
				fcn_id %in% c("108549") ~ 13700,
				fcn_id %in% c("123665") ~ 300000,
				TRUE ~ debt
			),
		
		drudgery_most_diff =
			case_when(
				fcn_id %in% c("115756") ~ 4,
				TRUE ~ drudgery_most_diff
			),
		drudgery_second_most_diff =
			case_when(
				fcn_id %in% c("116103") ~ 4,
				TRUE ~ drudgery_second_most_diff
			),
		drudgery_easiest =
			case_when(
				fcn_id %in% c("116267", "122489", "102745") ~ 3,
				TRUE ~ drudgery_easiest
			),
		fish_adults_week =
			case_when(
				fcn_id %in% c("106841") ~ 4,
				fcn_id %in% c("102330") ~ 5,
				TRUE ~ fish_adults_week
			),
		floor_material =
			case_when(
				fcn_id %in%
					c(
						"296044"
					) ~ 5,
				TRUE ~ floor_material
			),
		flooring_below_stove =
			case_when(
				fcn_id %in% c("100959", "101172", "122489", "124325") ~ 3,
				fcn_id %in% c("111593", "123012", "122063", "102722", "118250", "102745") ~ 6,
				fcn_id %in% c("291525", "106976", "123864") ~ 8,
				TRUE ~ flooring_below_stove
			),
		food_cant_afford_2wk =
			case_when(
				fcn_id %in% c("115741") ~ 0,
				TRUE ~ food_cant_afford_2wk
			),
		
		# If I don't use as.numeric()
		# Error: Problem with `mutate()` input `fuel_cant_afford_action`.
		# x must be a double vector, not a character vector.
		# i Input `fuel_cant_afford_action` is `case_when(...)`.
		
		# When I use as.numeric() Error: Can't combine `..1$fuel_cant_afford_action` <character> and `..2$fuel_cant_afford_action` <double>.
		
		fuel_cant_afford_action =
			case_when(
				fcn_id %in% c("116267", "124381") ~ as.character("8"), # as.numeric("8"),
				fcn_id %in% c("115741") ~ NA_character_, # NA_real_,
				TRUE ~ as.character(fuel_cant_afford_action) # as.numeric(fuel_cant_afford_action)
			),
		forest_wood_fee =
			case_when(
				fcn_id %in% c("109334") ~	100,
				TRUE ~ forest_wood_fee
			),
		fuel_30_receive_lpg =
			case_when(
				fcn_id %in% c("106582") ~	1,
				TRUE ~ fuel_30_receive_lpg
			),
		happy =
			case_when(
				fcn_id %in% c("122929") ~ 1,
				fcn_id %in% c("121962", "119364") ~ 2,
				fcn_id %in%	c( "101723", "111944",  "112726", "115771", "117505","117514", "122241", "123005") ~	4,
				TRUE ~ happy
			),
		income_cash_ngo =
			case_when(
				fcn_id %in% c("179622") ~	1500,
				fcn_id %in% c("123568") ~ 9000, # was -9000, assume that the negative was an accident; 9000 is a very reasonable value
				TRUE ~ income_cash_ngo
			),
		income_humanitarian_asst =
			case_when(
				fcn_id %in% c("111287") ~	400,
				fcn_id %in% c("124768") ~	900,
				fcn_id %in% c("153470") ~	1000,
				TRUE ~ income_humanitarian_asst
			),
		income_own_business =
			case_when(
				fcn_id %in% c("105366") ~	30000,
				TRUE ~ income_own_business
			),
		income_wage_labor =
			case_when(
				fcn_id %in% c("109673") ~	6000,
				fcn_id %in% c("113921") ~	6500,
				fcn_id %in% c("109334") ~	7000,
				TRUE ~ income_wage_labor
			),
		lpg_cylinder_repair =
			case_when(
				fcn_id %in% c("122489") ~	as.numeric(1), # as.character("1")
				TRUE ~ as.numeric(lpg_cylinder_repair), # as.character(lpg_cylinder_repair), 
			),
		lpg_repair_costs =
			case_when(
				fcn_id %in% c("108877") ~	150,
				fcn_id %in% c("291525") ~	250,
				TRUE ~ lpg_repair_costs
			),
		lpg_stove_repair =
			case_when(
				fcn_id %in% c("187831") ~	as.character("1"), #as.numeric(1)
				TRUE ~ as.character(lpg_stove_repair) #as.numeric(lpg_stove_repair)
			),
		medical =
			case_when(
				fcn_id %in% c("120564") ~	1000,
				fcn_id %in% c("124088") ~	2000,
				fcn_id %in% c("290455") ~	3000,
				fcn_id %in% c("106260", "200851") ~	6000,
				fcn_id %in% c("101456") ~	7000,
				fcn_id %in% c("112638") ~	13000,
				fcn_id %in% c("108549") ~	15000,
				fcn_id %in% c("124321") ~	20000,
				fcn_id %in% c("281201") ~	30000,
				fcn_id %in% c("292310") ~	70000,
				fcn_id %in% c("179975") ~	7000,
				fcn_id %in%
					c(
						"106585", "106586", "111597", "112019",
						"115651", "115661", "115662", "115663",
						"115678", "115682", "115685", "116450",
						"119405", "119603", "123571"
					) ~	NA_real_,
				TRUE ~ medical
			),
		potatoes_adults_week =
			case_when(
				fcn_id %in% c("101274", "109334", "115756") ~	5,
				fcn_id %in% c("178204", "289648") ~	6,
				fcn_id %in% c("120982") ~	7,
				TRUE ~ potatoes_adults_week
			),
		shelter =
			case_when(
				fcn_id %in% c("111327") ~	20000,
				fcn_id %in% c("110693") ~	30000,
				TRUE ~ shelter
			),
		spent_total_month = 
			case_when(
				fcn_id %in% c("128915") ~ 3120,
				TRUE ~ spent_total_month
			),
		target_respondent_current =
			case_when(
				fcn_id %in% c("107012") ~	"Emtiaz Fatema",
				TRUE ~ target_respondent_current
			),
		time_cooking =
			case_when(
				fcn_id %in% c(
					"107012", "107019", "108549", "111470",
					"111944", "112138", "113921", "115548",
					"115756", "115771", "115792", "115817",
					"117514", "123568", "123569", "123570",
					"123657", "123670", "123845", "123976",
					"192633", "192730", "193582", "195381",
					"201161", "289608", "289648"
				) ~	3,
				TRUE ~ time_cooking
			),
		time_harvesting_wood =
			case_when(
				fcn_id %in%
					c(
						"123571", "123665", "201612"
					) ~	2,
				fcn_id %in%
					c(
						"100959", "100971", "100972", "100999",
						"101034", "101038", "101618", "101705",
						"102330", "106585", "106586", "108169",
						"108351", "108549", "109673", "109807",
						"111470", "111944", "112138", "115371",
						"115465", "115756", "115771", "115817",
						"116542", "116707", "116710", "116932",
						"116985", "116987", "117514", "117943",
						"119603", "121962", "122092", "122093",
						"122150", "122241", "122378", "123005",
						"123568", "123656", "123657", "123677",
						"123845", "123847", "123976", "193130",
						"193582", "195381", "289608", "289648",
						"600019"
					) ~	3,
				TRUE ~ time_harvesting_wood
			),
		traditional_use_yesterday =
			case_when(
				fcn_id %in% ("115818") ~ 1,
				fcn_id %in% ("122929") ~ 3,
				TRUE ~ traditional_use_yesterday
			)
	) 

survey_data_long <-
	data_long_baseline %>%
	bind_rows(data_long_endline) %>%
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
	) %>%
	
	# select(-enumerator) %>%  
	mutate(
		enumerator_name =
			case_when( #fct_case_when(
				fcn_id %in% c("105441", "111470", "600010") & study_arm %in% c("post-intervention", "intervention follow-up") ~	"Shamima Akter",
				TRUE ~ enumerator_name
			),
		timepoint = 
			factor(
				study_arm,
				levels = c("pre-intervention", "intervention", "post-intervention", "intervention follow-up"),
				labels = c("baseline", "baseline", "endline", "endline")
			),
		study_arm_overall = 
			factor(
				study_arm,
				levels = c("pre-intervention", "intervention", "post-intervention", "intervention follow-up"),
				labels = c("intervention", "comparison", "intervention", "comparison")
			)
	) %>%
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


####### Check that the corrections worked ########
survey_data_long %>%
	filter(fcn_id %in% c("122055", "122063", "111198", "114444", "112064", "101840", "101841")) %>%
	select(study_arm, fcn_id, name_respondent, target_child_name, camp_id, block_id, subblock_id) %>%
	arrange(fcn_id)


############################# Host hh survey data ##################33

# study_arm == 4 is host pre-intervention
# study_arm == 5 is host post-intervention

data_long_host <- 
	read_rds(file_in_2) %>%
	mutate(study_arm = as.character(study_arm))


# 
# survey_data_long_host <-
# 	data_long_host %>%
# 	mutate(
# 		study_arm =
# 			case_when(
# 				study_arm == "pre-intervention" & target_child_name == "Zahangir alam"  ~ "host pre-intervention", # based on survey data
# 				TRUE ~ study_arm
# 			)
# 	)  %>%
# 	select(
# 		-starts_with("generated_note"),
# 		-starts_with("reserved_name")
# 	) %>%
# 	# mutate(master_row = X) %>%
# 	mutate(
# 		# enumerator = as.factor(enumerator),
# 		enumerator_name =
# 			case_when(
# 				enumerator == 1 ~ "Tunajjina Alam",
# 				enumerator == 2 ~ "Rayhanul Jannat",
# 				enumerator == 3 ~ "Shamima Akter",
# 				enumerator == 4 ~	"Morsida Akter",
# 				enumerator == 5 ~	"Way May Marma",
# 				enumerator == 6 ~ "Morselina Akter",
# 				enumerator == 7 ~ "Farhana Suma",
# 				enumerator == 8 ~	"Nishat Farjana",
# 				enumerator == 9 ~	"Arefa Khanam",
# 				enumerator == 10 ~	"Daliya Akter",
# 				enumerator == 11 ~	"Md. Jamilur Rahman",
# 				enumerator == 12 ~	"Md. Razu Ahmed",
# 				enumerator == 13 ~ "Mohammad Alamgir",
# 				enumerator == 14 ~	"Nazrin Akter",
# 				enumerator == 15 ~ "dummy",
# 				enumerator == 16 ~ "Fatema Akter",
# 				enumerator == 17 ~ "Tanij Akter"
# 			),
# 		
# 		# study_arm == 4 is host pre-intervention
# 		# study_arm == 5 is host post-intervention
# 		
# 		# study_arm = 
# 		# 	factor(
# 		# 		study_arm,
# 		# 		levels = c(4, 5),
# 		# 		labels = c("host pre-intervention", "host post-intervention")
# 		# 	),
# 		timepoint = 
# 			factor(
# 				study_arm,
# 				levels = c("host pre-intervention", "host post-intervention"),
# 				labels = c("baseline", "endline")
# 			),
# 		study_arm_overall = 
# 			factor(
# 				study_arm,
# 				levels =  c("host pre-intervention", "host post-intervention"),
# 				labels = c("intervention", "intervention")
# 			)
# 	) %>%
# 	
# 	# select(-enumerator) %>%  
# 	# mutate_at(
# 	# 	vars(
# 	# 		enumerator_name,
# 	# 		lpg_cylinder_repair, lpg_stove_repair,
# 	# 		fuel_cant_afford_action, food_cant_afford_action
# 	# 	), 
# 	# 	list(as.factor)
# 	# ) %>%
# 	# select(-X1) %>%
# select(SubmissionDate, starttime, endtime, deviceid, start_date, enumerator_name, fcn_id, timepoint, study_arm, everything())





################# Number of responses ################

data_long %>%
	count(study_arm)

survey_data_long %>%
	count(study_arm)

# survey_data_long_host %>% 
# count(study_arm)

# Saeed reports that they completed surveys with 575 post-intervention households; so I am missing 575-568 = 17 surveys (some of these were lost because of the required note that prohibited uploading). Also missing 10 host surveys


################# Save files ###########################
write_rds(survey_data_long, file_out_1)
# write_rds(survey_data_long_host, file_out_2)
