################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Clean Rohingya hh survey data
# @Version: 3.6.2
# @Date: 220831
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

# ============================================================================
# Parameters
file_in_1 <- here::here("2_data_raw/RohingyaFuel_survey_data_long.rds")

file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_clean.rds")

# ============================================================================

# Clean data using responses in "C:\Users\admin\Box Sync\Coding in r (lakwong@stanford.edu)\Rohingya_analysis\Data_review_revisions\Rohingya fuel data review and revisions_endline"

# ============================================================================
####### Rohingya hh survey data ##############
# ============================================================================


# baseline
# 1 = pre-intervention (intervention)
# 2 = intervention (comparison)
# 
# midline
# 3 = post-intervention (intervention)
# 6 = intervention follow-up (comparison)
#
# endline
# 7 = post-intervention rd3 (intervention)
# 8 = intervention follow-up rd3 (comparison)

# host
# 5 = host pre-intervention
# 9 = host post-intervention

# study_arm_overall == intervention: camp_id %in% ("8w", "9", "10)
# study_arm_overall == comparison: camp_id %in% ("8E", "18", "3", "4", "5") # 3,4,5 are UNCHR; others are IOM



data_long <- 
	read_rds(file_in_1) %>%
	mutate(study_arm = as.character(study_arm)) %>%
	# These hh were had study arm of "post-intervention" and "intervention follow-up" - the post-intervention is probably a mistake because 
	mutate(
		study_arm_number = study_arm,
		study_arm =
			factor(
				study_arm,
				levels = c(1, 3, 2, 6, 7, 8),
				labels = c("pre-intervention", "post-intervention", "intervention", "intervention follow-up", "post-intervention rd3", "comparison rd3")
				#These get changed further down and timepoints get added
			),
		study_arm = as.character(study_arm),
		study_arm =
			case_when(
				study_arm == "intervention follow-up" & fcn_id %in% c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767") ~ "post-intervention",
				TRUE ~ study_arm
			)
	)


#list of samples missing data 
data_long %>%
	filter(is.na(study_arm)) %>%
	select(fcn_id, study_arm, camp_id)



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
		
		# Updated after a discussion with the team to clarify the hh_id; values match
		# 2_data_raw/RohingyaFuelMaster_hh_data - Copy.xlsx.
		# 8W	d	I21	x	NA (no fcn_id or Rand_nu) -> 8wDI21999999_NA
		fcn_id = 
			ifelse(
				fcn_id == "x" &
					study_arm == "pre-intervention" & 
					camp_id == "8w" & 
					block_id == "D" & 
					subblock_id == "I21" & 
					name_respondent == "Juhura khatun" & 
					target_child_name == "Shahida begum", 
				"999999",
				fcn_id
			),
		
		# 8W	d	I21	NA	223 (no fcn_id) -> 8wDI21999998_223
		fcn_id = 
			ifelse(
				is.na(fcn_id) &
					study_arm == "pre-intervention" & 
					camp_id == "8w" & 
					block_id == "D" & 
					subblock_id == "I21" & 
					name_respondent == "Samina" & 
					target_child_name == "Mahbub rahman", # DOB 15-Apr-19
				"999998",
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
			),
		target_child_name = 
			case_when(
				study_arm == "pre-intervention" & camp_id == "8w" & block_id == "B" & subblock_id == "A13" & name_respondent == "Rashida Begum" ~ "Omar", 
				study_arm == "pre-intervention" & camp_id == "8w" & block_id == "B" & subblock_id == "A13" & name_respondent == "Noor Begum" ~ "Noor halima", 
				TRUE ~ target_child_name
			)
	) %>% 
	mutate(
		# sometimes collect_wood_forest_start and collect_wood_forest_stop were swapped
		# fcn_id with negative duration of collecting firewood --> probably the start and stop dates are incorrect
		# correct this during data cleaning
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
		
		collect_wood_forest_start =  # first_receive_lpg_ymd
			case_when(
				fcn_id %in% c("600061") ~ "1-Apr-17", 
				fcn_id %in% c("124870") ~ "1-Jul-17", 
				fcn_id %in% c("120773") ~ "1-Jul-18", 
				fcn_id %in% c("124869") ~ "1-Mar-17", 
				fcn_id %in% c("107198") ~ "1-Sep-18",
				TRUE ~ as.character(collect_wood_forest_start)
			),
		
		collect_wood_forest_stop =  # first_receive_lpg_ymd
			case_when(
				fcn_id %in% c("600061") ~ "1-Aug-17", 
				fcn_id %in% c("124870") ~ "1-Aug-17", 
				fcn_id %in% c("120773") ~ "1-Aug-18", 
				fcn_id %in% c("124869") ~ "1-Aug-17", 
				fcn_id %in% c("107198") ~ "1-Sepp-19",
				TRUE ~ as.character(collect_wood_forest_stop)
			)
	)


data_long_midline <-
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
		
		# Need to change their study arm to 8E because 8W was an intervention camp and 8E was a comparison camp
		# study_arm_overall study_arm              timepoint camp_id block_id subblock_id fcn_id name_mahji name_respondent name_hh_head target_child_name
		# <ord>             <chr>                  <ord>     <chr>   <chr>    <chr>       <chr>  <chr>      <chr>           <chr>        <chr>            
		# 1 comparison        intervention follow-up midline   8W      c        B30         114556 NA         Toslima         NA           Shahida          
		# 2 comparison        intervention follow-up midline   8W      a        B13         124614 NA         Arafat Begum    NA           Nur Kamal 
		
		
		
		
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
				fcn_id %in% c("114476", "114556", "124614") ~ as.character("8E"),
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
				fcn_id %in% c("295872") ~ as.character("UU14"), # was "F14" (F was the block)
				fcn_id %in% c("286224") ~ as.character("G30"), # was G3O
				fcn_id %in% c("187195") ~ as.character("DD22"), # was DD, changed to DD22 based on enumerator name, date, camp, block,
				fcn_id %in% c("114741") ~ as.character("B33"), # was "B33.". Checked that there are many other hh in same camp, block, and subblock B33
				fcn_id %in% c("289608", "123657") ~ as.character("G29"), # was "G,29". Checked that there are many other hh in same camp, block, and subblock G29
				TRUE ~ as.character(subblock_id)
			),
		
		# sometimes collect_wood_forest_start and collect_wood_forest_stop were swapped
		# fcn_id with negative duration of collecting firewood --> probably the start and stop dates are incorrect
		# correct this during data cleaning
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
		
		collect_wood_forest_start =  # first_receive_lpg_ymd
			case_when(
				fcn_id %in% c("121839") ~ "Apr 1, 2017", 
				fcn_id %in% c("182703") ~ "Aug 1, 2017", 
				fcn_id %in% c("110349") ~ "Feb 1, 2017", 
				fcn_id %in% c("295889") ~ "Feb 1, 2017", 
				fcn_id %in% c("184266") ~ "Mar 1, 2017", 
				fcn_id %in% c("109816") ~ "May 1, 2017", 
				fcn_id %in% c("184565") ~ "Nov 1, 2017",
				TRUE ~ as.character(collect_wood_forest_start)
			),
		
		collect_wood_forest_stop =  # first_receive_lpg_ymd
			case_when(
				fcn_id %in% c("121839") ~ "Sep 1, 2017", 
				fcn_id %in% c("182703") ~ "Oct 1, 2017", 
				fcn_id %in% c("110349") ~ "Aug 1, 2017", 
				fcn_id %in% c("295889") ~ "Aug 1, 2017", 
				fcn_id %in% c("184266") ~ "Sep 1, 2017", 
				fcn_id %in% c("109816") ~ "Aug 1, 2017", 
				fcn_id %in% c("184565") ~ "Dec 1, 2017",
				TRUE ~ as.character(collect_wood_forest_stop)
			),
		
		
		
		# in geocene anlysis we see that hh286053, not yet receiving but 100% lpg use, 10HH55286053, when already geocene tracking at 2019-11-24 used lpg even though said started at 2019-12-01. 
		first_receive_lpg =  # first_receive_lpg_ymd
			case_when(
				fcn_id %in% c("286053") ~ "Nov 1, 2019", #"23-Nov-19", # we see that this hh was using lpg starting on 23 Nov 2019 (the first day of our measuremnets)
				TRUE ~ as.character(first_receive_lpg)
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
				fcn_id %in% c("122489") ~	"1", # as.character("1")
				TRUE ~ as.character(lpg_cylinder_repair), # as.character(lpg_cylinder_repair), 
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


#Fix the endline data 

data_long_endline <- 
	# fix the endline data
	data_long %>%
	filter(study_arm %in% c("post-intervention rd3", "comparison rd3")) %>% 
	mutate(
		study_arm =           #Two households had wrong arm listed at endline
			case_when(
				fcn_id == "115651" ~ "post-intervention rd3", 
				fcn_id == "115661" ~ "post-intervention rd3", 
				TRUE ~ study_arm
			),
		fcn_id =
			case_when(
				study_arm == "post-intervention rd3" & fcn_id == "101841" ~ "101840",
				study_arm == "post-intervention rd3" & fcn_id == "108306" ~ "109306",
				study_arm == "post-intervention rd3" & fcn_id == "108796" ~ "108799",
				study_arm == "post-intervention rd3" & fcn_id == "111272" ~ "112172",
				study_arm == "post-intervention rd3" & fcn_id == "115603" ~ "115643",
				study_arm == "post-intervention rd3" & fcn_id == "123825" ~ "113825",
				study_arm == "post-intervention rd3" & fcn_id == "124324" ~ "124321",
				study_arm == "comparison rd3" & fcn_id == "157669" ~ "177669",
				study_arm == "comparison rd3" & fcn_id == "171094" ~ "171098",
				study_arm == "comparison rd3" & fcn_id == "183398" ~ "283398",
				study_arm == "post-intervention rd3" & fcn_id == "191395" ~ "191359",
				study_arm == "post-intervention rd3" & fcn_id == "207984" ~ "107984",
				study_arm == "comparison rd3" & fcn_id == "297872" ~ "295872",
				study_arm == "comparison rd3" & fcn_id == "650711" ~ "295890",
				study_arm == "comparison rd3" & fcn_id == "207984" ~ "107984",
				study_arm == "comparison rd3" & fcn_id == "147235" ~ "174235",
				TRUE ~ fcn_id
			),
		camp_id =
			case_when(
				hh_id %in% ("DDH21291234") ~ "8W",
				fcn_id %in% ("115766") ~ "10",
				TRUE ~ camp_id
			), 
		
		block_id = 
			case_when(
				block_id %in% ("292302") ~ "E",
				TRUE ~ as.character(block_id)
			), 
		subblock_id = 
			case_when(
				str_detect(subblock_id, "(.+)") == TRUE~ str_extract(subblock_id, "^[^\\(]+"),
				fcn_id %in% ("292302") ~ "UU16",
				fcn_id %in% ("115663") ~ "g39",
				fcn_id %in% ("123976") ~ "I14",
				fcn_id %in% ("123976") ~ "I14",
				fcn_id %in% ("650711") ~ "UU14", ## this fcn_id is incorrect and should have been changed above, but modifying here just in case
				fcn_id %in% ("295890") ~ "UU14",
				fcn_id %in% ("125333") ~ "UU14",
				fcn_id %in% ("125336") ~ "B33",
				TRUE ~ subblock_id
			),
		
		blanket = 
			case_when(
				blanket %in% ("182703") ~ 8,
				TRUE ~ blanket
			),
		buy_wood_cost_bundle =
			case_when(
				fcn_id %in% ("302244") ~ 50,
				TRUE ~ buy_wood_cost_bundle
			), 
		buy_wood_cost_month_estimate =
			case_when(
				fcn_id %in% ("302244") ~ 100,
				TRUE ~ buy_wood_cost_month_estimate
			), 
		
		chicken_duck_pigeon =
			case_when(
				fcn_id %in% ("175537") ~ 2,
				TRUE ~ chicken_duck_pigeon
			), 
		child_books =
			case_when(
				fcn_id %in% ("283059") ~ 10,
				fcn_id %in% ("277017") ~ 4,
				fcn_id %in% ("207030") ~ 5,
				TRUE ~ child_books
			), 
		collect_wood_when_last =
			case_when(
				fcn_id %in% ("175715") ~ "",
				fcn_id %in% ("187828") ~ "",
				TRUE ~ as.character(collect_wood_when_last)
			), 
		cook_who_w =
			case_when(
				fcn_id %in% ("249671") ~ 1,
				TRUE ~ cook_who_w 
			),  
		debt = 
			case_when(
				fcn_id %in% ("112064") ~ 1500,
				TRUE ~ debt
			), 
		food_source = 
			case_when(
				fcn_id %in% (c("112064", "283008", "277068", "277066", "169327", 
											 "280794", "179622", "179621", "147235", "184565", "283077",
											 "175489", "179354","147887","180644"	, "168593","147235") )~ 1,
				TRUE ~ food_source
			), 
		healthcare_visits_6mo = 
			case_when(
				fcn_id %in% ("286010") ~ 8,
				fcn_id %in% ("184510") ~ 2,
				TRUE ~ healthcare_visits_6mo
			), 
		hh_size = 
			case_when(
				fcn_id %in% ("122132") ~ 14,
				TRUE ~ hh_size
			), 
		income_home_garden = 
			case_when(
				fcn_id %in% (c("111472", "111470", "111589", "112010")) ~ 0,
				TRUE ~ income_home_garden
			), 
		lpg_willingness_to_pay = 
			case_when(
				fcn_id %in% ("302244")~ 100,
				TRUE ~ lpg_willingness_to_pay
			), 
		meat_consumption = 
			case_when(
				fcn_id %in% ("278797")~ 0,
				TRUE ~ meat_consumption
			),  
		name_respondent= 
			case_when(
				fcn_id %in% ("115660")~ "Nure jannat",
				fcn_id %in% ("170934")~ "Tasmin",
				TRUE ~ name_respondent
			),  
		sleep_fall_asleep_min = 
			case_when(
				fcn_id %in% (c("292314", "174188", "171478"," 186825"))~ 10,
				TRUE ~ sleep_fall_asleep_min
			), 
		sleep_hours = 
			case_when(
				fcn_id %in% ("278797")~ 10,
				fcn_id %in% ("181279")~ 7,
				TRUE ~ sleep_hours
			), 
		solar_panel = 
			case_when(
				fcn_id %in% ("106582")~ 2,
				TRUE ~ solar_panel
			), 
		spent_food = 
			case_when(
				fcn_id %in% ("173639")~ 3000,
				fcn_id %in% ("173639")~ 3000,
				TRUE ~ spent_food
			), 
		
		veggies_adults_week = 
			case_when(
				fcn_id %in% ("152849")~ 3,
				TRUE ~ veggies_adults_week
			),
		veggies_source =
			case_when(
				fcn_id %in% ("123571")~ "2 3",
				TRUE ~ as.character(veggies_source)
			))

# Combine the corrected datasets
survey_data_long <-
	data_long_baseline %>%
	bind_rows(data_long_midline) %>%
	mutate(collect_wood_when_last = as.character(collect_wood_when_last)) %>% 
	bind_rows(data_long_endline) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	# mutate(master_row = X) %>%
	mutate(
		## The following are not working
		# SubmissionDate = parse_date_time(SubmissionDate, orders = "mdy IMS", tz = "Asia/Dhaka"),
		# starttime = parse_date_time(starttime, orders = "mdy IMS", tz = "Asia/Dhaka"),
		# endtime = parse_date_time(endtime, orders = "mdy IMS", tz = "Asia/Dhaka"),
		start_date = mdy(start_date) # Not working for most values, not sure why
		# date forms: 
		# 9/15/2019 12:43
		# Sep 20, 2020 11:14:24 AM
		
	) %>%
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
			case_when(
				study_arm == "pre-intervention" ~ "baseline", 
				study_arm == "intervention" ~ "baseline", 
				study_arm == "post-intervention" ~ "midline", 
				study_arm == "intervention follow-up" ~ "midline", 
				study_arm == "post-intervention rd3" ~ "endline", 
				study_arm == "comparison rd3" ~ "endline"
			),
		timepoint = 
			ordered(
				timepoint,
				levels = c("baseline", "midline", "endline")
			),
		study_arm_overall =
			case_when(
				study_arm == "pre-intervention" ~ "intervention", 
				study_arm == "intervention" ~ "comparison", 
				study_arm == "post-intervention" ~ "intervention", 
				study_arm == "intervention follow-up" ~ "comparison", 
				study_arm == "post-intervention rd3" ~ "intervention", 
				study_arm == "comparison rd3" ~ "comparison"
			),
		study_arm_overall = 
			ordered(
				study_arm_overall, 
				levels = c("comparison", "intervention")
			)
	) %>%
	# select(-X1) %>%
	select(SubmissionDate, starttime, endtime, deviceid, start_date, enumerator_name, fcn_id, timepoint, study_arm, everything())

######## adding in first enroll and first receive lpg to the endline results when it wasnt asked #########

donor <- 
	survey_data_long %>%
	filter(timepoint == "midline") %>% 
	select(fcn_id, first_receive_lpg, first_enrolled_lpg)


data_begin_mid <- survey_data_long %>% filter(timepoint == "baseline" | timepoint == "midline")
data <- survey_data_long %>% filter(timepoint == "endline")


r = merge(data, donor, by="fcn_id", suffixes=c(".data", ".donor"))
na.idx = which(is.na(data$first_receive_lpg))
data[na.idx,"first_receive_lpg"] = r[na.idx,"first_receive_lpg.donor"]


na.idx2 = which(is.na(data$first_enrolled_lpg))
data[na.idx2,"first_enrolled_lpg"] = r[na.idx2,"first_enrolled_lpg.donor"]

data %>% count(timepoint, first_receive_lpg)

survey_data_longer <- 
	data %>% 
	bind_rows(data_begin_mid)

survey_data_longer

# survey_data_longer %>% 
# 	count(first_receive_lpg, timepoint) %>% 
# 	view()


####### Check that the corrections worked ########
survey_data_long %>%
	filter(fcn_id %in% c("122055", "122063", "111198", "114444", "112064", "101840", "101841")) %>%
	select(study_arm, fcn_id, name_respondent, target_child_name, camp_id, block_id, subblock_id) %>%
	arrange(fcn_id)



# data_long %>%
# 	filter(fcn_id %in% c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767")) %>%
# 	select(camp_id, study_arm, SubmissionDate)


# 8wDI21x should be 8wDI21999999_NA and 8wDI21_223 should be 8wDI21999998_223 after a discussion with the team to clarify the hh_id; 
# 10F192633F33_4329 should be  192633 (survey) or 192619 (geocene) --> keep survey fcn_id and change fcn_id in geocene
# 4E179029Pp 15_398 should be 179086 (survey) or 179029 (geocene) --> keep survey fcn_id and change fcn_id in geocene
# 8wD291519 was not found in survey? but fcn_id should be 291519 --> keep survey fcn_id and change fcn_id in geocene

# 1 999999 8wDI21999999_NA  
# 1 999998 8wDI21999998_223
# 2 291519 8wDI18291519_NA    # missing NA
# 2 192633 10FF33192633_4329 
# 3 192633 10FF33192633 
# 4 179086 4EPp 15179086_398
# 5 179086 4EPP15179086 

# survey_data_long %>%
# 	# filter(
# 	# 	hh_id %in% c(
# 	# 		"8wDI18224646", "8wDI18101573", "8wDI21x", "8wG107339G10_3638", "10G192754G38_3622", "10F192633F33_4329", 
# 	# 		"4E179029Pp 15_398", "10F201621F40_4201", "10F201745F40_4208", "9G123577G29_6359", 
# 	# 		"8wDH21291216", "8wD291519", "10D192773D12_2837")
# 	# ) %>%
# 	filter(
# 		fcn_id %in% c(
# 			"999999", "999998", "192633", "192619", "179086", "179029", "291519"
# 		)
# 	) %>%
# 	select(fcn_id, hh_id)


################# Number of responses ################

data_long %>%
	count(study_arm)

survey_data_long %>%
	count(study_arm)

# survey_data_long_host %>% 
# count(study_arm)

# Saeed reports that they completed surveys with 575 post-intervention households; so I am missing 575-568 = 17 surveys (some of these were lost because of the required note that prohibited uploading). Also missing 10 host surveys
# This is missing the final data coming in from camp 18 on the survey

################# Save files ###########################
write_rds(survey_data_long, file_out_1)

