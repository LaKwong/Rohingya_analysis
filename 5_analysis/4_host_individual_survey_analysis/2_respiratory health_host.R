################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Firewood collection, cost of wood in the market, reasons for using wood
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R")) #Pulls in all variable group names

## Chris
# source(here::here("1_config.R"))


# Parameters

file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")



#===============================================================================


# Load input files

survey_data <- read_rds(file_in_1)


################################################################################
## Time inside
################################################################################

# At baseline we asked about time inside the main survey
# At midline and endline, we asked about time inside as part of the individual hh member survey

################# Will need to combine this with the individual data
	

survey_data %>%
	group_by(study_arm_overall) %>%
	summarise_at(
		vars(
			hours_inside, #hours_outside, 
			target_child_hours_inside # target_child_hours_outside, 
		), 
		list(mean), 
		na.rm = TRUE
	) 

## Vector of variables to summarize
table3Vars <- c("hours_inside", "target_child_hours_inside")

# ## Vector of categorical variables that need transformation
# table3FactorVars <- c()

## Create a TableOne object
# tab3 <- CreateTableOne(data = survey_data, vars = table3Vars, factorVars = table3FactorVars, strata = "study_arm")
tab3 <- CreateTableOne(data = survey_data, vars = table3Vars, strata = "timepoint")


print(tab3)
# women in the comparison group spent more time inside, which could be viewed as a good thing by the community

## No need to visualize this





##############################################################################
### Respiratory health 
##############################################################################


# "target_child_arm_measurements_yn" and "target_child_breathing_yn" 
# don't need to be in the table but the show refusal rates equal across groups

## Vector of variables to summarize
table2_yn_Vars <- 
	c(
		respiratory_vars_yn,
		breathing_vars,
		nonrespiratory_vars_yn, 
		generalhealth_vars_yn, 
		# mental_health_vars_yn,
		MUAC_vars
	)

## Vector of categorical variables that need transformation
table2_yn_FactorVars <- 
	c(
		respiratory_vars_yn, 
		generalhealth_vars_yn,
		nonrespiratory_vars_yn
		# mental_health_vars_yn
	)

## Create a TableOne object
tab2_yn <- 
	CreateTableOne(
		data = survey_data %>% filter(timepoint == "baseline"), 
		vars = table2_yn_Vars, 
		factorVars = table2_yn_FactorVars, 
		strata = "fuel_30_receive_lpg"
	)




### 




# # Vars diff at baseline: 
# target_child_clinic_resp_yn, target_child_wheezing_yn, target_child_distrubed_speech_yn,
# respondent_disturbed_speech_yn , 
# target_child_eye_red_yn, target_child_eye_itch_yn, 
# respondent_eye_red_yn, respondent_eye_itch_yn, respondent_eye_sore_yn, 
#\

# Helpful blog on how to run multiple regressions at the same time: https://sebastiansauer.github.io/multiple-lm-purrr2/


resp_vars_impacted <-
	c(
		"target_child_clinic_resp_yn", 
		"target_child_wheezing_yn",
		"target_child_distrubed_speech_yn",
		"respondent_disturbed_speech_yn", 
		"target_child_eye_red_yn", "target_child_eye_itch_yn", 
		"respondent_eye_red_yn", "respondent_eye_itch_yn", "respondent_eye_sore_yn"
	)

# Apparently I can use stargazer but can't call lm_robust directly: https://declaredesign.org/r/estimatr/articles/regression-tables.html
library(stargazer)




fig_physical_health_child <- 
	survey_data %>%
	select(timepoint, study_arm_overall, fcn_id, all_of(respiratory_vars_yn), all_of(nonrespiratory_vars_yn), all_of(generalhealth_vars_yn)) %>%
	select(-contains("respondent_")) %>%
	gather(-timepoint, -study_arm_overall, -fcn_id, key = "category", value = "present") %>% # fuel_30_purchased_fuel
	filter(category %notin% c("target_child_disturbed_sleep_yn", "target_child_distrubed_speech_yn")) %>%
	
	group_by(timepoint, study_arm_overall, category) %>% 
	# fuel_30_purchased_fuel_endline
	# summarise(prevalence = mean(present, na.rm = TRUE)) %>% #  sd = sd(expenditure, na.rm = TRUE)
	mutate(
		child_resp_group = 
			case_when(
				category %in% c(
					"target_child_cough_yn", "target_child_resp_rate_yn", "target_child_weight_loss_yn", "target_child_wheezing_yn", 
					#"target_child_disturbed_sleep_yn", "target_child_distrubed_speech_yn",
					"target_child_eye_red_yn", "target_child_eye_itch_yn", "target_child_lethargy_yn", "target_child_fever_yn", "target_child_clinic_resp_yn"
				) ~ "child"
			),
		category = 
			ordered(
				category,
				levels = 
					c(					
						"target_child_cough_yn", "target_child_resp_rate_yn", "target_child_wheezing_yn", 
						#"target_child_disturbed_sleep_yn", "target_child_distrubed_speech_yn",
						"target_child_eye_red_yn", "target_child_eye_itch_yn", "target_child_lethargy_yn", "target_child_weight_loss_yn", "target_child_fever_yn", "target_child_clinic_resp_yn"), 
				labels = 
					c(
						"cough", "increased \nrespiratory rate \ntoday",  "current \nwheeze", 
						# "sleep disturbed \nby wheeze", "speech disturbed \nby wheeze", 
						"eyes red", "eyes itchy", "lethargic", "unexplained \nweight loss \nin 3 mo.", "fever", "went to clinic \nfor respiratory \ncomplaint"
					)
			)
	) %>% 
	# mutate(expenditure_LCI = expenditure_mean - sd, expenditure_UCI = expenditure_mean + sd) %>%
	ggplot(aes(x = timepoint, y = present, color = study_arm_overall, group = study_arm_overall, shape = timepoint)) + #  y = prevalence, 
	stat_summary(fun = "mean", geom = "line", size = 1) + # , position = position_dodge(width = 0.2)
	stat_summary(fun = "mean", geom = "point", size = 2) + 
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
	# ggpubr::stat_compare_means(
	# 	label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
	# 	# method = "wilcox.test",
	# 	paired = FALSE, # baseline and endline are paired but the change is not, and on-going and intervention are not
	# 	hide.ns = TRUE
	# ) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 2/3
	) +
	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
	scale_color_manual(
		name = "Study arm",
		breaks = c("intervention", "comparison"),
		# labels = c("Pre-intervention", "Intervention", "Outdoor"),
		labels = c("Intervention", "Comparison"),
		values = c("#138b87",  "#430154"),
		# values = c("#7570b3",  "#d95f02", "#1b9e77")
	) +
	theme_bw() +
	theme(
		# axis.text.x = element_text(angle = 0, hjust = 1)
		axis.text.x = element_blank(),
		axis.ticks.x  = element_blank(),
		# axis.line.x = element_blank(),
		legend.position = "none"
	) + 
	labs(
		# title = "Health status over time",
		x =  "", # "Health category",
		y = "Prevalence \namong children <2 years old",
		shape = "Timepoint",
		color = "Study arm"
	) + 
	facet_grid( ~ category) # fuel_30_purchased_fuel_endline

# fig_physical_health_child

survey_data %>% 
	group_by(timepoint, study_arm_overall) %>% 
	summarise(mean(respondent_resp_rate, na.rm =TRUE), 
						mean(respondent_weight_loss, na.rm = TRUE))

fig_physical_health_respondent <- 
	survey_data %>%
	select(timepoint, study_arm_overall, fcn_id, all_of(respiratory_vars_yn), all_of(nonrespiratory_vars_yn), all_of(generalhealth_vars_yn), respondent_weight_loss, respondent_resp_rate) %>%
	select(-contains("child_")) %>%
	gather(-timepoint, -study_arm_overall, -fcn_id, key = "category", value = "present") %>% # fuel_30_purchased_fuel
	# pull(category) %>% unique()
	filter(category %notin% c("respondent_disturbed_sleep_yn", "respondent_disturbed_speech_yn")) %>%
	group_by(timepoint, study_arm_overall, category) %>% # fuel_30_purchased_fuel_endline
	mutate(
		# 		 "respondent_cough_yn"            "respondent_resp_rate_yn"        "respondent_weight_loss_yn"      "respondent_wheezing_yn"        
		#  [5] "respondent_disturbed_sleep_yn"  "respondent_disturbed_speech_yn" "respondent_eye_red_yn"          "respondent_eye_itch_yn"        
		#  [9] "respondent_eye_sore_yn"         "respondent_headache_yn"         "respondent_backache_yn"        
		child_resp_group = 
			case_when(
				category %in% c(
					"respondent_cough_yn", "respondent_resp_rate", "respondent_weight_loss", "respondent_wheezing_yn", 
					#"respondent_disturbed_sleep_yn", "respondent_disturbed_speech_yn", 
					"respondent_eye_red_yn", "respondent_eye_itch_yn", "respondent_eye_sore_yn", "respondent_headache_yn", "respondent_backache_yn"
				) ~ "respondent"
			)
	) %>%
	mutate(
		category = 
			ordered(
				category,
				levels = 
					c(					
						"respondent_cough_yn", "respondent_resp_rate", "respondent_wheezing_yn", 
						#"respondent_disturbed_sleep_yn", "respondent_disturbed_speech_yn", 
						"respondent_eye_red_yn", "respondent_eye_itch_yn", "respondent_eye_sore_yn", "respondent_weight_loss", "respondent_headache_yn", "respondent_backache_yn"
					), 
				labels = 
					c(
						"cough", "increased \nrespiratory rate \ntoday",  "current \nwheeze", 
						# "sleep disturbed \nby wheeze", "speech disturbed \nby wheeze",
						"eyes red", "eyes itchy", "eyes sore", "unexplained \nweight loss \nin 3 mo.", "headache", "backache"
					)
			)
	) %>%
	filter(!is.na(category)) %>% 
	# mutate(expenditure_LCI = expenditure_mean - sd, expenditure_UCI = expenditure_mean + sd) %>%
	ggplot(aes(x = timepoint, y = present, color = study_arm_overall, group = study_arm_overall, shape = timepoint)) + # y = prevalence
	stat_summary(fun = "mean", geom = "line", size = 1) + # , position = position_dodge(width = 0.2)
	stat_summary(fun = "mean", geom = "point", size = 2) + 
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
	# ggpubr::stat_compare_means(
	# 	label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
	# 	# method = "wilcox.test",
	# 	paired = FALSE, # baseline and endline are paired but the change is not, and on-going and intervention are not
	# 	hide.ns = TRUE
	# ) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 2/3
	) +
	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
	scale_color_manual(
		name = "Study arm",
		breaks = c("intervention", "comparison"),
		# labels = c("Pre-intervention", "Intervention", "Outdoor"),
		labels = c("Intervention", "Comparison"),
		values = c("#138b87",  "#430154"),
		# values = c("#7570b3",  "#d95f02", "#1b9e77")
	) +
	theme_bw() +
	theme(
		# axis.text.x = element_text(angle = 0, hjust = 1)
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		# axis.line.x = element_blank(),
		legend.position = "none"
	) + 
	labs(
		# title = "Health status over time",
		x =  "", # "Health category",
		y = "Prevalence \namong female caregivers",
		shape = "Timepoint",
		color = "Study arm"
	) + 
	facet_grid( ~ category) # fuel_30_purchased_fuel_endline # , labeller = label_wrap_gen(width = 25, multi_line = TRUE)



fig_physical_health <- gridExtra::arrangeGrob(fig_physical_health_child, fig_physical_health_respondent, ncol = 1)


as_ggplot(fig_physical_health)








