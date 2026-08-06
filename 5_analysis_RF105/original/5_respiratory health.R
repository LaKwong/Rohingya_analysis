################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong/ Christopher LeBoa 
# @Description: This document documents the respiratory and physical health outcomes documented by this study. It contains the following sections 
#1: Time Inside 
#2: 
# @Date Updated: 241019
################################################################################
rm(list = ls())

source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R")) #Pulls in all variable group names
source(here::here("5_analysis/Dif_in_dif_fxn.R")) 
#this is a function that conducts a difference in difference analysis and reports the interaction term of timepoint*study arm as the dif in dif result 


file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_triple.rds") #Overall survey dataset
file_in_2 <- here::here("4_data/pm_data_nearest_min.rds") #Particulate matter sar saved to the nearest minute 
file_in_3 <- here::here("4_data/PM_hh_averages_20220717.rds") #PM data that takes hh averages at each timepoint

# Load input files
survey_data <- read_rds(file_in_1)
pm_data_nearest_min <- read_rds(file_in_2) # PM_Estimate
hh_pm_average_location <- read_rds(file_in_3)


###################
# Add in an accute lower respiratory illness variable to the survey by combining the fever and cough in last 6 months with the questions 

#target_child_cough: In the past 6 months, has ${target_child_name} coughed continuously coughed for more than 2 weeks?
#target_child_fever: In the past 6 months, has ${target_child_name} had a fever for longer than 1 week or a fever >38°C with no explanation?


survey_data <- 
  survey_data %>% 
  mutate(
    target_child_alri = if_else(target_child_clinic_resp_yn == 1 | target_child_resp_rate_yn == 1, 1, 0), 
    target_child_asthma = if_else(target_child_wheezing_yn == 1, 1, 0), 
    target_child_severe_asthma = if_else((target_child_wheezing_yn == 1 & target_child_distrubed_speech_yn == 1) |(target_child_wheezing_yn == 1 & target_child_disturbed_sleep == 1), 1, 0)
    )



################################################################################
## Time inside
################################################################################

# At baseline we asked about time inside the main survey
# At midline and endline, we asked about time inside as part of the individual hh member survey

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
tab3 <- CreateTableOne(data = survey_data, vars = table3Vars, strata = "study_arm_overall")


kable(print(tab3))
# women in the comparison group spent more time inside, which could be viewed as a good thing by the community

##############################################################################
### Respiratory health 
##############################################################################


respiratory_data <-  #Creates a dataset of health variables 
	survey_data %>%
	select(
		fcn_id, study_arm, camp_id, block_id, subblock_id,
		all_of(respiratory_vars), all_of(generalhealth_vars)
	)

saveRDS(respiratory_data, here::here("4_data/respiratory_data.rds"))


## Vector of variables to summarize in respiratory health variable 
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
tab2_yn_bl <- 
	CreateTableOne(
		data = survey_data %>% filter(timepoint %in% c("baseline")), 
		vars = table2_yn_Vars, 
		factorVars = table2_yn_FactorVars, 
		strata = "study_arm_overall"
	)

## Create a TableOne object
tab2_yn_ml <- 
	CreateTableOne(
		data = survey_data %>% filter(timepoint %in% c("midline")), 
		vars = table2_yn_Vars, 
		factorVars = table2_yn_FactorVars, 
		strata = "study_arm_overall"
	)

## Create a TableOne object
tab2_yn_el <- 
	CreateTableOne(
		data = survey_data %>% filter(timepoint %in% c("endline")), 
		vars = table2_yn_Vars, 
		factorVars = table2_yn_FactorVars, 
		strata = "study_arm_overall"
	)

## Print out a table object at each timepoint to compare the two graoups 
tab2_yn_bl
tab2_yn_ml
tab2_yn_el

## Proportion difference Conf Interval listed in paper 

#clinic cisit 
prop.test(x = c(176, 255), n=c(437,494), correct = FALSE)

#wheezing 
prop.test(x = c(56, 80), n=c(437,494), correct = FALSE)

#Wheezing with distubed speech 
prop.test(x = c(14, 35), n=c(48,60), correct = FALSE)

#Eye redness
prop.test(x = c(66, 117), n=c(437,494), correct = FALSE)

#Eye itch
prop.test(x = c(41, 86), n=c(437,494), correct = FALSE)

#fever
prop.test(x = c(263, 322), n=c(437,494), correct = FALSE)

#resp redness
prop.test(x = c(38, 98), n=c(437,494), correct = FALSE)

#resp redness
prop.test(x = c(42, 93), n=c(437,494), correct = FALSE)

#resp redness
prop.test(x = c(41, 86), n=c(437,494), correct = FALSE)


#child wheez
prop.test(x = c(165, 251), n=c(437,494), correct = FALSE)

### Based on variables that were diffferent at baseline we selected variables for reporting possible health 
# # Vars diff at baseline: 
# target_child_clinic_resp_yn, target_child_wheezing_yn, target_child_distrubed_speech_yn,
# respondent_disturbed_speech_yn , 
# target_child_eye_red_yn, target_child_eye_itch_yn, 
# respondent_eye_red_yn, respondent_eye_itch_yn, respondent_eye_sore_yn, 


resp_vars_impacted <-
	c(
		"target_child_clinic_resp_yn", 
		"target_child_wheezing_yn",
		"target_child_distrubed_speech_yn",
		"respondent_disturbed_speech_yn", 
		"target_child_eye_red_yn",
		"target_child_eye_itch_yn", 
		"respondent_wheezing_yn", 
		"target_child_resp_rate", 
		"respondent_eye_red_yn",
		"respondent_eye_itch_yn", 
		"respondent_eye_sore_yn",
		"target_child_alri", 
		"target_child_asthma", 
		"target_child_severe_asthma",
		"target_child_cough",
		"target_child_fever",
		"target_child_resp_rate"
	)

model_data <- survey_data %>% filter(timepoint %in% c("baseline", "endline"))

dind_fxn(resp_vars_impacted, model_data)

dind_fxn("target_child_alri", model_data) ## This shows that our ALRI variable does not work well its results not significant 





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
						"persistant \ncough", "increased \nrespiratory rate \ntoday",  "current \nwheeze", 
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
		y = "Prevalence \namong children <2 years old at baseline",
		shape = "Timepoint",
		color = "Study arm"
	) + 
	facet_grid( ~ category) # fuel_30_purchased_fuel_endline

# fig_physical_health_child

survey_data %>% 
	group_by(timepoint, study_arm_overall) %>% 
	summarise(mean(respondent_resp_rate, na.rm =TRUE), 
						mean(respondent_weight_loss, na.rm = TRUE))

#Figure out where the graph is > 100 
survey_data %>% filter(timepoint == "baseline") %>% select(weight_loss_reported_respondant) %>% count(weight_loss_reported_respondant)

#At baseline the resp rate variable is 
## resp_rate_reported_respondant
## weight_loss_reported_respondant


fig_physical_health_respondent <- 
	survey_data %>%
  mutate(
    respondent_resp_rate_yn = if_else(is.na(respondent_resp_rate_yn), resp_rate_reported_respondant, respondent_resp_rate_yn), 
    respondent_weight_loss_yn = if_else(is.na(respondent_weight_loss_yn), weight_loss_reported_respondant, respondent_weight_loss_yn)) %>% 
  filter(respondent_weight_loss_yn != 99) %>% 
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
					"respondent_cough_yn", "respondent_resp_rate_yn", "respondent_weight_loss_yn", "respondent_wheezing_yn", 
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
						"respondent_cough_yn", "respondent_resp_rate_yn", "respondent_wheezing_yn", 
						#"respondent_disturbed_sleep_yn", "respondent_disturbed_speech_yn", 
						"respondent_eye_red_yn", "respondent_eye_itch_yn", "respondent_eye_sore_yn", "respondent_weight_loss_yn", "respondent_headache_yn", "respondent_backache_yn"
					), 
				labels = 
					c(
						"persistant \ncough", "increased \nrespiratory rate \ntoday",  "current \nwheeze", 
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



# ggarrange(
# 	sp,                                                 # First row with scatter plot
# 	ggarrange(bxp, dp, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
# 	nrow = 2, 
# 	labels = "A"                                        # Labels of the scatter plot
# ) 

fig_physical_health <-
	ggarrange(
		fig_physical_health_child,
		fig_physical_health_respondent,
		# fig_mental_health_respondent,
		common.legend = TRUE, legend = "right",
		nrow = 2
	)

fig_physical_health

ggsave(
	here::here("6_figures", "fig_physical_health.png"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "png"
)










