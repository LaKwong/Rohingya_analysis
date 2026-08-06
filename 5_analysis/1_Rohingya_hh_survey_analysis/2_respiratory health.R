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



###############################################################################
# Reduction in PM2.5 due to intervention?
##############################################################################

# 
# pm_bm_mod1 <- lm_robust(
# 	data = hh_pm_average_location %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = hh_mean ~ timepoint + study_arm + timepoint * study_arm,
# 	clusters = hh_id
# )
# summary(pm_bm_mod1)
# 
# # pm_bm_mod1 <- lm_robust(
# # 	data = hh_pm_average_location %>% filter(timepoint %in% c("baseline", "midline")),  
# # 	formula = PM_Estimate ~ timepoint + study_arm + timepoint * study_arm,
# # 	clusters = pm_data_nearest_min %>% filter(timepoint %in% c("baseline", "midline")) %>% pull(hh_id)
# # )
# # 
# # pm_be_mod1 <- lm(
# # 	data = hh_pm_average_location %>% filter(timepoint %in% c("baseline", "endline")),  
# # 	formula = PM_Estimate ~ timepoint + study_arm + timepoint * study_arm
# # )
# 
# pm_be_mod1 <- lm_robust(
# 	data = hh_pm_average_location %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = hh_mean ~ timepoint + study_arm + timepoint * study_arm,
# 	clusters = hh_id
# )
# summary(pm_be_mod1)
# 


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
		"target_child_eye_red_yn", "target_child_eye_itch_yn", 
		"respondent_eye_red_yn", "respondent_eye_itch_yn", "respondent_eye_sore_yn", "target_child_cough", "target_child_fever"
	)

model_data <- survey_data %>% filter(timepoint %in% c("baseline", "endline"))

dind_fxn(resp_vars_impacted, model_data)



# 
# # Apparently I can use stargazer but can't call lm_robust directly: https://declaredesign.org/r/estimatr/articles/regression-tables.html
# library(stargazer)
# 
# survey_data_test <- 
# 	survey_data %>% 
# 	mutate(study_arm_overall_num = if_else(study_arm_overall == "intervention", 1, 0))
# 
# mod1 <- lm(
# 	data = survey_data_test %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = buy_wood_cost ~ timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num
# )
# 
# mod1
# 
# 
# model1 <- tidy(mod1, conf.int=TRUE) %>% 
# 	filter(str_detect(term, ":")) %>% 
# 	select(estimate, conf.low, conf.high)
# 
# 
# mod2 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = target_child_wheezing_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod3 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = target_child_distrubed_speech_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod4 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = respondent_disturbed_speech_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod5 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_eye_red_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod6 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_eye_itch_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod7 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_red_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod8 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_itch_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# mod9 <- lm(
# 	data = survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_sore_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall
# )
# summary(mod1) # * #estimated difference in difference of 4.99%
# summary(mod2) # not sig
# summary(mod3) # not sig
# summary(mod4) # ***
# summary(mod5) # not sig
# summary(mod6) # **
# summary(mod7) # ***
# summary(mod8) # ***
# summary(mod9) # **
# 
# 
# stargazer(
# 	mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, 
# 	ci.custom = starprep(
# 		mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, 
# 		# this would work except there are some missing observation so the number of clusters isn't matching.....
# 		# clusters = survey_data %>% filter(timepoint %in% c("baseline", "midline")) %>% pull(fcn_id),
# 		# I'll leave this for now and fix it when we get to the paper. 
# 		stat = "ci"
# 	)
# )
# 
# survey_data %>% filter(timepoint %in% c("baseline", "midline")) %>% View()
# length(survey_data %>% filter(timepoint %in% c("baseline", "midline")) %>% pull(fcn_id))
# dim(survey_data %>% filter(timepoint %in% c("baseline", "midline")))
# 
# # Try using lm_robust
# mod1_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "endline")),  
# 	formula = buy_wood_cost ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod2_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_wheezing_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod3_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_distrubed_speech_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod4_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_disturbed_speech_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod5_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_eye_red_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod6_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = target_child_eye_itch_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod7_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_red_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod8_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_itch_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# mod9_robust <- lm_robust(
# 	survey_data %>% filter(timepoint %in% c("baseline", "midline")),  
# 	formula = respondent_eye_sore_yn ~ timepoint + study_arm_overall + timepoint * study_arm_overall,
# 	clusters = fcn_id
# )
# 
# summary(mod1_robust) # *
# summary(mod2_robust) # not sig
# summary(mod3_robust) # not sig
# summary(mod4_robust) # ***
# summary(mod5_robust) # not sig
# summary(mod6_robust) # **
# summary(mod7_robust) # ***
# summary(mod8_robust) # ***
# summary(mod9_robust) # **

# # stargazer doesn't work with results from lm_robust
# stargazer(mod1_robust, mod2_robust, mod3_robust, title  = "test", align = TRUE)


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










