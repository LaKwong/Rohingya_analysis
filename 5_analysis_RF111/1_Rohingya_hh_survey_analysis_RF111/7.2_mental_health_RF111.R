################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Mental health analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

## Chris
# source(here::here("1_config.R"))


# Parameters

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)




##############################################################################
# Compare CES-D depression score
###############################################################################

# CES-D = Center for Epidemiological Studies - Depression
# CES_D_score
# CES_D_o16

## CES_D

# fig.width=6, fig.height=5

survey_data %>%
	select(mental_health_bad_vars, mental_health_good_vars)

CES_D_data <-
	survey_data %>%
	rowwise() %>% 
	mutate(
		CES_D_score = sum(c_across(c(mental_health_bad_vars, mental_health_good_vars))) # use all_of() when using select and use c_across() when using other verbs
	) %>%
	ungroup() %>%
	# CES_D >16 meaning at risk for depression has not been validated in Bangladesh
	mutate(
		CES_D_o16 = ifelse(CES_D_score > 16, "at-risk", "not at risk")
	)

# select(study_arm, fcn_id, CES_D_score) %>%
# spread(key = "study_arm", value = "CES_D_score") %>%


# CES_D_comparisons <- list(c("pre-intervention", "post-intervention"), c("intervention", "intervention follow-up"))
CES_D_comparisons <- list(c("pre-intervention", "intervention"), c("post-intervention", "intervention follow-up"))

fig_CES_D_data <-
	CES_D_data %>%
	mutate(study_arm = ordered(study_arm, levels = c("pre-intervention", "intervention", "post-intervention", "intervention follow-up"))) %>%
	ggplot(aes(x = study_arm, y = CES_D_score)) + # , fill = org
	geom_violin(position = "dodge") +
	ggpubr::stat_compare_means(
		# label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
		# method = "wilcox.test",
		# paired = TRUE,
		# 
		# # label.y = 18,
		# hide.ns = TRUE#,
		# # ref.group = ".all."
		# # show.legend = TRUE # doesn't show the legend of stas?
		comparisons = CES_D_comparisons, # Add pairwise comparisons p-value
		label.y = 45
	) +
	ggpubr::stat_compare_means(label.y = 50) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 2/3
	) +
	theme_classic() +
	theme(
		axis.text.x = element_text(angle = 0, hjust = 1)
	) + 
	labs(
		title = "Change in CES_D",
		x = "study_arm",
		y = "CES_D score"
	) # +
# facet_wrap(~ org)

fig_CES_D_data

ggsave(
	here::here("6_figures", "fig_CES_D.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 5,
	units = "in",
	device = "tiff"
)



CES_D_data %>%
	tabyl(study_arm, CES_D_o16) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns() %>%
	select(study_arm, 'at-risk') # no need to also display "not at risk" since it is just 100%-'at-risk'

# average score is not very helpful
# CES_D_data %>%
# 	group_by(study_arm) %>%
# 	summarise(
# 		CES_D_score_mean = mean(CES_D_score), CES_D_score_sd = sd(CES_D_score)
# 	)

write_rds(CES_D_data, file_out_CES_D_data)

# Sep by IOM vs UNHCR
# split by insufficient LPG 


################## Wide

CES_D_data_long <-
	survey_data_base_wide %>%
	rowwise() %>% 
	mutate(
		CES_D_score.baseline = sum(c_across(c(paste0(mental_health_bad_vars, ".baseline"), paste0(mental_health_good_vars, ".baseline")))),
		CES_D_score.endline = sum(c_across(c(paste0(mental_health_bad_vars, ".endline"), paste0(mental_health_good_vars, ".endline")))),
		CES_D_score.change = sum(CES_D_score.endline, -CES_D_score.baseline, na.rm = TRUE)
	) %>%
	ungroup() %>%
	select(study_arm, fcn_id, CES_D_score.baseline, CES_D_score.endline, CES_D_score.change) %>%
	gather(-c(study_arm, fcn_id), key = "timepoint", value = "CES_D_score") %>%
	mutate(
		timepoint = 
			ordered(
				timepoint,
				levels = c("CES_D_score.baseline", "CES_D_score.endline", "CES_D_score.change"),
				labels = c("baseline", "endline", "change")
			)
	)
# # CES_D >16 meaning at risk for depression has not been validated in Bangladesh
# mutate(
# 	CES_D_o16_baseline = ifelse(CES_D_score_baseline > 16, "at-risk", "not at risk"),
# 			CES_D_o16_endline = ifelse(CES_D_score_endline > 16, "at-risk", "not at risk")
# )

# select(study_arm, fcn_id, CES_D_score) %>%
# spread(key = "study_arm", value = "CES_D_score") %>%


# CES_D_comparisons <- list(c("pre-intervention", "post-intervention"), c("intervention", "intervention follow-up"))
# CES_D_comparisons <- list(c("pre-intervention", "intervention"), c("post-intervention", "intervention follow-up"))

arrowLab <- data.frame(lab = c("Higher risk of depression"), x = c(-0.1), y = c(30))

fig_CES_D_data_change <-
	CES_D_data_long %>%
	filter(timepoint %in% c("change")) %>%
	mutate(
		timepoint = 
			ordered(
				timepoint,
				levels = c("change")
			)
	) %>%
	ggplot(aes(x = timepoint, y = CES_D_score)) + # , fill = org
	geom_violin(aes(fill = study_arm), position = "dodge") +
	# Mean_sdl adds the mean and sd to the plot
	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.9), color="red") +
	# try to add an arrow outside the left axis to show that higher values are more at risk for depression)
	# 	geom_segment(aes(x = -0.1, xend = -0.1, y= 30, yend= 35),
	#                          arrow = arrow(length = unit(0.2,"cm"))) +
	#         geom_text(data = arrowLab, aes(x = x, y = y,label = lab), size = 3) +
	ggpubr::stat_compare_means(
		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
		method = "wilcox.test",
		paired = FALSE, # baseline and endline are paired but the change is not, and on-going and intervention are not
		
		# label.y = 18,
		hide.ns = TRUE#, The baseline and endline samples are paired but the change is not
		
		
		
		################### Analyze them separately? ###################
		
		
		
		# ref.group = ".all."
		# show.legend = TRUE # doesn't show the legend of stas?
	) +
	# ggpubr::stat_compare_means(label.y = 50) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 2/3
	) +
	theme_classic() +
	theme(
		axis.text.x = element_blank(), # angle = 90
		axis.ticks.x = element_blank(),
		legend.position = "bottom",
		legend.title = element_blank()
	) +
	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
	labs(
		title = "Change in Center for Epidemiological Studies \n- Depression (CES-D) Score",
		x = "",  #"Study timepoint",
		y = "CES-D score"
	) # +
# facet_wrap(~ org)
# coord_cartesian(xlim = c(0, 1), clip = "off")

fig_CES_D_data_change

ggsave(
	here::here("6_figures", "fig_CES_D_change.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 5,
	units = "in",
	device = "tiff"
)

CES_D_data %>%
	tabyl(study_arm, CES_D_o16) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns() %>%
	select(study_arm, 'at-risk') # no need to also display "not at risk" since it is just 100%-'at-risk'

# average score is not very helpful
# CES_D_data %>%
# 	group_by(study_arm) %>%
# 	summarise(
# 		CES_D_score_mean = mean(CES_D_score), CES_D_score_sd = sd(CES_D_score)
# 	)

write_rds(CES_D_data, file_out_CES_D_data)

# Sep by IOM vs UNHCR
# split by insufficient LPG 






################################################################################
## Suicidal thoughts
#################################################################################

# fig.width=6, fig.height=5

survey_data_suicidal_thoughts <-
	survey_data %>%
	mutate_at(
		vars(suicidal_thoughts_30),
		funs(
			suicidal_thoughts_30_yn = case_when(
				. == "Never" ~ "No suicidal thoughts",
				TRUE ~ "Thought of ending own life at least once in the past 30 days"
			)
		)
	) # %>%
# select(suicidal_thoughts_30, suicidal_thoughts_30_yn)
# tabyl(suicidal_thoughts_30_yn, study_arm) 

survey_data_suicidal_thoughts %>%
	tabyl(study_arm, suicidal_thoughts_30_yn) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>% # adorn_pct_formatting(rounding = "half up", digits = 0)
	adorn_ns() %>%
	select(study_arm, 'Thought of ending own life at least once in the past 30 days') #%>% # no need to also display "not at risk" since it is just 100%-'at-risk'
# knitr::kable()

# # Currently not working
# suicide_data_long <-
# 	survey_data_base_wide %>%
# 	rowwise() %>% 
# 	mutate(
# 		suicidal_thoughts_30_yn.change = sum(suicidal_thoughts_30_yn.endline, -suicidal_thoughts_30_yn.baseline, na.rm = TRUE)
# 	) %>%
# 	ungroup() %>%
# 	select(study_arm, fcn_id, suicidal_thoughts_30_yn.baseline, suicidal_thoughts_30_yn.endline, suicidal_thoughts_30_yn.change) %>%
# 	gather(-c(study_arm, fcn_id), key = "timepoint", value = "suicidal_thoughts") %>%
# 	mutate(
# 		timepoint = 
# 			ordered(
# 				timepoint,
# 				levels = c("suicidal_thoughts_30_yn.baseline", "suicidal_thoughts_30_yn.endline", "suicidal_thoughts_30_yn.change"),
# 				labels = c("baseline", "endline", "change")
# 			)
# 	) %>%
# 	ungroup()
# 
# fig_suicide_data <-
# 	suicide_data_long %>%
# 	group_by(study_arm, timepoint) %>%
# 	summarise(suicidal_thoughts_pc = mean(suicidal_thoughts)) %>%
# 	filter(timepoint %in% c("change")) %>%
# 	ggplot(aes(x = timepoint, y = suicidal_thoughts_pc)) + # , fill = org
# 	geom_col(aes(fill = study_arm), position = "dodge") +
# 	# Mean_sdl adds the mean and sd to the plot
# 	# stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.9), color="red") +
# 	# try to add an arrow outside the left axis to show that higher values are more at risk for depression)
# 	# 	geom_segment(aes(x = -0.1, xend = -0.1, y= 30, yend= 35),
# 	#                          arrow = arrow(length = unit(0.2,"cm"))) +
# 	#         geom_text(data = arrowLab, aes(x = x, y = y,label = lab), size = 3) +
# 	ggpubr::stat_compare_means(
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # baseline and endline are paired but the change is not, and on-going and intervention are not
# 		
# 		# label.y = 18,
# 		hide.ns = TRUE#, The baseline and endline samples are paired but the change is not
# 		
# 		
# 		
# 		################### Analyze them separately? ###################
# 		
# 		
# 		
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	# ggpubr::stat_compare_means(label.y = 50) +
# 	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) +
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in suicidal thoughts \nin the past 30 days",
# 		x = "",  #"Study timepoint",
# 		y = "Percent of respondents who reported \nthinking of taking their life in the past 30 days"
# 	) # +
# # facet_wrap(~ org)
# # coord_cartesian(xlim = c(0, 1), clip = "off")
# 
# fig_suicide_data
# 
# ggsave(
# 	here::here("6_figures", "fig_suicide_change.tiff"),
# 	plot = last_plot(),
# 	scale = 1,
# 	height = 6,
# 	width = 5,
# 	units = "in",
# 	device = "tiff"
# )





hh_negative_thoughts_to_visit <-
	survey_data_suicidal_thoughts %>%
	filter(suicidal_thoughts_30_yn == 1) %>%
	select(hh_id, camp_id, subblock_id, name_respondent) 

# write.csv(hh_negative_thoughts_to_visit, "")



#################################################################################
## Generalized Anxiety Disorder (GAD-7)
#################################################################################


# Source: GAD-7-anxiety-screen
# 
# [frequency_2wks] 	0	Never
# [frequency_2wks] 	1	Several days
# [frequency_2wks] 	2	Over half the days
# [frequency_2wks] 	3	Nearly every day
# 
# This is the correct numbering for assessment. Add scores for all.
# score 
# 0-4 = no anxiety
# 5-9 = mild anxiety
# 10-14 = moderate anxiety
# 15+ = severe anxiety
# 
# "Using the threshold score of 10, the GAD-7 has a sensitivity of 89% and a specificity of 82% for GAD. It is
# moderately good at screening three other common anxiety disorders - panic disorder (sensitivity 74%,
# specificity 81%), social anxiety disorder (sensitivity 72%, specificity 80%) and post-traumatic stress
# disorder (sensitivity 66%, specificity 81%)."


survey_data %>%
	select(anxiety_vars)

anxiety_data <-
	survey_data %>%
	filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>% 
	mutate(study_arm = fct_drop(study_arm)) %>%
	rowwise() %>% 
	mutate(
		anxiety_score = sum(c_across(anxiety_vars)) # use all_of() when using select and use c_across() when using other verbs
	) %>%
	ungroup() %>%
	
	# I don't know that this has been validated in Bangladesh 
	
	mutate(
		anxiety_level =
			case_when(
				anxiety_score >= 15 ~ "severe anxiety",
				anxiety_score >= 10 ~ "moderate anxiety",
				anxiety_score >= 5 ~ "mild anxiety",
				TRUE ~ "less than mild anxiety"
			)
	) %>%
	select(fcn_id, study_arm, anxiety_vars, anxiety_score, anxiety_level)

anxiety_data %>%
	filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>% 
	mutate(study_arm = fct_drop(study_arm)) %>%
	tabyl(study_arm, anxiety_level) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# average score is not very helpful
# CES_D_data %>%
# 	group_by(study_arm) %>%
# 	summarise(
# 		CES_D_score_mean = mean(CES_D_score), CES_D_score_sd = sd(CES_D_score)
# 	)

write_rds(anxiety_data, file_out_anxiety_data)


#################################################################################
# Mental health figure
#################################################################################


# fig.width=18, fig.height=10
fig_mental_health_respondent <- 
	survey_data %>%
	select(timepoint, study_arm_overall, fcn_id, all_of(mental_health_vars_yn)) %>%
	gather(-timepoint, -study_arm_overall, -fcn_id, key = "category", value = "present") %>% # fuel_30_purchased_fuel
	group_by(timepoint, study_arm_overall, category) %>% # fuel_30_purchased_fuel_endline
	# summarise(prevalence = mean(present, na.rm = TRUE)) %>% #  sd = sd(expenditure, na.rm = TRUE)
	mutate(
		child_resp_group = 
			case_when(
				category %in% c(
					mental_health_vars_yn
				) ~ "respondent"
			)
	) %>%
	
	# 	# all
	# filter(
	# 	category %in% 
	# 		c(						 
	# 			"happy_yn", "enjoyed_life_yn", "self_worth_yn", "hopeful_yn", "bothered_yn", "sick_yn",                
	# 			"no_appetite_yn", "diff_concentrating_yn", "restless_sleep_yn", "exert_effort_yn", "less_talkative_yn", "feeling_disliked_yn",    
	# 			"unfriendly_people_yn", "fearful_yn", "lonely_yn", "crying_spells_yn", "cant_get_going_yn", "feeling_down_yn",        
	# 			"life_failure_yn", "depressed_yn",  
	# 			# "feeling_anxious_yn", "feeling_worry_yn", "feeling_not_relaxing_yn", "feeling_restless_yn",    
	# 			# "feeling_annoyed_yn", "feeling_afraid_yn", 
# 			"suicidal_thoughts_30_yn"
# 		)
# ) %>%
# mutate(
# 	category = 
# 		ordered(
# 			category,
# 			levels = 
# 				c(					
# 					"happy_yn", "enjoyed_life_yn", "self_worth_yn", "hopeful_yn", "bothered_yn", "sick_yn",                
# 					"no_appetite_yn", "diff_concentrating_yn", "restless_sleep_yn", "exert_effort_yn", "less_talkative_yn", "feeling_disliked_yn",    
# 					"unfriendly_people_yn", "fearful_yn", "lonely_yn", "crying_spells_yn", "cant_get_going_yn", "feeling_down_yn",        
# 					"life_failure_yn", "depressed_yn",  
# 					# "feeling_anxious_yn", "feeling_worry_yn", "feeling_not_relaxing_yn", "feeling_restless_yn",    
# 					# "feeling_annoyed_yn", "feeling_afraid_yn", 
# 					"suicidal_thoughts_30_yn"
# 				), 
# 			labels = 
# 				c(
# 					"happy", "enjoyed life", "as good \nas others", "hopeful",
# 					"bothered", "sick", "no appetite", "difficulty \nconcentrating", "restless \nsleep", "everything \nwas effort", "not talkative", "disliked",
# 					"unfriendly", "fearful", "lonely", "had crying \nspells", "unmotivated", "down", "failure", "depressed", 
# 					# "anxious", "worried", "unable to \nrelax", "restless", "annoyed", "afraid", 
# 					"suicidal in \npast 30 days"
# 				)
# 		)


# only vars that changed substantially
filter(
	category %in% 
		c(						 
			"happy_yn", "enjoyed_life_yn", "self_worth_yn", "hopeful_yn",                
			"no_appetite_yn",  "less_talkative_yn",   
			
			"depressed_yn",  
			# "feeling_anxious_yn", "feeling_worry_yn", "feeling_not_relaxing_yn", "feeling_restless_yn",    
			# "feeling_annoyed_yn", "feeling_afraid_yn", 
			"suicidal_thoughts_30_yn"
		)
) %>%
	mutate(
		category = 
			ordered(
				category,
				levels = 
					c(					
						"happy_yn", "enjoyed_life_yn", "self_worth_yn", "hopeful_yn",                
						"no_appetite_yn",  "less_talkative_yn",   
						
						"depressed_yn",  
						# "feeling_anxious_yn", "feeling_worry_yn", "feeling_not_relaxing_yn", "feeling_restless_yn",    
						# "feeling_annoyed_yn", "feeling_afraid_yn", 
						"suicidal_thoughts_30_yn"
					), 
				labels = 
					c(
						"happy", "enjoyed life", "as good \nas others", "hopeful",
						"no appetite",   "not talkative",
						
						"depressed", 
						# "anxious", "worried", "unable to \nrelax", "restless", "annoyed", "afraid", 
						"suicidal in \npast 30 days"
					)
			)
	) %>%
	# mutate(expenditure_LCI = expenditure_mean - sd, expenditure_UCI = expenditure_mean + sd) %>%
	ggplot(aes(x = timepoint, y = present, color = study_arm_overall, group = study_arm_overall, shape = timepoint)) + # y = prevalence
	stat_summary(fun = "mean", geom = "line", size = 1) + # , position = position_dodge(width = 0.2)
	stat_summary(fun = "mean", geom = "point", size = 2) + 
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
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
		axis.ticks.x = element_blank()#,
		# axis.line.x = element_blank(),
		# legend.position = "none"
		# legend.position = "bottom",
		# legend.direction = "horizontal"
	) + 
	guides(
		shape = guide_legend(title.position="top", title.hjust = 0.5),
		color = guide_legend(title.position="top", title.hjust = 0.5)
	) +
	labs(
		# title = "Health status over time",
		x =  "", # "Health category",
		y = "Prevalence among female caregivers",
		shape = "Timepoint",
		color = "Study arm"
	) + 
	facet_wrap( ~ category, nrow = 2) # fuel_30_purchased_fuel_endline # , labeller = label_wrap_gen(width = 25, multi_line = TRUE)

fig_mental_health_respondent

ggsave(
	here::here("6_figures", "fig_mental_health.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "tiff"
)


# mental health model
# Have each outcome on the left side with the right side being
# PAIRED
# use robust standard errors to account for pairing (same hh at baseline and endline)
# 
# outcome = C + b1 * study_arm_overall + b2 * timepoint + b3 * study_arm_overall * timepoint + E
# 
# I'd like to add a control for organization (IOM or UNHCR but I can't because then all of the comparison hh in the UNHCR arm couldn't be compared)



survey_data_did <-
	survey_data %>%
	mutate(
		treat = 
			case_when(
				study_arm_overall == "intervention" ~ 1,
				study_arm_overall == "comparison" ~ 0
			),
		time = 
			case_when(
				timepoint == "baseline" ~ 0,
				timepoint == "endline" ~ 1
			)
	) %>%
	mutate_at(
		vars(c(all_of(mental_health_vars_yn))), 
		funs(
			case_when(
				. == "Yes" ~ 1,
				. == "No" ~ 0,
				TRUE ~ NA_real_
			)
		)
	)
