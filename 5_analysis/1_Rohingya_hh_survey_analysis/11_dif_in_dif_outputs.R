# Difference in difference model output table 

# Author: Chris leBoa 
# Version: 2022-09-30

# Libraries
library(tidyverse)

survey_data <- read_rds(here::here("4_data/RohingyaFuel_survey_data_triple.rds"))



# Parameters
#survey_data %>% group_by(timepoint) %>% summarise(mean(weight_loss_reported_respondent, na.rm = TRUE))

#Change the parameters in this list to whatever variables you want and it will output them with confidence intervals 
var_interest <- 
	c(
		"target_child_cough",
		"target_child_resp_rate",
		"target_child_wheezing",
		"target_child_eye_red_yn",
		"target_child_eye_itch_yn",
		"target_child_lethargy",
		"target_child_weight_loss",
		"target_child_fever",
		"target_child_clinic_resp_yn",
		"target_child_wheezing_yn",
		"target_child_distrubed_speech_yn",
		
		"respondent_disturbed_speech_yn", 
		"respondent_eye_red_yn",
		"respondent_eye_itch_yn", 
		"respondent_eye_sore_yn",
		"respondent_eye_red",
		"respondent_eye_itch",
		"respondent_eye_sore",
		"respondent_headache",
		"respondent_backache",
		"respondent_wheezing",
		"respondent_disturbed_sleep",
		"respondent_disturbed_speech"
	)

#==============================================================================
model_data <- 
	survey_data %>% 
	mutate(
		study_arm_overall_num = if_else(as.character(study_arm_overall) == "intervention", 1, 0), 
		buy_wood_cost_usd = case_when(
			timepoint_num == 0 ~ buy_wood_cost / BDT_USD_exchange_rate_baseline, 
			timepoint_num == 0.5 ~ buy_wood_cost / BDT_USD_exchange_rate_midline, 
			timepoint_num == 1 ~ buy_wood_cost / BDT_USD_exchange_rate_endline), 
		spent_food_cost_usd = case_when(
			timepoint_num == 0 ~ spent_food / BDT_USD_exchange_rate_baseline, 
			timepoint_num == 0.5 ~ spent_food / BDT_USD_exchange_rate_midline, 
			timepoint_num == 1 ~ spent_food / BDT_USD_exchange_rate_endline)
		) %>% 
	filter(timepoint %in% c("baseline", "endline"))

model_data %>% group_by(timepoint_num, study_arm_overall_num) %>% summarize(mean(buy_wood_cost_usd))

lm_caller <- function(LHS, RHS, data){
	formula <- as.formula(paste0(LHS, " ~ ", RHS))
	output <- lm(formula, data = data)
	return(output)
} ## This function taken from Tanner's blog https://tkoomar.github.io/post/2020-05-02-tutorial-iterate-lm/


model_1 <- lm_caller(LHS = "buy_wood_cost_usd", RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num" , data = model_data)
summary(model_1)  
#Test that function works on one model

model_list <- lapply(var_interest, lm_caller, RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num", data = model_data)
#
names(model_list) <- var_interest

model_list_conf <- map(model_list, ~broom::tidy(.,conf.int = TRUE))


output_table <- NULL

for(i in 1:length(model_list)){
	name <- names(model_list[i])
	int <-  model_list_conf [[i]][4,c(2,6,7)]
	output_table <- bind_rows(output_table,cbind(name,int))
}

output_table



