# Dif in difference function 
#This code can be used for estimating the difference in difference effect for
#any set of variables in an experiement. It can be used for a string of variables 
# by writing the var_input parameter as a vector using c()
# Author: Chris leBoa 
# Version: 2022-09-30

# Libraries
library(tidyverse)



dind_fxn <- 
	function(var_interest, data){
		
		lm_caller <- function(LHS, RHS, data){
			formula <- as.formula(paste0(LHS, " ~ ", RHS))
			output <- lm(formula, data = data)
			return(output)
		}
		
		model_list <- lapply(var_interest, lm_caller, RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num" , data = data)
		names(model_list) <- var_interest
		model_list_conf <- map(model_list, ~broom::tidy(.,conf.int = TRUE))
		
		output_table <- NULL
		
		for(i in 1:length(model_list)){
			name <- names(model_list[i])
			int <-  model_list_conf [[i]][4,c(2,6,7)]
			output_table <- bind_rows(output_table,cbind(name,int))
		}
		
		return(output_table)
	}

# The way that this model is run on the data
 #dind_fxn(var_interest, model_data)


### Testing dif in dif fxn 
#dind_fxn(c("buy_wood_cost_usd", "spent_food_cost_usd"),  survey_data %>% filter(timepoint %in% c("baseline", "endline")))

# model_data %>% 
# 	group_by(timepoint, study_arm_overall) %>% 
# 	summarise(
# 		mean(spent_food, na.rm = TRUE),
# 		mean(spent_food_cost_usd, na.rm = TRUE),
# 		mean(spent_total_month, na.rm = TRUE), 
# 		mean(spent_food, na.rm = TRUE) / mean(spent_total_month, na.rm = TRUE)
# 		#spent_food / spent_total_month
# 		)
# 
# model_data %>% 
# 	group_by(timepoint, study_arm_overall) %>% 
# 	summarise(
# 		mean(spent_food, na.rm = TRUE),
# 		mean(spent_hygiene, na.rm = TRUE), 
# 		mean(spent_hh_items, na.rm = TRUE), 
# 		mean(spent_tobacco_pan, na.rm = TRUE), 
# 		mean(spent_phone, na.rm = TRUE), 
# 		mean(spent_food_cost_usd, na.rm = TRUE),
# 		mean(spent_total_month, na.rm = TRUE), 
# 		mean(spent_food, na.rm = TRUE) / mean(spent_total_month, na.rm = TRUE)
# 		#spent_food / spent_total_month
# 	)
# 
# 
# 	
# View(model_data)
# 
# 	
#dind_fxn(mental_health_vars,  survey_data %>% filter(timepoint %in% c("baseline", "endline")))
# dind_fxn("respondent_weight_loss", model_data)
# 
# mental_health_sig_vars <- c("enjoyed_life", "self_worth", "hopeful", "sick", "no_appetite", "diff_concentrating", "fearful", "cant_get_going", "life_failure", "suicidal_thoughts_30")

    #The way the function would be written in order to produce a result. 

library(lme4)


dind_fxn_binom <- 
  function(var_interest, data){
    
    lm_caller <- function(LHS, RHS, data){
      formula <- as.formula(paste0(LHS, " ~ ", RHS))
      output <- glmer(formula, data = data, family = binomial)
      return(output)
    }
    
    model_list <- lapply(var_interest, lm_caller, RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num + (1|hh_id)" , data = data)
    names(model_list) <- var_interest
    model_list_conf <- map(model_list, ~broom::tidy(.,conf.int = TRUE))
    
    output_table <- NULL
    
    for(i in 1:length(model_list)){
      name <- names(model_list[i])
      int <-  model_list_conf [[i]][4,c(2,6,7)]
      output_table <- bind_rows(output_table,cbind(name,int))
    }
    
    return(output_table)
  }

dind_fxn_linear <- 
  function(var_interest, data){
    
    lm_caller <- function(LHS, RHS, data){
      formula <- as.formula(paste0(LHS, " ~ ", RHS))
      output <- lm(formula, data = data)
      return(output)
    }
    
    model_list <- lapply(var_interest, lm_caller, RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num" , data = data)
    names(model_list) <- var_interest
    model_list_conf <- map(model_list, ~broom::tidy(.,conf.int = TRUE))
    
    output_table <- NULL
    
    for(i in 1:length(model_list)){
      name <- names(model_list[i])
      int <-  model_list_conf [[i]][4,c(2,6,7)]
      output_table <- bind_rows(output_table,cbind(name,int))
    }
    
    return(output_table)
  }
