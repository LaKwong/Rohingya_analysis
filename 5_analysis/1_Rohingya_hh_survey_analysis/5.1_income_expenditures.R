# Comparisons of expenditures and income 

# Author: Name
# Version: 2022-10-07

# Libraries

library(tidyverse)



# Parameters


#===============================================================================

#Code


#Types of income 
survey_data_study_arm_group <- 
  survey_data %>%
  filter(timepoint == "baseline") %>% 
  mutate(
    study_arm_group = case_when(
      camp_id %in% c("8W", "9", "10") ~ "Intervention IOM", 
      camp_id %in% c("8E", "18") ~ "Comparison IOM",
      camp_id %in% c("3","4","5") ~ "Comparison UNHCR", 
      .default = NA
    )) 

## Table if incomes by study arm group 
survey_data_study_arm_group %>% 
  group_by(study_arm_group) %>% 
  summarise_at(
    vars(income,income_wage_labor, income_cash_ngo, income_humanitarian_asst, income_selling_wood, income_skill_labor, income_own_business,income_abroad, income_farming), 
    list(mean = mean, sd = sd, median = median), na.rm = TRUE
  ) %>% write_csv("baseline_income_split.csv")

## Histograms of wages by type 
income_overall_all <- 
  survey_data_study_arm_group %>% 
  ggplot(aes(x = income, color = study_arm_group)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  # scale_fill_manual(values = c( "blue", "darkgreen")) +
  scale_y_continuous(labels=scales::percent) + 
  coord_cartesian(xlim = c(0, 20000))

income_overall_unhcr_hh <- 
  survey_data_study_arm_group %>% 
  filter(study_arm_group != "Comparison UNHCR") %>%
  ggplot(aes(income)) + 
  geom_histogram(aes(y = after_stat(width*density), fill = study_arm_group), position = "dodge") + 
  scale_fill_manual(values = c( "red", "purple", "orange")) + 
  scale_y_continuous(labels=scales::percent) 

income_wage_all <- 
  survey_data_study_arm_group %>% 
  ggplot(aes(income_wage_labor)) + 
  geom_histogram(binwidth = 1000, aes(y = after_stat(width*density), fill = study_arm_overall), position = "dodge") + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c( "blue", "darkgreen"))

income_wage_unhcr_hh <- 
  survey_data_study_arm_group %>% 
  filter(study_arm_group != "Comparison UNHCR") %>%
  ggplot(aes(income_wage_labor)) + 
  geom_histogram(aes(y = after_stat(width*density), fill = study_arm_group), position = "dodge") + 
  scale_fill_manual(values = c( "red", "purple", "orange")) + 
  scale_y_continuous(labels=scales::percent) 

income_cash_ngo_all <- 
  survey_data_study_arm_group %>% 
  ggplot(aes(income_cash_ngo)) + 
  geom_histogram(aes(fill = study_arm_overall), position = "dodge") + 
  scale_fill_manual(values = c( "blue", "darkgreen"))

income_cash_ngo_unhcr_hh <- 
  survey_data_study_arm_group %>% 
  filter(study_arm_group != "Comparison UNHCR") %>%
  ggplot(aes(income_cash_ngo)) + 
  geom_histogram(aes(y = after_stat(width*density), fill = study_arm_group), position = "dodge") + 
  scale_fill_manual(values = c( "red", "purple", "orange")) + 
  scale_y_continuous(labels=scales::percent) 

income_humanitarian_asst_all <- 
  survey_data_study_arm_group %>% 
  ggplot(aes(income_humanitarian_asst)) + 
  geom_histogram(aes(fill = study_arm_overall), position = "dodge") + 
  scale_fill_manual(values = c( "blue", "darkgreen"))

income_humanitarian_asst_unhcr_hh <- 
  survey_data_study_arm_group %>% 
 # filter(income_humanitarian_asst > 0) %>% 
  filter(study_arm_group != "Comparison UNHCR") %>%
  ggplot(aes(income_humanitarian_asst)) + 
  geom_histogram(aes(y = after_stat(width*density), fill = study_arm_group), position = "dodge") + 
  scale_fill_manual(values = c( "red", "purple", "orange")) + 
  scale_y_continuous(labels=scales::percent) 

#spending by group at baseline and endline 
spending_indiv <- 
	survey_data %>% 
	select(-c(spent_total_month_with_6mo_monthly)) %>% 
	mutate(total_expend_month = spent_total_month) %>% 
	pivot_longer(cols = c(starts_with("spent"), "buy_wood_cost"), names_to = "spent_cat",values_to = "Mean_Spent", names_prefix = "spent_") 

## Used for sending baseline habits baseline 
spending_group <- 
	spending_indiv %>% 
	group_by(timepoint, study_arm_overall, spent_cat) %>%
	drop_na(spent_cat, Mean_Spent) %>% 
  mutate(Mean_Spent_usd = Mean_Spent/BDT_USD_exchange_rate_baseline) %>% 
  summarise(ci = list(mean_cl_normal(Mean_Spent_usd) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest %>% 
  view()

# 
# 	summarise(
# 		spent_mean = mean(Mean_Spent, na.rm = TRUE),
# 		spent_sd = sd(Mean_Spent, na.rm = TRUE),
# 		count = n(),
# 		pct_spent = mean(Mean_Spent) /mean(total_expend_month, na.rm = TRUE) * 100
# 	) %>% 
#   mutate(se = spent_sd / sqrt(count),
#          spent_mean_usd = spent_mean / BDT_USD_exchange_rate_baseline, 
#          lower_ci = lower_ci(smean, se, count), 
#          upper_ci = upper_ci(smean, se, count)
#          ) %>% 
# 	arrange(desc(spent_mean)) %>% 
# 	ungroup() 




#Plotting spending groups at baseline and endline 
spending_group %>% 
	ggplot(aes(x = spent_mean, y = spent_cat)) + 
	geom_point(aes(color = timepoint)) + 
	facet_grid(study_arm_overall~.) + 
	scale_color_brewer(palette = "Set1")

#Plottting changes by spending category baseline to endline
spending_group %>% 
	group_by(study_arm_overall) %>% 
	select(-spent_mean) %>% 
	filter(spent_cat != c("total_month", "rent")) %>% 
	pivot_wider(names_from = timepoint, values_from = pct_spent) %>% 
	ungroup() %>% 
	mutate(pct_change = endline - baseline) %>% 
	ggplot() +
	geom_hline(aes(yintercept = 0)) +
	geom_point(aes(x = spent_cat, y = pct_change, color = study_arm_overall)) + 
	scale_color_brewer(palette = "Set1") + 
	theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
	labs(
		title = "Change in % of household spending from baseline to endline", 
		y = "Change from baseline to endline", 
		x = "spending category"
			 	)

model_data %>% 
	filter(income < 20000, spent_total_month < 30000) %>% 
	ggplot()+
	geom_histogram(aes(x = income), fill = "green", alpha = 0.2) + 
	geom_histogram(aes(x = spent_total_month), fill = "red", alpha = 0.2) + 
#	geom_vline(aes(xintercept = mean(income, na.rm = TRUE)), color = "darkgreen") + 
#	geom_vline(aes(xintercept = mean(spent_total_month, na.rm = TRUE)), color = "red") + 
#	geom_text(aes(x = 2500, y = 250, label = round(mean(income, na.rm = TRUE), 1)), color = "darkgreen") + 
#	geom_text(aes(x = 6000, y = 250, label = round(mean(spent_total_month, na.rm = TRUE), 1)), color = "red") + 
	labs(
		x = "Taka per month", 
		y = "Number of households", 
		title = "Reported income (green) and spending (red) per month "
	) +
	facet_grid(timepoint ~ study_arm_overall)
	
model_data %>% 
	group_by(study_arm, timepoint) %>% 
	summarise(mean(income, na.rm = TRUE))

spending_group %>% 
	write_csv("7_tables/spending_by_group.csv")



#Make histograms of food expenditures 

survey_data %>% 
#	filter(income < 20000, spent_total_month < 30000) %>% 
	ggplot()+
	#geom_histogram(aes(x = income), fill = "green", alpha = 0.2) + 
	geom_histogram(aes(x = spent_food, fill = study_arm_overall), alpha = 0.2) + 
	#	geom_vline(aes(xintercept = mean(income, na.rm = TRUE)), color = "darkgreen") + 
	#	geom_vline(aes(xintercept = mean(spent_total_month, na.rm = TRUE)), color = "red") + 
	#	geom_text(aes(x = 2500, y = 250, label = round(mean(income, na.rm = TRUE), 1)), color = "darkgreen") + 
	#	geom_text(aes(x = 6000, y = 250, label = round(mean(spent_total_month, na.rm = TRUE), 1)), color = "red") + 
	labs(
		x = "Taka per month", 
		y = "Number of households", 
		title = "Spending on food per month "
	) +
	facet_grid(timepoint ~ .)

# find per hh change in food expendutures due to changes in fuel expenses 
food_final <- 
	survey_data %>% 
	filter(timepoint == "endline") %>% 
	select(fcn_id, spent_food)

change_fuel_spend <- 
	survey_data %>% 
	select(fcn_id, study_arm_overall, timepoint, buy_wood_cost) %>% 
	pivot_wider(names_from = timepoint, values_from = buy_wood_cost) %>% 
	mutate(
		baseline_usd = baseline / BDT_USD_exchange_rate_baseline, 
		endline_usd = endline / BDT_USD_exchange_rate_endline, 
		change_fuel = baseline - endline,  # switched order to show decrease spending as positve 
		change_fuel_usd =  baseline_usd - endline_usd # switched order to show decrease spending as positve 
		) 

change_fuel_spend %>% 
	group_by(study_arm_overall) %>% 
	summarise(
	 	mean_taka = mean(change_fuel/spent_food, na.rm = TRUE), 
		sd_taka = sd(change_fuel/spent_food, na.rm = TRUE),
		mean_usd = mean(change_fuel_usd/ spent_food_cost_usd, na.rm = TRUE),
		sd_usd = sd(change_fuel_usd/ spent_food_cost_usd, na.rm = TRUE)
		)

change_fuel_spend %>% 
	group_by(study_arm_overall) %>% 
	ggplot() + 
	geom_histogram(aes(x = change_fuel_usd/ spent_food_cost_usd, fill = study_arm_overall), position = "dodge") + 
	labs(title = "Decrease in fuel spending at endline as % of final food spending")


##### Dif in dif model for food spending 

model_data <- 
  survey_data %>% 
  filter(timepoint %in% c("baseline", "endline")) %>% 
  mutate(spent_food_pct = if_else(spent_food > 0, spent_total_month > 0, spent_food/spent_total_month, NA)) %>% 
  drop_na(spent_food_pct)

dind_fxn("spent_food_pct", model_data)
	
	

	
