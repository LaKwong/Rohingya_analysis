################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################

source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R")) 

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")


# Load input files
survey_data <- read_rds(file_survey_data_base)

survey_data_baseline <-
	survey_data %>%
	filter(timepoint == "baseline")

################################################################################
# Fuel sources
################################################################################

### Fuel used in the past 30 days

fuel_used_30_days <- 
	survey_data %>% 
	select(
		fcn_id,
		study_arm_overall,
		timepoint,
		fuel_30_gather_scraps,
		fuel_30_collect_wood,
		fuel_30_receive_wood,
		fuel_30_buy_wood,
		fuel_30_receive_lpg,
		fuel_30_buy_lpg,
		fuel_30_receive_crh,
		fuel_30_buy_crh,
		fuel_30_other
	) %>%
	pivot_longer( 
		cols = c(
			fuel_30_gather_scraps,
			fuel_30_collect_wood,
			fuel_30_receive_wood,
			fuel_30_buy_wood,
			fuel_30_receive_lpg,
			fuel_30_buy_lpg,
			fuel_30_receive_crh,
			fuel_30_buy_crh,
			fuel_30_other
		),
		names_to = "fuel"
	) %>%
	mutate(value = ifelse(is.na(value), 0, value)) %>%
	mutate(fuel = str_remove(fuel, "^.{8}")) %>%
	mutate(
		study_arm_overall = 
			ordered(
				study_arm_overall,
				levels = c("comparison", "intervention"),
				labels = c("Comparison group", "Intervention group")
			)
	) %>%
	mutate(
		fuel = 
			ordered(
				fuel,
				levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
				labels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
			)
	)


fig_fuel_used_30_days <-
	fuel_used_30_days %>%
	group_by(fuel, study_arm_overall, timepoint) %>%
	summarise(prop = mean(value)) %>%
	ggplot() + 
	geom_point(aes(x = timepoint, y = prop, color = fuel, group = fuel)) + 
	geom_line(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
	scale_y_continuous(labels = scales::percent) +
	labs(
		title = "Types of Cooking Fuel Used in the Past 30 Days, By Study Arm and Timepoint",
		color = "Fuel",
		x = "Timepoint", 
		y = "Percent of households (%)"
	) + 
	theme_bw() + 
	facet_wrap(. ~ study_arm_overall)

## Fuel 30 graph simplified 
fuel_used_30_days_simple_wide <-
  survey_data %>%
  group_by(study_arm_overall, timepoint) %>%
  mutate(
    wood = case_when(
     fuel_30_gather_scraps == 1 ~ 1, 
     fuel_30_buy_wood == 1 ~ 1, 
     fuel_30_collect_wood == 1 ~ 1, 
     fuel_30_receive_wood == 1 ~ 1, 
     .default = 0
    ), 
    lpg = case_when(
      fuel_30_buy_lpg == 1 ~ 1, 
      fuel_30_receive_lpg == 1 ~ 1, 
      .default = 0
    ),
    other = case_when(
      fuel_30_gather_scraps == 1 ~ 1, 
      fuel_30_buy_wood == 1 ~ 1, 
      fuel_30_collect_wood == 1 ~ 1, 
      fuel_30_receive_wood == 1 ~ 1, 
      fuel_30_buy_crh == 1 ~ 1,
      fuel_30_receive_crh == 1 ~ 1, 
      fuel_30_other == 1 ~ 1, 
      .default = 0
    ),
  ) 


fuel_used_30_days_simple <-  ## THe percent of hh that used wood and lpg in the last 30 days 
  fuel_used_30_days_simple_wide %>% 
  mutate(wood_and_lpg = if_else(wood == 1 & (fuel_30_receive_lpg == 1 | fuel_30_buy_lpg == 1), 1, 0)) %>% 
  pivot_longer(cols = c(wood, fuel_30_receive_lpg, fuel_30_buy_lpg, wood_and_lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(fuel_simple, timepoint, study_arm_overall) %>% 
  summarise(
    prop = mean(value), 
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n)) %>% 
  #mutate(lower = prop -ci, upper = prop + ci) %>% 
  ungroup() 

fuel_used_30_days_simple <-  ## THe percent of hh that used wood and lpg in the last 30 days 
  fuel_used_30_days_simple_wide %>% 
  mutate(wood_and_lpg = if_else(wood == 1 & (fuel_30_receive_lpg == 1 | fuel_30_buy_lpg == 1), 1, 0)) %>% 
  pivot_longer(cols = c(wood, fuel_30_receive_lpg, fuel_30_buy_lpg, wood_and_lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(fuel_simple) %>% 
  summarise(
    prop = mean(value), 
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n)) %>% 
  #mutate(lower = prop -ci, upper = prop + ci) %>% 
  ungroup() 



## Provide the # of households that used each fuel type at endline 

fuel_used_30_days_simple_wide %>% 
  group_by(timepoint, study_arm_overall) %>% 
  count(lpg, wood)

fuel_used_30_days_simple_wide %>% 
  group_by(timepoint) %>% 
  mutate(fuel_30_collect_wood_scrap = if_else(fuel_30_collect_wood == 1 | fuel_30_gather_scraps == 1, 1, 0)) %>% 
  count(fuel_30_buy_wood, fuel_30_collect_wood_scrap )
  
## Costs of LPG households pay 

survey_data %>% 
  group_by(timepoint) %>% 
  filter(buy_lpg_cost >0) %>% 
  count(study_arm_overall)
 # summarise(mean(buy_lpg_cost))
  
54/(437) # Divide number spent on lpg by the number in the group 
86/494

504/84.74 # Paid LPG at midline 
314/93.45 # Paid LPG endline 

survey_data$buy_lpg_cos































######## 
#The following code was not used in the paper 
########

#### Dif in dif analysis of fuel type 

model_data <-survey_data %>%  filter(timepoint %in% c("baseline", "endline") )
dind_fxn(c("fuel_30_gather_scraps", "fuel_30_collect_wood", "fuel_30_buy_wood"), model_data)



percent_plot <- 
  fuel_used_30_days %>% 
  group_by(fuel, study_arm_overall, timepoint) %>%
  summarise(
    mean.mpg = mean(value),          
    sd.mpg = sd(value, na.rm = TRUE),
    n.mpg = n()) %>%
  mutate(
    se.mpg = sd.mpg / sqrt(n.mpg),
    lower.ci.mpg = mean.mpg - qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
   upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg) %>% 
  #pivot_wider(names_from = timepoint, values_from = c(mean.mpg, lower.ci.mpg, upper.ci.mpg)) %>% 
  mutate(
    mean_chg = mean.mpg - mean.mpg[1], 
  ) %>% 
  #select(-c(baseline,midline,endline)) %>% 
  pivot_longer(cols = starts_with("year"), names_to = "timepoint", values_to = "pct_change")
class(percent_plot) 

ggplot(data = percent_plot, aes(x = fuel, y = pct_change, color = timepoint, group = timepoint)) + 
  geom_point(position = position_dodge(width=0.5)) +
 # geom_errorbar(position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Types of Cooking Fuel Used in the Past 30 Days, By Study Arm and Timepoint",
    color = "Time Point",
    x = "Fuel Type", 
    y = "Percent Change Use (%)"
  ) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(. ~ study_arm_overall)


ggsave(
	here::here("6_figures", "fig_fuel_used_30_days.png"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "png"
)

##### Table of fuel used in past 30 days

fuel_use_30_days_wide <-
	survey_data %>% 
	select(
		fcn_id,
		study_arm_overall,
		timepoint,
		fuel_30_gather_scraps,
		fuel_30_collect_wood,
		fuel_30_receive_wood,
		fuel_30_buy_wood,
		fuel_30_receive_lpg,
		fuel_30_buy_lpg,
		fuel_30_receive_crh,
		fuel_30_buy_crh,
		fuel_30_other
	) %>%
	pivot_longer( 
		cols = c(
			fuel_30_gather_scraps,
			fuel_30_collect_wood,
			fuel_30_receive_wood,
			fuel_30_buy_wood,
			fuel_30_receive_lpg,
			fuel_30_buy_lpg,
			fuel_30_receive_crh,
			fuel_30_buy_crh,
			fuel_30_other
		),
		names_to = "fuel"
	) %>%
	mutate(value = ifelse(is.na(value), 0, value)) %>%
	count(fuel, study_arm_overall, timepoint, value) %>%
	group_by(fuel, study_arm_overall, timepoint) %>%
	mutate(prop = prop.table(n) * 100) %>%
	mutate(fuel = str_remove(fuel, "^.{8}"))  %>%
	pivot_wider(names_from = value, values_from = c(n, prop)) %>%
	select(-c(n_0, prop_0)) %>%
	replace_na(list(n_1 = 0L, prop_1 = 0)) %>% # need 0L because n_1 is an integer
	pivot_wider(names_from = c("timepoint", "study_arm_overall"), values_from = c("n_1", "prop_1"), names_vary = "slowest") %>% # slowest" makes it vary first by the values then the names
	mutate(
		fuel = 
			ordered(
				fuel,
				levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
				labels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
			)
	) %>%
	ungroup %>%
	arrange(fuel) %>%
	select(
		fuel, 
		n_1_baseline_comparison, prop_1_baseline_comparison, n_1_midline_comparison, prop_1_midline_comparison, n_1_endline_comparison, prop_1_endline_comparison,
		n_1_baseline_intervention, prop_1_baseline_intervention, n_1_midline_intervention, prop_1_midline_intervention, n_1_endline_intervention, prop_1_endline_intervention
	)

write_csv(fuel_use_30_days_wide, "fuel_use_30.csv")



fuel_use_30_days_wide %>%
	kbl(
		caption = "Fuel Use in the Past 30 Days, By Timepoint ",
		booktabs = TRUE,
		digits = 1, # only rounded the dbl (didn't add digits to int), which is good 
		format = "latex"
	) %>%
	add_header_above(c("Fuel", "n", "%", "n", "%", "n", "%",  "n", "%", "n", "%", "n", "%")) %>%
	add_header_above(c("", "Baseline" = 2, "Midline" = 2, "Endline" = 2, "Baseline" = 2, "Midline" = 2, "Endline" = 2)) %>%
	add_header_above(c("", "Comparison" = 6, "Intervention" = 6)) %>%
	# remove the header manually
	kable_styling(latex_options = c("scale_down")) # "striped",


##############################################################################
# Intervention effects on types of fuel used in past 30 days
##############################################################################

fuel_used_30_days_reg <-
	survey_data %>% 
	select(
		fcn_id,
		study_arm_overall,
		timepoint,
		fuel_30_gather_scraps,
		fuel_30_collect_wood,
		fuel_30_receive_wood,
		fuel_30_buy_wood,
		fuel_30_receive_lpg,
		fuel_30_buy_lpg,
		fuel_30_receive_crh,
		fuel_30_buy_crh,
		fuel_30_other
	) %>%
	pivot_longer( 
		cols = c(
			fuel_30_gather_scraps,
			fuel_30_collect_wood,
			fuel_30_receive_wood,
			fuel_30_buy_wood,
			fuel_30_receive_lpg,
			fuel_30_buy_lpg,
			fuel_30_receive_crh,
			fuel_30_buy_crh,
			fuel_30_other
		),
		names_to = "fuel"
	) %>%
	mutate(value = ifelse(is.na(value), 0, value)) %>%
	mutate(fuel = str_remove(fuel, "^.{8}")) %>%
	pivot_wider(names_from = "fuel", values_from = "value") %>%
	rowwise() %>%
	mutate(wood = ifelse((collect_wood + receive_wood + buy_wood) >= 1, 1, 0))

# mutate(
# 	fuel = 
# 		ordered(
# 			fuel,
# 			levels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected"),
# 			labels = c("Scraps", "Wood", "Wood", "Wood", "Compressed rice husks", "Compressed rice husks", "LPG, purchased", "LPG, received", "Plastic") # We know from the qualitative interviews that Other == Plastic
# 		)
# ) %>%

# group_by(fuel, study_arm_overall, timepoint) %>%
# summarise(prop = mean(value))


# Whether you use wood or not is binary so we need a Poisson 




###############################################################################
# Stove use
###############################################################################

# Percent of hh that use their stoves for the following reasons

# will need to separate pre-intervention, intervention, and post-intervention groups --> of course, will need to separate for every analysis...

stove_use_data <-
	survey_data %>%
	mutate_at(
		vars(			
			stove_boil_drink, # Pc of hh that boil drinking water
			stove_boil_bathe, # Pc of hh that boil bathing water
			stove_reason_stay_warm, # Pc of hh that use the stove to stay warm
			stove_reason_cook_together, # Pc of hh that use the stove to stay warm
			stove_reason_sell_food, # Pc of hh that use the stove to cook food to sell
		),
		list(as.character) # This codes factors as 1 and 2 instead of 0 and 1, unless you read in the chacter
	) %>%
	mutate_at(
		vars(
			stove_boil_drink, # Pc of hh that boil drinking water
			stove_boil_bathe, # Pc of hh that boil bathing water
			stove_reason_stay_warm, # Pc of hh that use the stove to stay warm
			stove_reason_cook_together, # Pc of hh that use the stove to stay warm
			stove_reason_sell_food, # Pc of hh that use the stove to cook food to sell
		),
		list(as.numeric) # This codes factors as 1 and 2 instead of 0 and 1, unless you read in the chacter
	) %>%
	select(
		fcn_id, 
		study_arm_overall,
		timepoint,
		traditional_use_yesterday, # Number of times traditional stove was used yesterday
		LPG_use_yesterday, # Number of times LPG stove was used yesterday
		stove_boil_drink, # Pc of hh that boil drinking water
		stove_boil_bathe, # Pc of hh that boil bathing water
		boil_yesterday_times, # Number of times the stove was used for boiling water yesterday
		stove_reason_stay_warm, # Pc of hh that use the stove to stay warm
		stove_reason_cook_together, # Pc of hh that use the stove to stay warm
		stove_reason_sell_food, # Pc of hh that use the stove to cook food to sell
		# cook_sell_yesterday is associated with the question "In the past 7 days, how many days did you cook food to sell?" so 7 is a reasonable answer
		# cook_sell_yesterday, # Of hh that cook food to sell, the number of times they cooked food to sell yesterday (maybe don't include this because cooking to sell was so rare)
		cook_sell_days_week,
		# cook_to_sell, # Of hh that cook food to sell, the percentage of total food cooked that they sell
		cook_to_sell_percent
	)

stove_use_data %>%
	group_by(study_arm_overall, timepoint) %>%
	summarise_at(
		vars(
			traditional_use_yesterday, # Number of times traditional stove was used yesterday
			LPG_use_yesterday, # Number of times LPG stove was used yesterday
			stove_boil_drink, # Pc of hh that boil drinking water
			stove_boil_bathe, # Pc of hh that boil bathing water
			boil_yesterday_times, # Number of times the stove was used for boiling water yesterday
			stove_reason_stay_warm, # Pc of hh that use the stove to stay warm
			stove_reason_cook_together, # Pc of hh that use the stove to stay warm
			stove_reason_sell_food, # Pc of hh that use the stove to cook food to sell
			# cook_sell_yesterday is associated with the question "In the past 7 days, how many days did you cook food to sell?" so 7 is a reasonable answer
			# cook_sell_yesterday, # Of hh that cook food to sell, the number of times they cooked food to sell yesterday (maybe don't include this because cooking to sell was so rare)
			cook_sell_days_week,
			# cook_to_sell, # Of hh that cook food to sell, the percentage of total food cooked that they sell
			cook_to_sell_percent
		),
		list(mean),
		na.rm = TRUE
	) %>% view()

stove_use_data %>%
	filter(timepoint == "endline") %>%
	summarise_at(
		vars(
			traditional_use_yesterday, # Number of times traditional stove was used yesterday
			LPG_use_yesterday, # Number of times LPG stove was used yesterday
			stove_boil_drink, # Pc of hh that boil drinking water
			stove_boil_bathe, # Pc of hh that boil bathing water
			boil_yesterday_times, # Number of times the stove was used for boiling water yesterday
			stove_reason_stay_warm, # Pc of hh that use the stove to stay warm
			stove_reason_cook_together, # Pc of hh that use the stove to cook for relatives/friends
			stove_reason_sell_food, # Pc of hh that use the stove to cook food to sell
			# cook_sell_yesterday is associated with the question "In the past 7 days, how many days did you cook food to sell?" so 7 is a reasonable answer
			# cook_sell_yesterday, # Of hh that cook food to sell, the number of times they cooked food to sell yesterday (maybe don't include this because cooking to sell was so rare)
			cook_sell_days_week,
			# cook_to_sell, # Of hh that cook food to sell, the percentage of total food cooked that they sell
			cook_to_sell_percent
		),
		list(mean),
		na.rm = TRUE
	) %>%
	pivot_longer(
		cols = 
			c(
				"traditional_use_yesterday", "LPG_use_yesterday", 
				"stove_boil_drink", "stove_boil_bathe", "boil_yesterday_times", "stove_reason_stay_warm", 
				"stove_reason_cook_together", "stove_reason_sell_food", "cook_sell_days_week", "cook_to_sell_percent"
			),
		names_to = "use", 
		values_to = "num_or_pc"
	)

# At endline, when most hh had LPG, groups combined used as follows
# use                        num_or_pc
# <chr>                          <dbl>
# 1 traditional_use_yesterday    0.820  # of the hh that reported using anything except their lpg stove yesterday
# 2 LPG_use_yesterday            3.60   # of hh that reported receiving or purchasing LPG
# 3 stove_boil_drink             0.881  
# 4 stove_boil_bathe             0.846  
# 5 boil_yesterday_times         0.904  
# 6 stove_reason_stay_warm       0.0139 
# 7 stove_reason_cook_together   0.952  
# 8 stove_reason_sell_food       0.00506
# 9 cook_sell_days_week          4.25   
# 10 cook_to_sell_percent         1.75 

### For hh that reported purchasing firewood, is this because they were selling wood or were using extra fuel to stay warm, cood for others, or cook to sell?





###############################################################################
# Firewood dead? 
###############################################################################

# ## vegetation_dead when collected for fuel?
# 1	Yes, the scraps/wood I collect are all dead
# 2	I collect some scraps/wood that are dead and I gather scraps/leaves/twigs while they are still alive
# 3	I cut down live plants for the twigs/wood I need

survey_data %>%
	filter(timepoint == "baseline") %>% # select only baseline because this is when they will have the best memory
	select(gather_scraps_dead, gather_wood_dead) %>%
	
	# use the following code to get the output for all options for all vars
	mutate(across(.fns = as.character)) %>%
	pivot_longer(cols = everything(), names_to = "var") %>%
	count(var, value, name = 'count') %>%
	group_by(var) %>%
	mutate(N = prop.table(count) * 100)

# var                value count     N
# <chr>              <chr> <int> <dbl>
# 1 gather_scraps_dead 1       284 35.9 
# 2 gather_scraps_dead 2       187 23.6 
# 3 gather_scraps_dead 3        22  2.78
# 4 gather_scraps_dead NA      298 37.7 (these ppl didn't gather scraps)
# 5 gather_wood_dead   1       295 37.3 
# 6 gather_wood_dead   NA      496 62.7

###############################################################################
## Wood for reasons other than cooking
###############################################################################

# "Have you or your family ever collected firewood for purposes other than your own household's cooking needs (cooking food to sell, etc)?"


survey_data %>%
	select(gather_wood) %>%
	table()

# 21 report gathering wood for reasons other than their own household's cooking - how does this compare to the number that cook to sell (1?), the number that cook to stay warm (?) + the number that cook for non-household member (guessing these people didn't include themselves)



###############################################################################
## Forest for reasons other than wood
###############################################################################

# "Have you or your family ever gone to the forest to take anything other than wood (food, household materials, etc)?"

survey_data %>%
	select(forest_collect_not_wood) %>%
	table()


## Vector of variables to summarize
table10Vars <- 
	c(
		"reason_forest_food", "reason_forest_med", "reason_forest_shelter",
		"reason_forest_privacy", "reason_forest_defecation", "reason_forest_leisure",
		"reason_forest_other", "reason_forest_other_specified"
	)

## Vector of categorical variables that need transformation
table10FactorVars <- table10Vars

# Create a TableOne object
tab10 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table10Vars, 
		factorVars = table10FactorVars, 
		strata = "study_arm_overall"
	)

print(tab10)

survey_data %>%
	ggplot(aes(x = cost_forest_not_wood)) +
	geom_histogram() + 
	geom_vline(aes(xintercept = mean(cost_forest_not_wood, na.rm = TRUE), col = 'red'), size = 2, show.legend = TRUE) + 
	geom_vline(aes(xintercept = median(cost_forest_not_wood, na.rm = TRUE), col = 'blue'), size = 2, show.legend = TRUE) +
	scale_colour_manual("Legend", values = c(red = "red", blue = "blue"), labels = c("mean", "median"))

survey_data %>%
	summarise_at(vars(cost_forest_not_wood), list(mean), na.rm = TRUE)

survey_data %>%
	filter(forest_collect_not_wood == 1) %>%
	select(cost_forest_not_wood, starts_with("reason_forest")) %>%
	arrange(desc(cost_forest_not_wood))

fuel_used_30_days_reg %>%
	 	group_by(timepoint, study_arm_overall) %>%  #, study_arm_overall, timepoint) 
	 	summarise(prop = sum(collect_wood) / n())


# 14% of 36 people replied that they go to the forest for "other" reasons but no other reasons are specified - why?...becuase on Q487 cost_forest_not_wood the relevance is wrong...selected({reason_forest},'66') should have been selected({reason_forest_other},'1')...oops
# 
# --> ask about this in the fgd?
# 	
# 	Three hh report paying <100 BDT to go into the forest for non-wood. This is ever more than was paid to collect firewood. Interestingly, the one hh that paid 300 BDT was the only hh that went to the forest for privacy (and they recorded privacy as the only reason for going)

########################################################################
#Run out of LPG
#######################################################################

freq_cook <- survey_data %>% 
  filter(!fuel_use_non_lpg_freq_cook > 200 | is.na(fuel_use_non_lpg_freq_cook)) %>% 
	group_by(study_arm_overall, timepoint) %>% 
  mutate(
    run_out = if_else(fuel_use_non_lpg_freq_cook > 0, 1, 0)
    ) 
  


  
freq_cook %>% 
	summarize(
	      mean = mean(fuel_use_non_lpg_freq_cook,  na.rm = TRUE),    
	      sd = sd(fuel_use_non_lpg_freq_cook, na.rm = TRUE),
	      n = n(), 
	      pct_out = sum(run_out, na.rm = TRUE)/n
	      ) %>%
	    mutate(
	      se = sd/ sqrt(n),
	      lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
	      upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
	      lower.ci.prop = pct_out - 1.96 * sqrt((pct_out/(1-pct_out)/n)), 
	      upper.ci.prop = pct_out + 1.96 * sqrt((pct_out/(1-pct_out)/n)),
	      )  %>% 
	view()


survey_data %>% 
  filter(fuel_use_non_lpg_freq_cook < 200) %>% 
  ggplot() + 
  geom_histogram(aes(fuel_use_non_lpg_freq_cook)) + 
  labs(x = "How many days before your last LPG refill did your household run out of LPG for cooking food?")




#### IF you instead use the lpg days possible numbers and the 2020 refill schedule 
survey_data %>% 
	filter(lpg_days_possible > 10) %>% 
	mutate(
		days_from_refill = 
			case_when(
				hh_size < 3 ~ 45 - lpg_days_possible, 
				hh_size < 7 ~ 35 - lpg_days_possible, 
				hh_size < 11 ~ 29 - lpg_days_possible, 
				hh_size >= 11  ~ 22 - lpg_days_possible
			) 
	) %>% 
	#	group_by(study_arm, timepoint) %>% 
	filter(days_from_refill < 0) %>% 
	count(study_arm_overall, timepoint)

survey_data %>% count(study_arm_overall, timepoint)

#Proportion of population that ran out of fuel before refill 
#############################################################



