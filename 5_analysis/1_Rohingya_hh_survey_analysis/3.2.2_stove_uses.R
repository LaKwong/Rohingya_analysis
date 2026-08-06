################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")


# Load input files
survey_data <- read_rds(file_survey_data_base)

survey_data_baseline_endline <-
  survey_data %>% filter(timepoint %in% c("baseline", "endline"))

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


##################################################
####### Intervention impact ########################
##################################################
# Was there a change in the percent using LPG from baseline to endline?

# define first model - mixed logistic regression
glmer_binom <- function(outcome, data, timepoint1, timepoint2) {
  glmer(
    formula(paste0(outcome, " ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1|fcn_id)")),
    family = binomial(link = "logit"),
    data = data %>% filter(timepoint %in% c(timepoint1, timepoint2)),
    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
  )
}

# function to clean model output
mod_output <- function(mod_fxn, outcome, data, timepoint1, timepoint2) {
  
  mod <- mod_fxn(outcome, data, timepoint1, timepoint2)
  
  mod_out <- 
    tidy(mod) %>%
    filter(term == "study_arm_overall.L:timepoint.L") %>%
    mutate(
      outcome = outcome,
      OR = exp(estimate),
      lower = exp(estimate + qnorm(0.025) * std.error),
      upper = exp(estimate - qnorm(0.025) * std.error),
      p_value = p.value
    ) %>%
    mutate_at(vars(OR:upper), ~round(., 3)) %>%
    mutate(p_value = round(p_value, 5)) %>%
    select(outcome, OR:p_value) %>%
    return()
}

# define list of outcomes
# could call the cols by index 
# outcome_list <- names(data_ind_unique_hh_mem)[c(12, 11, 8, 11, 22, 25, 24, 26)]
# but I'm worried about messing up the index so instead will call directly
outcome_list <- c(
  "fuel_30_gather_scraps", "fuel_30_collect_wood", "fuel_30_buy_wood", "fuel_30_receive_wood", 
  "fuel_30_receive_lpg", "fuel_30_buy_lpg", 
  "fuel_30_receive_crh", "fuel_30_buy_crh", "fuel_30_other"
)

# model diagnostics
mod1_test <- glmer_binom("fuel_30_receive_lpg", data = survey_data, "baseline", "endline")

summary(mod1_test)
# Calculate the variable inflation factor for each of the predictor vars. If VIF >10 then there is likely colinearity
car::vif(mod1_test)
# The residuals should be randomly scattered around the horizontal line to indicate that the linearity assumption is reasonable
plot(mod1_test)
# Generat predicted probabilities
p = predict(mod1_test, survey_data_baseline_endline, type = "response")
# if create a contingency table where p >0.5 sets the prediction to TRUE, 
con_table = table(p > 0.5, survey_data_baseline_endline$fuel_30_receive_lpg)
# The contingency table shows true positives, true negatives, false positives and false negatives. False positives and false negatives should be small
con_table

output_mod_fuel_30_days <-
  outcome_list %>%
  map_df(~mod_output(glmer_binom, outcome = ., data = survey_data, timepoint1 = "baseline", timepoint2 = "endline"))


# Check that the equation works as expected
# binomial mixed effects regression
fuel_30_receive_lpg_mod <-
  glmer(
    fuel_30_receive_lpg ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), 
    family = binomial(link = "logit"), 
    data = survey_data_baseline_endline, 
    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
  ) # initally failed to converge so added: , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
summary(fuel_30_receive_lpg_mod)




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

#Comp mid
182/437  #41.6
#Comp end 
169/439 #38.4

#int mid 
138/496 #27.8
#int end 
156/494 #31.5


#############################################################
#### FUEL RUN OUT Action 


