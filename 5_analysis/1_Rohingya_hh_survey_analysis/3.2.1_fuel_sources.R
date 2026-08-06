################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################

#write.csv(fuel_used_ever_summary, file_out_1)

##############################################################################
# Make a figure

fig_fuel_used_ever <-
  fuel_used_ever_summary %>%
  ggplot() + 
  geom_point(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
  geom_line(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
  # Not sure why value/sum(value) isn't working to get the proportion
  # geom_line(aes(x = timepoint, y = value/sum(value), color = fuel, group = fuel)) +
  geom_errorbar(aes(x = timepoint, ymin = lower, ymax = upper, color = fuel), width = 0.1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Types of Cooking Fuel Used Ever, By Study Arm and Timepoint",
    color = "Fuel type",
    x = "Timepoint",
    y = "Percent of households using specified fuel type (%)"
  ) +
  theme_bw() +
  facet_wrap(~ study_arm_overall)

fig_fuel_used_ever

ggsave(
  here::here("6_figures", "fig_fuel_used_ever.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)


################################################################################
# Fuel used in the past 30 days
################################################################################

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
  # Add a market that each row is one observation
  mutate(n = 1)


fuel_used_30_days_summary <-
  fuel_used_30_days %>%
  group_by(study_arm_overall, timepoint, fuel) %>%
  summarise(
    prop = mean(value),
    sd = sd(value),
  )
write.csv(fuel_used_30_days_summary, file_out_2)

##############################################################################
# Make a figure

fig_fuel_used_30_days <-
  fuel_used_30_days_summary %>%
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
        labels = c("Scraps, gathered", "Wood, harvested", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
      )
  ) %>%
  ggplot() + 
  geom_point(aes(x = timepoint, y = prop, color = fuel, group = fuel)) + 
  geom_line(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
  geom_errorbar(aes(x = timepoint, ymin = lower, ymax = upper, color = fuel), width = 0.1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "Types of Cooking Fuel Used in the Past 30 Days, By Study Arm and Timepoint",
    color = "Type of fuel used in past 30 days",
    x = "Timepoint", 
    y = "Percent of households using specified fuel type (%)"
  ) + 
  theme_bw() + 
  facet_wrap(. ~ study_arm_overall)

fig_fuel_used_30_days


ggsave(
  here::here("6_figures", "fig_fuel_used_30_days.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)

fig_fuel_used_30_days <-
  fuel_used_30_days_summary %>%
  mutate(
    study_arm_overall = 
      ordered(
        study_arm_overall,
        levels = c("comparison", "intervention"),
        labels = c("Comparison group", "Intervention group")
      )
  ) %>%
  mutate(
    fuel_group = 
      ordered(
        fuel,
        levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
        labels = c("Scraps, gathered", "Wood, harvested", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
      )
  ) %>%
  ggplot() + 
  geom_point(aes(x = timepoint, y = prop, color = fuel, group = fuel)) + 
  geom_line(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
  geom_errorbar(aes(x = timepoint, ymin = lower, ymax = upper, color = fuel), width = 0.1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "Types of Cooking Fuel Used in the Past 30 Days, By Study Arm and Timepoint",
    color = "Type of fuel used in past 30 days",
    x = "Timepoint", 
    y = "Percent of households using specified fuel type (%)"
  ) + 
  theme_bw() + 
  facet_wrap(. ~ study_arm_overall)

################################################################################
# Fuel used in past 30 days - intervention group at baseline
################################################################################


fuel_used_30_days_summary_intervention_baseline <-
  fuel_used_30_days_summary %>%
  filter(study_arm_overall == "intervention") %>%
  filter(timepoint == "baseline")

# study_arm_overall timepoint fuel             prop     sd method     x     n    mean     lower   upper
# <ord>             <ord>     <chr>           <dbl>  <dbl> <chr>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
# 1 intervention      baseline  buy_crh       0.285   0.452  wilson   141   494 0.285    2.47e- 1 0.327  
# 2 intervention      baseline  buy_lpg       0       0      wilson     0   494 0       -4.30e-19 0.00772
# 3 intervention      baseline  buy_wood      0.599   0.491  wilson   296   494 0.599    5.55e- 1 0.641  
# 4 intervention      baseline  collect_wood  0.198   0.399  wilson    98   494 0.198    1.66e- 1 0.236  
# 5 intervention      baseline  gather_scraps 0.532   0.499  wilson   263   494 0.532    4.88e- 1 0.576  
# 6 intervention      baseline  other         0.0972  0.296  wilson    48   494 0.0972   7.41e- 2 0.126  
# 7 intervention      baseline  receive_crh   0.101   0.302  wilson    50   494 0.101    7.76e- 2 0.131  
# 8 intervention      baseline  receive_lpg   0       0      wilson     0   494 0       -4.30e-19 0.00772
# 9 intervention      baseline  receive_wood  0.00405 0.0636 wilson     2   494 0.00405  1.11e- 3 0.0146

write.csv(fuel_used_30_days_summary_intervention_baseline, file_out_3)

################################################################################
# Fuel used in past 30 days - table of lpg vs wood 
################################################################################
fuel_used_30_days_wood_lpg_other <-
  survey_data %>%
  select(study_arm_overall, timepoint, fcn_id, contains("fuel_30")) %>%
  group_by(study_arm_overall, timepoint) %>%
  mutate(
    wood = 
      case_when( 
        fuel_30_buy_wood == 1 ~ 1, 
        fuel_30_collect_wood == 1 ~ 1, 
        fuel_30_receive_wood == 1 ~ 1,
        TRUE ~ 0
      ),
    biomass = 
      case_when(
        fuel_30_gather_scraps == 1 ~ 1, 
        fuel_30_buy_wood == 1 ~ 1, 
        fuel_30_collect_wood == 1 ~ 1, 
        fuel_30_receive_wood == 1 ~ 1, 
        fuel_30_buy_crh == 1 ~ 1,
        fuel_30_receive_crh == 1 ~ 1, 
        TRUE ~ 0
      ), 
    lpg = 
      case_when(
        fuel_30_buy_lpg == 1 ~ 1, 
        fuel_30_receive_lpg == 1 ~ 1, 
        TRUE ~ 0
      ),
    other = 
      case_when( 
        fuel_30_other == 1 ~ 1,
        TRUE ~ 0
      ),
  ) %>%
  mutate(n = 1)

fuel_used_30_days_wood_lpg <- 
  fuel_used_30_days_wood_lpg_other %>% 
  pivot_longer(cols = c(wood, lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(study_arm_overall, timepoint, fuel_simple) %>% 
  summarise(
    prop = mean(value),
    sd = sd(value),
    binom.confint(x = sum(value, na.rm = TRUE), n = sum(n), methods = "wilson")
  ) %>%
  ungroup() 

fuel_used_30_days_biomass_lpg <- 
  fuel_used_30_days_wood_lpg_other %>% 
  pivot_longer(cols = c(wood, lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(study_arm_overall, timepoint, fuel_simple) %>% 
  summarise(
    prop = mean(value),
    sd = sd(value),
    binom.confint(x = sum(value, na.rm = TRUE), n = sum(n), methods = "wilson")
  ) %>%
  ungroup() 


write.csv(fuel_used_30_days_wood_lpg, file_out_4)
write.csv(fuel_used_30_days_biomass_lpg, file_out_5)

################################################################################
##### Model intervention impact #####
###############################################################################
# Was there a change in the percent using LPG from baseline to endline?
# For the model, we need to change timepoint and study_arm (and any other ordered variables) into factors, becuase when they are ordered, they are evaluated as integers rather than factors. Not sure why
survey_data_mod <-
  survey_data %>%
  filter(timepoint %in% c("baseline", "endline")) %>%
  mutate(
    study_arm_overall = as.factor(as.character(study_arm_overall)),
    timepoint = as.factor(as.character(timepoint)),
    fcn_id = as.factor(fcn_id)
  )

fuel_used_30_days_wood_lpg_other_mod <-
  fuel_used_30_days_wood_lpg_other %>%
  filter(timepoint %in% c("baseline", "endline")) %>%
  mutate(
    study_arm_overall = as.factor(as.character(study_arm_overall)),
    timepoint = as.factor(as.character(timepoint)),
    fcn_id = as.factor(fcn_id)
  )

# logistic regression because the outcome is binary 0 and 1

# HOWEVER, fuel_30_receive_lpg = 0 for all intervention hh at baseline and fuel_30_collect_wood = 0 for all control hh at baseline (or close to that)
# so there is "complete separation"
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#penalizationhandling-complete-separation
# The best generalized model (that will handle fixed effects) is Bayesian GLMM
# There are multiple Bayesain GLMM packages, try bglmer

# This isn't really necessary as we can just subtract the prevalences to get the prevalence difference, but we would need it for calculating confidence intervals

# https://stats.stackexchange.com/questions/366784/how-to-get-around-the-glmer-warning-downdated-vtv-is-not-positive-definite
library(blme)
# https://cran.r-project.org/web/packages/blme/blme.pdf
test_mod <-
  bglmer(
    fuel_30_receive_lpg ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), 
    family = binomial,
    data = fuel_used_30_days_wood_lpg_other_mod, # survey_data_mod
    cov.prior = NULL,
    fixef.prior = normal(cov = diag(9), 2)
    # control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )

# Error in diag(cov, p) : 
#   'nrow' or 'ncol' cannot be specified when 'x' is a matrix

pairs(emmeans(test_mod, ~ procedure))


summary(test_mod)
coef(test_mod)

exp(2.926e+01)

par(mfrow = c(2, 2))
plot(test_mod)


# Make a function that will apply this across all the fuel types at different timepoints
# define first model - mixed logistic regression
glmer_binom <- function(outcome, data, timepoint1, timepoint2) {
  glmer(
    formula(paste0(outcome, "~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1|fcn_id)")),
    family = binomial(link = "logit"),
    data = data %>% 
      filter(timepoint %in% c(timepoint1, timepoint2)) %>% 
      mutate(study_arm_overall = as.factor(as.character(study_arm_overall)), timepoint = as.factor(as.character(timepoint))),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

glmer_poisson <- function(outcome, data, timepoint1, timepoint2) {
  glmer(
    formula(paste0(outcome, "~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1|fcn_id)")),
    family = poisson(link = "log"),
    # For the model, we need to change timepoint and study_arm (and any other ordered variables) into factors, becuase when they are ordered, they are evaluated as integers rather than factors. Not sure why
    data = data %>% 
      filter(timepoint %in% c(timepoint1, timepoint2)) %>% 
      mutate(study_arm_overall = as.factor(as.character(study_arm_overall)), timepoint = as.factor(as.character(timepoint))),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

# function to clean model output
mod_output <- function(mod_fxn, outcome, data, timepoint1, timepoint2) {
  
  mod <- mod_fxn(outcome, data, timepoint1, timepoint2)
  
  mod_out <- 
    tidy(mod) %>%
    filter(term == "study_arm_overallintervention:timepointendline") %>%
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
outcome_list <- 
  c(
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
# Generate predicted probabilities
p = predict(mod1_test, survey_data_mod, type = "response")
# if create a contingency table where p >0.5 sets the prediction to TRUE, 
con_table = table(p > 0.5, survey_data_mod$fuel_30_receive_lpg)
# The contingency table shows true positives, true negatives, false positives and false negatives. False positives and false negatives should be small
con_table

output_mod_fuel_30_days <-
  outcome_list %>%
  map_df(~mod_output(glmer_poisson, outcome = ., data = survey_data, timepoint1 = "baseline", timepoint2 = "endline"))

write_csv(output_mod_fuel_30_days, file_out_6)

