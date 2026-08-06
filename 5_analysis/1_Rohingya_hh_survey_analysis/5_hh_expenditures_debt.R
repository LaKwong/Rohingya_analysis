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

file_out_1 <- here::here("7_tables/monthly_expenditures_summary.csv")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################

# Expenses

survey_data %>%
  group_by(study_arm) %>%
  summarise_at(
    vars(spent_monthly_vars),
    list(mean = mean, median = median),
    na.rm = TRUE
  ) %>%
  ungroup()

fcn_id_purchased_fuel_30_endline <-
  survey_data %>%
  filter(
    (fuel_30_buy_wood == 1 | fuel_30_buy_lpg == 1 | fuel_30_buy_crh == 1) & timepoint == "endline"
  ) %>%
  pull(fcn_id) %>% unique() # had to use pull to get a vector instead of select, which kept a tibble


fcn_id_lpg_sufficiency_endline <-
  survey_data %>%
  filter(
    (lpg_sufficiency == "LPG sufficent") & timepoint == "endline"
  ) %>%
  pull(fcn_id) %>% unique() # had to use pull to get a vector instead of select, which kept a tibble

monthly_expenditures_base <- 
  survey_data %>%
  select(timepoint, study_arm_overall, fcn_id, fuel_30_buy_wood, fuel_30_buy_lpg, fuel_30_buy_crh, spent_total_month, all_of(spent_monthly_vars)) %>%
  select(-c(fuel_30_buy_wood, fuel_30_buy_lpg, fuel_30_buy_crh)) %>% #
  mutate(
    # fuel_30_purchased_fuel = ifelse(fuel_30_buy_wood == 1 | fuel_30_buy_lpg == 1 | fuel_30_buy_crh == 1, 1, 0),
    # labels all the hh that purchased fuel at endline, regardless of whether the purchased at baseline or not (arm is still captured in study_group_overall color)
    fuel_30_purchased_fuel_endline = ifelse(fcn_id %in% fcn_id_purchased_fuel_30_endline, 1, 0),
    lpg_sufficiency_endline = ifelse(fcn_id %in% fcn_id_lpg_sufficiency_endline, "lpg_sufficient_endline", "lpg_insufficient_endline")
  ) %>%
  gather(-timepoint, -study_arm_overall, -fcn_id, -fuel_30_purchased_fuel_endline, -lpg_sufficiency_endline, key = "category", value = "expenditure") %>% # fuel_30_purchased_fuel
  # filter(category %notin% 
  # 			 	c(
  # 			 		"spent_total_month_with_6mo_monthly",
  # 			 		"buy_lpg_cost", 
  # 			 		"spent_tobacco_pan", 
  # 			 		"spent_clothing_month", 
  # 			 		"spent_medical_month", 
  # 			 		"spent_debt_month",
  # 			 		
  # 			 		"spent_transport", "spent_shelter_month", 
  # 			 		"spent_celebrations_month",   "spent_education_month", 
# 			 		"spent_phone",  "spent_hygiene",
# 			 		"spent_hh_items", "spent_other", "spent_other_month"
# 			 	)
# ) %>%
mutate(
  category = as.factor(category)
) %>%
  ungroup()

monthly_expenditures_summary <-
  monthly_expenditures_base %>%
  group_by(timepoint, study_arm_overall, lpg_sufficiency_endline, category) %>% #  lpg_sufficiency_endline
  summarise(
    n = n(),
    income_mean = mean(expenditure, na.rm = TRUE),
    sd = sd(expenditure, na.rm = TRUE),
    income = income_mean + qnorm(0.025) * sd,
    upper = income_mean - qnorm(0.025) * sd
  )

write.csv(monthly_expenditures_summary, file_out_1)



##############################################################################
### Make figure 

supp.labs <- c("Firewood spending (per month)", "Food spending (per month)")
names(supp.labs) <- c("buy_wood_cost", "spent_food")


fig_monthly_expenditures <-	
  monthly_expenditures_base %>%
  mutate(
    lpg_sufficiency_endline = ordered(lpg_sufficiency_endline, levels = c("lpg_sufficient_endline", "lpg_insufficient_endline"), labels = c("LPG sufficient \nat endline", "LPG insufficient \nat endline")),
    fuel_30_purchased_fuel_endline = 
      # factor(fuel_30_purchased_fuel, levels = c(0, 1), labels = c("Did NOT \n buy fuel \nin past 30 days", "Bought fuel \nin past 30 days ")),
      factor(fuel_30_purchased_fuel_endline, levels = c(0, 1), labels = c("Did NOT \nbuy fuel \nin 30 days before endline", "Bought fuel \nin 30 days before endline")),
    category_label = 
      ordered(
        category,
        levels = c(
          "spent_total_month_with_6mo_monthly",
          "spent_food",
          "buy_wood_cost",
          "buy_lpg_cost",
          "spent_tobacco_pan",
          "spent_clothing_month",
          "spent_debt_month", # There is one value of 50,000 (intervention, midline) that is pulling up the average a lot
          "spent_medical_month",
          "spent_transport", "spent_shelter_month",
          "spent_celebrations_month",   "spent_education_month",
          "spent_phone",  "spent_hygiene",
          "spent_hh_items", "spent_other_month"
        ),
        # labels = 
        # 	c(
        # 		"total", # all expenditures
        # 		"  wood  ", 
        # 		"  food  "#,  
        # 		"other", #"   lpg   ",
        # 		"other", #"pan/\ntobacco",
        # 		"other", #"clothing",
        # 		"other", #"medical",
        # 		"other", # "paying \ndebt",
        # 
        # 		"other", "other",
        # 		"other", "other",
        # 		"other", "other",
        # 		"other", "other"
        # 	)
        labels =
          c(
            "total", # all expenditures
            "  food  ",
            "  wood  ",
            "   lpg   ",
            "pan/\ntobacco",
            "clothing",
            "paying \ndebt",
            "other", # "medical",
            "other", "other", # "transport", "shelter",
            "other", "other",
            "other", "other",
            "other", "other"
          )
      )
  ) %>%
  ungroup() %>%
  group_by(timepoint, study_arm_overall, category_label) %>% #  lpg_sufficiency_endline
  ggplot(aes(x = timepoint, y = expenditure / BDT_USD_exchange_rate_endline, color = study_arm_overall, shape = timepoint, group = study_arm_overall)) +
  # geom_boxplot() +
  # geom_point(size = 3) + # position = position_dodge(width = 0.2),
  stat_summary(fun = "mean", geom = "line", size = 1) +
  stat_summary(fun = "mean", geom = "point", size = 2) +
  stat_summary(
    fun.data = "mean_cl_boot",
    geom = "linerange" #,
    # position = position_dodge(width = 0.2
  ) +
  
  # ggpubr::stat_compare_means(
  # 	# aes(group = timepoint),
  # 	label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  # 	# label = "p.format",
  # 	method = "wilcox.test",
  # 	paired = FALSE, # not paired because this is endline only
  # 	# label.y = 18,
  # 	hide.ns = TRUE#,
  # 	# ref.group = ".all."
  # 	# show.legend = TRUE # doesn't show the legend of stas?
# ) +
scale_y_continuous(breaks = seq(0, 70, 2), minor_breaks = seq(0, 70, 1), labels = scales::label_dollar()) +
  scale_color_manual(
    name = "Study_arm",
    breaks = c("intervention", "comparison"),
    # labels = c("Pre-intervention", "Intervention", "Outdoor"),
    labels = c("Intervention", "Comparison"),
    values = c("#138b87",  "#430154"),
    # values = c("#7570b3",  "#d95f02", "#1b9e77")
  ) +
  # viridis::scale_fill_viridis(
  # 	discrete = TRUE,
  # 	end = 2/3
  # ) +
  
  theme_bw() +
  theme(
    # axis.text.x = element_text(angle = 0, hjust = 1)
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  guides(
    color = guide_legend(title = "Study arm"),
    shape = guide_legend(title = "Timepoint")
  ) +
  labs(
    title = "Monthly household expenditure",
    x = "Expenditure category",
    y = "Monthly expenditure (USD)"
  ) + 
  # facet_wrap(~ category_label, ncol = 3) #, scales = "free_y"
  facet_grid(category_label ~ lpg_sufficiency_endline, scales = "free_y") 
# facet_grid( ~ category, labeller = labeller(category = supp.labs)) # + # these only work with facet_wrap: , strip.position = "bottom", scales = "free_y"
# facet_grid(lpg_sufficiency_endline ~ category) + # fuel_30_purchased_fuel_endline
# coord_cartesian(ylim = c(0, 40))


fig_monthly_expenditures

ggsave(
  here::here("6_figures", "fig_monthly_expenditures_few_categories.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)

##### DIF in Dif for monthly expenditures 

dind_fxn(var_interest = "spent_food", model_data)
dind_fxn(var_interest = "spent_food", model_data)
dind_fxn(var_interest = "buy_wood_cost", model_data)

survey_data$spent_food


fig_monthly_expenditures







##############################################################################
## Cost of firewood
##############################################################################

# buy_wood_cost 
# "In the past 30 days, how much money (taka) did you spend on firewood?"

##############################################################################
## Cost to enter the forest for firewood
##############################################################################

# forest_wood_fee




##############################################################################
# Credit
##############################################################################

# Percent that have access to credit
survey_data %>%
  count(credit_access == 1)

# Percent of hh that have access to credit thorugh specific mechanisms

# Refactor so that dk = 0

survey_data %>%
  summarise_at(
    vars(
      credit_relatives, credit_charities, credit_village_head, credit_lender, credit_bank, credit_cooperative
    ),
    list(mean),
    na.rm = TRUE
  )

# Use credit to borrow money for food in past 3 months
survey_data %>%
  count(credit_borrow_money_food == 1)

# Percent that have used mobile money
survey_data %>%
  count(mobile_money == 1)



################################################################################
### Income and LPG sufficiency
###############################################################################
# HH who got LPG later couldn't take advantage of not walking to the forest and working instead because the jobs were taken already
# 
# Where did the firewood money go? Saving, which we didn't ask about?

# Percentage of hh making any money from selling wood
survey_data %>%
  group_by(study_arm) %>%
  count(income_selling_wood != 0)
# Only 1 hh made money selling wood and this was pre-intervention

# Of those selling wood, the income from selling wood as a percent of total income
# Moot since this is only 1 hh


## Was income dep on LPG sufficiency? (Didn't have to go to the forest?)

fcn_id_lpg_sufficiency_endline <-
  survey_data %>%
  filter(
    (lpg_sufficiency == "LPG sufficent") & timepoint == "endline"
  ) %>%
  pull(fcn_id) %>% unique() # had to use pull to get a vector instead of select, which kept a tibble


monthly_income_base <- 
  survey_data %>%
  select(timepoint, study_arm_overall, fcn_id, org, starts_with("income")) %>%
  select(-income_yes, -starts_with("income_abroad_"), -income_home_garden) %>% #income_abroad_location, income_abroad_relative
  gather(-timepoint, -study_arm_overall, -fcn_id, -org, key = "income_source", value = "income") %>%
  mutate(income = as.numeric(income)) %>%
  mutate(
    lpg_sufficiency_endline = ifelse(fcn_id %in% fcn_id_lpg_sufficiency_endline, "LPG sufficient \nat endline", "LPG insufficient \nat endline"),
    lpg_sufficiency_endline = ordered(lpg_sufficiency_endline, levels = c("LPG sufficient \nat endline", "LPG insufficient \nat endline"))
  ) %>%
  mutate(
    income_source = 
      ordered(
        income_source,
        levels = c(
          "income",
          "income_wage_labor", "income_cash_ngo", 
          "income_skill_labor", "income_own_business",
          "income_abroad",
          "income_handicrafts_tailoring", "income_farming", 
          # "income_home_garden_amt",
          "income_humanitarian_asst", "income_selling_wood"  
        ),
        labels = 
          c(
            "total \nincome",
            "wage \nlabor", "NGO \nwork", 
            "skilled \nlabor", "own \nbusiness", 
            "remittences", 
            "tailoring \nhandicrafts", "farming",
            # "home garden",
            "selling \naid", "selling \nwood"
            
          )
      )
  ) %>%
  filter(income_source != "home_garden") %>%
  group_by(timepoint, study_arm_overall, income_source) %>% # lpg_sufficiency_endline, 
  summarise(
    income_mean = mean(income, na.rm = TRUE),
    sd = sd(income, na.rm = TRUE), 
    pct_this_income = sum(if_else(income > 0, 1,0), na.rm = TRUE)/n() ) %>% 
  view()
# mutate(income_LCI = income_mean - sd, income_UCI = income_mean + sd) %>%


monthly_income_base

monthly_income_base %>% 
  group_by(timepoint, study_arm_overall) %>% 
  filter(income_source == "total \nincome")


fig_monthly_income <-	
  ggplot(aes(x = timepoint, y = income_mean / BDT_USD_exchange_rate_endline, color = study_arm_overall, shape = timepoint, group = study_arm_overall)) +
  # geom_boxplot(aes(fill = lpg_sufficiency), position = "dodge") +
  # Mean_sdl adds the mean and sd to the plot
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2)) +
  # geom_errorbar(aes(ymin = income_LCI, ymax = income_UCI), position = position_dodge(width = 0.2)) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
  
  ggpubr::stat_compare_means(
    # aes(group = timepoint),
    label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
    # label = "p.format",
    method = "wilcox.test",
    paired = FALSE, # not paired because this is endline only
    # label.y = 18,
    hide.ns = TRUE#,
    # ref.group = ".all."
    # show.legend = TRUE # doesn't show the legend of stas?
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10), labels = scales::label_dollar()) +
  viridis::scale_fill_viridis(
    discrete = TRUE,
    end = 2/3
  ) +
  theme_classic() +
  theme(
    # axis.text.x = element_text(angle = 0, hjust = 1)
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  labs(
    title = "Monthly income",
    x = "Income source",
    y = "Monthly income (BDT)"
  ) +
  facet_grid(~ income_source, scales = "free_y")
# facet_grid(lpg_sufficiency_endline ~ income_source) + # 
# coord_cartesian(ylim = c(0, 5000))

fig_monthly_income

ggsave(
  here::here("6_figures", "fig_monthly_income.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)

# Note that the IOM graph contains the intervention hh, which were poorer at baseline while the UNHCR hh are only in the on-going intervention arm












######################################################################
#### Extra code #########
#######################################################################


# ## Extra code for income - differences
# fig_monthly_income_1 <-
# 	survey_data %>%
# 	filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>%
# 	select(study_arm, fcn_id, org, lpg_sufficiency, starts_with("income")) %>%
# 	select(-income_yes, starts_with("income_abroad_"), -income_home_garden) %>% #income_abroad_location, income_abroad_relative
# 	gather(-study_arm, -fcn_id, -org, -lpg_sufficiency, key = "income_source", value = "income") %>%
# 	mutate(income = as.numeric(income)) %>%
# 	mutate(
# 		income_source =
# 			ordered(
# 				income_source,
# 				levels = c(
# 					"income",
# 					"income_wage_labor", "income_cash_ngo",
# 					"income_skill_labor", "income_own_business",
# 					"income_abroad",
# 					"income_handicrafts_tailoring", "income_farming",
# 					# "income_home_garden_amt",
# 					"income_humanitarian_asst", "income_selling_wood"
# 				),
# 				labels =
# 					c(
# 						"total income",
# 						"wage labor", "NGO",
# 						"skilled labor", "own business",
# 						"remittences",
# 						"tailoring \nhandicrafts", "farming",
# 						# "home garden",
# 						"selling aid", "selling wood"
# 						
# 					)
# 			)
# 	) %>%
# 	filter(income_source != "home_garden") %>%
# 	ggplot(aes(x = income_source, y = income)) +
# 	geom_boxplot(aes(fill = lpg_sufficiency), position = "dodge") +
# 	# Mean_sdl adds the mean and sd to the plot
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = lpg_sufficiency), position = position_dodge(0.8), color="red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = lpg_sufficiency),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		# label = "p.format",
# 		method = "wilcox.test",
# 		paired = FALSE, # not paired because this is endline only
# 		# label.y = 18,
# 		hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		axis.text.x = element_text(angle = 0, hjust = 1)
# 	) +
# 	labs(
# 		title = "Monthly income at endline (Sep-Nov 2020)",
# 		x = "Income source",
# 		y = "Monthly income (BDT)"
# 	) +
# 	facet_wrap(~ org)
# 
# fig_monthly_income_diffs <- 
# 	survey_data_base_wide %>%
# 	select(fcn_id, org.baseline, study_arm, paste0(c(all_of(income_monthly_vars)), ".baseline"), paste0(c(all_of(income_monthly_vars)), ".endline")) %>%
# 	# select(-c(income_yes.baseline, income_yes.endline)) %>%
# 	rowwise() %>%
# 	mutate(
# 		income_total_diff = income.endline - income.baseline,
# 		income_cash_ngo_diff = income_cash_ngo.endline - income_cash_ngo.baseline,
# 		income_wage_labor_diff = income_wage_labor.endline - income_wage_labor.baseline,
# 		income_skill_labor_diff = income_skill_labor.endline - income_skill_labor.baseline,
# 		income_own_business_diff = income_own_business.endline - income_own_business.baseline,
# 		income_abroad_diff = income_abroad.endline - income_abroad.baseline,
# 		income_handicrafts_tailoring_diff = income_handicrafts_tailoring.endline - income_handicrafts_tailoring.baseline,
# 		income_farming_diff = income_farming.endline - income_farming.baseline,
# 		income_humanitarian_asst_diff = income_humanitarian_asst.endline - income_humanitarian_asst.baseline,
# 		income_selling_wood_diff = income_selling_wood.endline - income_selling_wood.baseline,
# 		# income_home_garden_diff = income_home_garden_amt.endline - income_home_garden_amt.baseline
# 	) %>%
# 	select(fcn_id, org.baseline, study_arm, contains("_diff")) %>%
# 	gather(-fcn_id, -org.baseline, -study_arm, key = category, value = "income_diff") %>%
# 	
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"income_total_diff",
# 					"income_wage_labor_diff", "income_cash_ngo_diff", 
# 					"income_skill_labor_diff", "income_own_business_diff", 
# 					"income_abroad_diff",	
# 					"income_handicrafts_tailoring_diff", "income_farming_diff", 
# 					# "income_home_garden_amt_diff"
# 					"income_humanitarian_asst_diff", "income_selling_wood_diff"  
# 					
# 				),
# 				labels = 
# 					c(
# 						"total income",
# 						"wage labor", "NGO", 
# 						"skilled labor", "own business", 
# 						"remittences", 
# 						"tailoring \nhandicrafts", "farming",
# 						# "home garden"
# 						"selling aid", "selling wood"
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = income_diff)) +
# 	geom_boxplot(aes(fill = study_arm), position = "dodge") +
# 	# geom_violin(aes(fill = study_arm)) +
# 	# Mean_sdl adds the mean and sd to the plot
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = study_arm),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		# label = "p.format",
# 		method = "wilcox.test",
# 		paired = FALSE, # Not paired because this is the difference
# 		
# 		label.y = 18000,
# 		hide.ns = FALSE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?,
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 0, hjust = 1),
# 		# axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change  in monthly income",
# 		x = "Income category",
# 		y = "Change in monthly income (BDT)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-20000, 20000)) # + # does not show one intervention hh data point for wage labor of -50,000 
# # facet_wrap(~ org.baseline)
# 
#
#
#
####################################################################################
#################################################################################### 
# ## Extra code monthly income - change in percent of income from each category
# 
# monthly_income_diffs_pc <- 
# 	survey_data_base_wide %>%
# 	select(fcn_id, org.baseline, study_arm, paste0(c(all_of(income_monthly_vars)), ".baseline"), paste0(c(all_of(income_monthly_vars)), ".endline")) %>%
# 	# select(-c(income_yes.baseline, income_yes.endline)) %>%
# 	mutate_at(vars(contains("income")), funs(as.numeric(as.character(.)))) %>% # Need to turn factors into characters before they are made into text
# 	# str() 
# 	mutate_at(vars(contains("income")), funs(replace_na), replace = 0) %>%
# 	rowwise() %>%
# 	mutate(
# 		income_total_diff_pc = ifelse(income.baseline == 0 | income.endline == 0, 0, income.endline  / income.baseline),
# 		income_cash_ngo_diff_pc = 
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_cash_ngo.endline / income.endline) - (income_cash_ngo.baseline / income.baseline)),
# 		income_wage_labor_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_wage_labor.endline / income.endline) - (income_wage_labor.baseline / income.baseline)),
# 		income_skill_labor_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_skill_labor.endline / income.endline) - (income_skill_labor.baseline / income.baseline)),
# 		income_own_business_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_own_business.endline / income.endline) - (income_own_business.baseline / income.baseline)),
# 		income_abroad_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_abroad.endline / income.endline) - (income_abroad.baseline / income.baseline)),
# 		income_handicrafts_tailoring_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_handicrafts_tailoring.endline / income.endline) - (income_handicrafts_tailoring.baseline / income.baseline)),
# 		income_farming_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_farming.endline / income.endline) - (income_farming.baseline / income.baseline)),
# 		income_humanitarian_asst_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_humanitarian_asst.endline / income.endline) - (income_humanitarian_asst.baseline / income.baseline)),
# 		income_selling_wood_diff_pc =  
# 			ifelse(income.baseline == 0 | income.endline == 0, 0, (income_selling_wood.endline / income.endline) - (income_selling_wood.baseline / income.baseline))
# 		# income_home_garden_diff = (income_home_garden_amt.endline/income.endline) - (income_home_garden_amt.baseline / income.baseline)
# 	) %>%
# 	# select(fcn_id, org.baseline, study_arm, contains("income")) %>%
# 	# select(income.baseline, income.endline, income_total_diff_pc, contains("income_cash"), contains("income_wage"), contains("income_skill"), contains("income_own"), contains("income_abroad"), contains("income_handicrafts"), contains("income_farming"), contains("income_humanitarian"), contains("income_selling")) %>% 
# 	# View()
# 	
# 	select(fcn_id, org.baseline, study_arm, contains("_diff_pc")) %>%
# 	gather(-fcn_id, -org.baseline, -study_arm, key = category, value = "income_diff_pc")
# 
# # monthly_income_diffs_pc %>% View()
# 
# fig_monthly_income_diffs_pc <-
# 	monthly_income_diffs_pc %>%
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"income_total_diff_pc",
# 					"income_wage_labor_diff_pc", "income_cash_ngo_diff_pc", 
# 					"income_skill_labor_diff_pc", "income_own_business_diff_pc", 
# 					"income_abroad_diff_pc",	
# 					"income_handicrafts_tailoring_diff_pc", "income_farming_diff_pc", 
# 					# "income_home_garden_amt_diff"
# 					"income_humanitarian_asst_diff_pc", "income_selling_wood_diff_pc"  
# 					
# 				),
# 				labels = 
# 					c(
# 						"total income",
# 						"wage labor", "NGO", 
# 						"skilled labor", "own business", 
# 						"remittences", 
# 						"tailoring \nhandicrafts", "farming",
# 						# "home garden"
# 						"selling aid", "selling wood"
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = income_diff_pc, fill = study_arm)) +
# 	# ggplot(aes(x = category, y = income_diff, fill = org.baseline)) + # There is not sig diff for any categories between IOM and UNHCR
# 	geom_boxplot(position = "dodge") +
# 	# Mean_sdl adds the mean and sd to the plot
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = study_arm),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # diff so not paired
# 		
# 		label.y = 6 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 0, hjust = 1),
# 		# axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in source of monthly income as percent of total income",
# 		x = "Income category",
# 		y = "Percent of monthly income (%)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ org.baseline) +
# 	# facet_wrap(~ study_arm, nrow = 1) # + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-1, 6)) # does not show one intervention hh data point for wage labor of -50,000
# 
# 
# 
####################################################################################
####################################################################################
# ## Extra code for monthly expenditures
# fig_monthly_expenditures <-
# 	survey_data %>%
# 	# select(study_arm, fcn_id, lpg_sufficiency, all_of(spent_monthly_vars)) %>%
# 	select(study_arm, fcn_id, fuel_30_buy_wood, fuel_30_buy_lpg, fuel_30_buy_crh, all_of(spent_monthly_vars)) %>%
# 	
# 	mutate(
# 		fuel_30_purchased_fuel = ifelse(fuel_30_buy_wood == 1 | fuel_30_buy_lpg == 1 | fuel_30_buy_crh == 1, 1, 0)
# 	) %>%
# 	select(-c(fuel_30_buy_wood, fuel_30_buy_lpg, fuel_30_buy_crh)) %>%
# 	gather(-study_arm, -fcn_id, -fuel_30_purchased_fuel, key = "category", value = "expenditure") %>%
# 	mutate(
# 		category = as.factor(category),
# 		category =
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month_with_6mo_monthly",
# 					"spent_food",  "spent_tobacco_pan",
# 					"spent_clothing_month", "spent_medical_month",
# 					"spent_transport", "spent_shelter_month",
# 					"spent_celebrations_month",   "spent_education_month",
# 					"spent_phone",  "spent_hygiene",
# 					"spent_hh_items", "spent_other",
# 					
# 					"buy_wood_cost", "buy_lpg_cost", "spent_debt_month"
# 				),
# 				labels =
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco",
# 						"clothing",  "medical",
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other",
# 						"other", "other",
# 						
# 						"  wood  ", "   lpg   ", " debt "
# 					)
# 			),
# 		fuel_30_buy_wood =
# 			factor(fuel_30_purchased_fuel, levels = c(0, 1), labels = c("Did NOT \n buy fuel \nin past 30 days", "Bought fuel \nin past 30 days "))
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure, fill = study_arm)) +
# 	geom_boxplot(position = "dodge") +
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = study_arm),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # diff so not paired
# 		
# 		label.y = 1500 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 45, hjust = 1)
# 		axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) +
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Monthly expenditures",
# 		x = "Expense category",
# 		y = "Monthly expenditure (BDT)"
# 	) +
# 	# facet_grid( ~ study_arm + fuel_30_purchased_fuel) + # fuel_30_buy_wood # , nrow = 1
# 	coord_cartesian(ylim = c(0, 16000)) # removes one data point for debt of 50,000
# 
# monthly_expenditures_diffs <- 
# 	survey_data_base_wide %>%
# 	select(study_arm, fcn_id, paste0(c("fuel_30_buy_wood", "fuel_30_buy_lpg", "fuel_30_buy_crh", all_of(spent_monthly_vars)), ".baseline"), paste0(c("fuel_30_buy_wood", "fuel_30_buy_lpg", "fuel_30_buy_crh", all_of(spent_monthly_vars)), ".endline")) %>%
# 	mutate(
# 		fuel_30_purchased_fuel.baseline = ifelse(fuel_30_buy_wood.baseline == 1 | fuel_30_buy_lpg.baseline == 1 | fuel_30_buy_crh.baseline == 1, 1, 0),
# 		fuel_30_purchased_fuel.endline = ifelse(fuel_30_buy_wood.endline == 1 | fuel_30_buy_lpg.endline == 1 | fuel_30_buy_crh.endline == 1, 1, 0)
# 	) %>%
# 	select(-c(fuel_30_buy_wood.baseline, fuel_30_buy_wood.endline, fuel_30_buy_lpg.baseline, fuel_30_buy_lpg.endline, fuel_30_buy_crh.baseline, fuel_30_buy_crh.endline)) %>%
# 	rowwise() %>%
# 	mutate(
# 		spent_total_month_diff = spent_total_month_with_6mo_monthly.endline - spent_total_month_with_6mo_monthly.baseline,
# 		spent_food_diff = spent_food.endline - spent_food.baseline,
# 		spent_wood_diff = buy_wood_cost.endline - buy_wood_cost.baseline,
# 		spent_tobacco_pan_diff = spent_tobacco_pan.endline - spent_tobacco_pan.baseline,
# 		spent_clothing_diff = spent_clothing_month.endline - spent_clothing_month.baseline,
# 		spent_debt_diff = spent_debt_month.endline - spent_debt_month.baseline,
# 		spent_medical_diff = spent_medical_month.endline - spent_medical_month.baseline,
# 		spent_transport_diff = spent_transport.endline - spent_transport.baseline,
# 		spent_hygiene_diff = spent_hygiene.endline - spent_hygiene.baseline,
# 		spent_phone_diff = spent_phone.endline - spent_phone.baseline,
# 		spent_shelter_diff = spent_shelter_month.endline - spent_shelter_month.baseline,
# 		spent_other_diff = spent_other.endline - spent_other.baseline,
# 		spent_celebrations_diff = spent_celebrations_month.endline - spent_celebrations_month.baseline,
# 		spent_education_diff = spent_education_month.endline - spent_education_month.baseline,
# 		spent_hh_items_diff = spent_hh_items.endline - spent_hh_items.baseline,
# 		
# 		spent_lpg_diff = buy_lpg_cost.endline - buy_lpg_cost.baseline,
# 		# "spent_food", "buy_wood_cost", "spent_tobacco_pan", 
# 		# "spent_clothing_month", "spent_debt_month", "spent_medical_month", 
# 		# "spent_transport", "spent_hygiene", "spent_phone", 
# 		# "spent_shelter_month", "spent_other_month", 
# 		# "spent_celebrations_month",   "spent_education_month", 
# 		# "spent_hh_items"
# 	) %>%
# 	select(fcn_id, study_arm, fuel_30_purchased_fuel.baseline, fuel_30_purchased_fuel.endline, contains("_diff")) %>%
# 	gather(-c(fcn_id, study_arm, fuel_30_purchased_fuel.baseline, fuel_30_purchased_fuel.endline), key = category, value = "expenditure_diff") %>%
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month_diff",
# 					"spent_food_diff",  "spent_tobacco_pan_diff", 
# 					"spent_clothing_diff", "spent_medical_diff", 
# 					"spent_transport_diff", "spent_shelter_diff", 
# 					"spent_celebrations_diff",   "spent_education_diff", 
# 					"spent_phone_diff",  "spent_hygiene_diff",
# 					"spent_hh_items_diff", "spent_other_diff", 
# 					
# 					"spent_wood_diff", "spent_lpg_diff", "spent_debt_diff"
# 				),
# 				labels = 
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco", 
# 						"clothing",  "medical", 
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other", 
# 						"other", "other", 
# 						
# 						"  wood  ", "   lpg   ", " debt "
# 					)
# 			)
# 	)
# 
# 
# 
# fig_monthly_expenditures_diffs <-
# 	monthly_expenditures_diffs %>%
# 	ggplot(aes(x = category, y = expenditure_diff, fill = study_arm)) +
# 	geom_boxplot(position = "dodge") +
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = study_arm),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # diff so not paired
# 		
# 		label.y = 10000 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 45, hjust = 1),
# 		axis.text.x = element_text(angle = 0), #
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in monthly expenditures",
# 		x = "Expense category",
# 		y = "Change in monthly expenditure (BDT)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-10000, 12000)) # removes one pre-intervention hh data point for debt of 50,000 and two data points (1 pre-intervention hh and one intervention hh) for food that were  approx -25000 BDT
# 
# 
####################################################################################
####################################################################################
# ## Extra code for monthly expenditures - differences
# 
# monthly_expenditures_diffs_fuel_30_purchased_comparisons = 
# 	list(
# 		c("did not purchase fuel at baseline and endline", "purchased fuel at baseline and endline"),
# 		c("did not purchase fuel at baseline and endline", "purchased fuel at endline but not baseline"),
# 		c("did not purchase fuel at baseline and endline", "purchased fuel at baseline but not endline"),
# 		c("purchased fuel at baseline and endline", "purchased fuel at endline but not baseline"),
# 		c("purchased fuel at baseline and endline", "purchased fuel at baseline but not endline")
# 	)
# 
# fig_monthly_expenditures_diffs_fuel_30_purchased <-
# 	monthly_expenditures_diffs %>%
# 	# Compare these 4 groups: 
# 	# 1. fuel_30_purchased_fuel.baseline == 0, fuel_30_purchased_fuel.endline == 0
# 	# 2. fuel_30_purchased_fuel.baseline == 1, fuel_30_purchased_fuel.endline == 0
# 	# 3. fuel_30_purchased_fuel.baseline == 0, fuel_30_purchased_fuel.endline == 1
# 	# 4. fuel_30_purchased_fuel.baseline == 1, fuel_30_purchased_fuel.endline == 1
# 	mutate(
# 		fuel_30_purchased_category =
# 			case_when(
# 				fuel_30_purchased_fuel.baseline == 0 & fuel_30_purchased_fuel.endline == 0 ~ "did not purchase fuel at baseline and endline", # 
# 				fuel_30_purchased_fuel.baseline == 1 & fuel_30_purchased_fuel.endline == 0 ~ "purchased fuel at baseline but not endline", # expect they will show a greater diff than  0 0 or 1 1 
# 				fuel_30_purchased_fuel.baseline == 0 & fuel_30_purchased_fuel.endline == 1 ~ "purchased fuel at endline but not baseline", # expect they will show less of a diff than 0 0 or 1 1
# 				fuel_30_purchased_fuel.baseline == 1 & fuel_30_purchased_fuel.endline == 1 ~ "purchased fuel at baseline and endline"
# 			),
# 		fuel_30_purchased_category = 
# 			ordered(
# 				fuel_30_purchased_category,
# 				levels = c(
# 					"did not purchase fuel at baseline and endline", 
# 					"purchased fuel at baseline but not endline", 
# 					"purchased fuel at endline but not baseline",
# 					"purchased fuel at baseline and endline"
# 				)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure_diff, fill = fuel_30_purchased_category)) +
# 	geom_boxplot(position = "dodge") +
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = fuel_30_purchased_category), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		comparison = monthly_expenditures_diffs_fuel_30_purchased_comparisons,
# 		# aes(group = fuel_30_purchased_category),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # diff so not paired
# 		
# 		label.y = 10000 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 45, hjust = 1),
# 		axis.text.x = element_text(angle = 0), #
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in monthly expenditures",
# 		x = "Expense category",
# 		y = "Change in monthly expenditure (BDT)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-10000, 12000)) # removes one pre-intervention hh data point for debt of 50,000 and two data points (1 pre-intervention hh and one intervention hh) for food that were  approx -25000 BDT
# 
# 
####################################################################################
####################################################################################
####### Extra code for monthly expenditures - change in percent expenditures on each category ###
# 
# monthly_expenditures_diffs_pc <- 
# 	survey_data_base_wide %>%
# 	select(fcn_id, study_arm, paste0(c(all_of(spent_monthly_vars)), ".baseline"), paste0(c(all_of(spent_monthly_vars)), ".endline")) %>%
# 	rowwise() %>%
# 	mutate(
# 		spent_total_month_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, spent_total_month_with_6mo_monthly.endline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_food_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_food.endline / spent_total_month_with_6mo_monthly.endline) - (spent_food.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_wood_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (buy_wood_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_wood_cost.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_tobacco_pan_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_tobacco_pan.endline / spent_total_month_with_6mo_monthly.endline) - (spent_tobacco_pan.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_clothing_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_clothing_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_clothing_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_debt_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_debt_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_debt_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_medical_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_medical_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_medical_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_transport_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_transport.endline / spent_total_month_with_6mo_monthly.endline) - (spent_transport.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_hygiene_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_hygiene.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hygiene.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_phone_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_phone.endline / spent_total_month_with_6mo_monthly.endline) - (spent_phone.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_shelter_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_shelter_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_shelter_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_other_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_other.endline / spent_total_month_with_6mo_monthly.endline) - (spent_other.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_celebrations_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_celebrations_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_celebrations_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_education_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_education_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_education_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_hh_items_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_hh_items.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hh_items.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		
# 		spent_lpg_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (buy_lpg_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_lpg_cost.baseline / spent_total_month_with_6mo_monthly.baseline))
# 	) %>%
# 	select(fcn_id, study_arm, contains("_diff")) %>%
# 	gather(-fcn_id, -study_arm, key = category, value = "expenditure_diff_pc")
# 
# fig_monthly_expenditure_diffs_pc <-
# 	monthly_expenditures_diffs_pc %>%
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month_diff_pc",
# 					"spent_food_diff_pc",  "spent_tobacco_pan_diff_pc", 
# 					"spent_clothing_diff_pc", "spent_medical_diff_pc", 
# 					"spent_transport_diff_pc", "spent_shelter_diff_pc", 
# 					"spent_celebrations_diff_pc",   "spent_education_diff_pc", 
# 					"spent_phone_diff_pc",  "spent_hygiene_diff_pc",
# 					"spent_hh_items_diff_pc", "spent_other_diff_pc", 
# 					
# 					"spent_wood_diff_pc", "spent_lpg_diff_pc", "spent_debt_diff_pc"
# 				),
# 				labels = 
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco", 
# 						"clothing",  "medical", 
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other", 
# 						"other", "other", 
# 						
# 						"  wood  ", "   lpg   ", " debt "
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure_diff_pc, fill = study_arm)) +
# 	geom_boxplot(position = "dodge") +	
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = study_arm),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # not paired because difference
# 		
# 		label.y = 4.5 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		axis.text.x = element_text(angle = 0, hjust = 1),
# 		# axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in category of monthly expenditure as percent of total expenditure",
# 		x = "Expenditure category",
# 		y = "Percent of monthly expenses (%)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) # + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-3, 5)) # does not show one intervention hh data point for wage labor of -50,000
# 
# 
# 
# 
# monthly_expenditures_diffs_pc_fuel_30_purchased <- 
# 	survey_data_base_wide %>%
# 	select(study_arm, fcn_id, paste0(c("fuel_30_buy_wood", "fuel_30_buy_lpg", "fuel_30_buy_crh", all_of(spent_monthly_vars)), ".baseline"), paste0(c("fuel_30_buy_wood", "fuel_30_buy_lpg", "fuel_30_buy_crh", all_of(spent_monthly_vars)), ".endline")) %>%
# 	mutate(
# 		fuel_30_purchased_fuel.baseline = ifelse(fuel_30_buy_wood.baseline == 1 | fuel_30_buy_lpg.baseline == 1 | fuel_30_buy_crh.baseline == 1, 1, 0),
# 		fuel_30_purchased_fuel.endline = ifelse(fuel_30_buy_wood.endline == 1 | fuel_30_buy_lpg.endline == 1 | fuel_30_buy_crh.endline == 1, 1, 0)
# 	) %>%
# 	mutate(
# 		fuel_30_purchased_category =
# 			case_when(
# 				fuel_30_purchased_fuel.baseline == 0 & fuel_30_purchased_fuel.endline == 0 ~ "did not purchase fuel at baseline and endline", # 
# 				fuel_30_purchased_fuel.baseline == 1 & fuel_30_purchased_fuel.endline == 0 ~ "purchased fuel at baseline but not endline", # expect they will show a greater diff than  0 0 or 1 1 
# 				fuel_30_purchased_fuel.baseline == 0 & fuel_30_purchased_fuel.endline == 1 ~ "purchased fuel at endline but not baseline", # expect they will show less of a diff than 0 0 or 1 1
# 				fuel_30_purchased_fuel.baseline == 1 & fuel_30_purchased_fuel.endline == 1 ~ "purchased fuel at baseline and endline"
# 			)
# 	) %>%
# 	filter(fuel_30_purchased_category %in% c("did not purchase fuel at baseline and endline", "purchased fuel at baseline but not endline")) %>%
# 	mutate(
# 		fuel_30_purchased_category = 
# 			ordered(
# 				fuel_30_purchased_category,
# 				levels = c(
# 					"purchased fuel at baseline but not endline", # The pure intervention group
# 					"did not purchase fuel at baseline and endline"#, # The pure on-going intervention group
# 					
# 					# "purchased fuel at endline but not baseline", # The comparison group that did not have access to LPG at endline
# 					# "purchased fuel at baseline and endline" # The intervention group that didn't actually get the intervention / did not have access to LPG at endline
# 				)
# 			)
# 	) %>%
# 	select(-c(fuel_30_purchased_fuel.baseline, fuel_30_purchased_fuel.endline, fuel_30_buy_wood.baseline, fuel_30_buy_wood.endline, fuel_30_buy_lpg.baseline, fuel_30_buy_lpg.endline, fuel_30_buy_crh.baseline, fuel_30_buy_crh.endline)) %>%
# 	rowwise() %>%
# 	mutate(
# 		spent_total_month_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, spent_total_month_with_6mo_monthly.endline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_food_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_food.endline / spent_total_month_with_6mo_monthly.endline) - (spent_food.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_wood_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (buy_wood_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_wood_cost.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_tobacco_pan_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_tobacco_pan.endline / spent_total_month_with_6mo_monthly.endline) - (spent_tobacco_pan.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_clothing_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_clothing_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_clothing_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_debt_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_debt_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_debt_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_medical_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_medical_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_medical_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_transport_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_transport.endline / spent_total_month_with_6mo_monthly.endline) - (spent_transport.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_hygiene_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_hygiene.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hygiene.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_phone_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_phone.endline / spent_total_month_with_6mo_monthly.endline) - (spent_phone.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_shelter_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_shelter_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_shelter_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_other_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_other.endline / spent_total_month_with_6mo_monthly.endline) - (spent_other.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_celebrations_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_celebrations_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_celebrations_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_education_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_education_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_education_month.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		spent_hh_items_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (spent_hh_items.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hh_items.baseline / spent_total_month_with_6mo_monthly.baseline)),
# 		
# 		spent_lpg_diff_pc = 
# 			ifelse(spent_total_month_with_6mo_monthly.baseline == 0 | spent_total_month_with_6mo_monthly.endline == 0, 0, (buy_lpg_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_lpg_cost.baseline / spent_total_month_with_6mo_monthly.baseline))
# 	) %>%
# 	select(fcn_id, study_arm, fuel_30_purchased_category, contains("_diff")) %>%
# 	gather(-fcn_id, -study_arm, -fuel_30_purchased_category, key = category, value = "expenditure_diff_pc")
# 
# fig_monthly_expenditures_diffs_pc_fuel_30_purchased <-
# 	monthly_expenditures_diffs_pc_fuel_30_purchased %>%
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month_diff_pc",
# 					"spent_food_diff_pc",  "spent_tobacco_pan_diff_pc", 
# 					"spent_clothing_diff_pc", "spent_medical_diff_pc", 
# 					"spent_transport_diff_pc", "spent_shelter_diff_pc", 
# 					"spent_celebrations_diff_pc",   "spent_education_diff_pc", 
# 					"spent_phone_diff_pc",  "spent_hygiene_diff_pc",
# 					"spent_hh_items_diff_pc", "spent_other_diff_pc", 
# 					
# 					"spent_wood_diff_pc", "spent_lpg_diff_pc", "spent_debt_diff_pc"
# 				),
# 				labels = 
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco", 
# 						"clothing",  "medical", 
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other", 
# 						"other", "other", 
# 						
# 						"  wood  ", "   lpg   ", " debt "
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure_diff_pc, fill = fuel_30_purchased_category)) +
# 	geom_boxplot(position = "dodge") +	
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = fuel_30_purchased_category), position = position_dodge(0.8), color = "red") +
# 	ggpubr::stat_compare_means(
# 		aes(group = fuel_30_purchased_category),
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = FALSE, # not paired because difference
# 		
# 		label.y = 4.5 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		axis.text.x = element_text(angle = 0, hjust = 1),
# 		# axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in category of monthly expenditure as percent of total expenditure",
# 		x = "Expenditure category",
# 		y = "Percent of monthly expenses (%)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) # + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-3, 5)) # does not show one intervention hh data point for wage labor of -50,000
# 
# fig_monthly_expenditures_diffs_pre_intervention_lpg_insufficient <-
# 	survey_data %>%
# 	filter(study_arm == "post-intervention") %>%
# 	rowwise() %>%
# 	mutate(
# 		
# 	) %>%
# 	ungroup() %>%
# 	select(study_arm, fcn_id, lpg_sufficiency, all_of(spent_monthly_vars)) %>% 
# 	# group_by(study_arm, fuel_30_buy_wood) %>%
# 	# 		summarise_at(
# 	# 	vars(spent_monthly_vars),
# 	# 	list(mean),
# 	# 	na.rm = TRUE
# 	# ) %>%
# 	
# 	gather(-study_arm, -fcn_id, -lpg_sufficiency, key = "category", value = "expenditure") %>% 
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month",
# 					"spent_food",  "spent_tobacco_pan", 
# 					"spent_clothing_month", "spent_medical_month", 
# 					"spent_transport", "spent_shelter_month", 
# 					"spent_celebrations_month",   "spent_education_month", 
# 					"spent_phone",  "spent_hygiene",
# 					"spent_hh_items", "spent_other_month", 
# 					
# 					"buy_wood_cost", "spent_debt_month"
# 				),
# 				labels = 
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco", 
# 						"clothing",  "medical", 
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other", 
# 						"other", "other", 
# 						
# 						"  wood  ", " debt "
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure, fill = lpg_sufficiency)) + # fuel_30_buy_wood
# 	geom_boxplot(position = "dodge") +
# 	ggpubr::stat_compare_means(
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = TRUE,
# 		
# 		label.y = 20000 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_text(angle = 45, hjust = 1),
# 		axis.text.x = element_text(angle = 0), # 
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Monthly expenditures among intervention households that did and did run out of LPG at endline", #"Monthly expenditures among households that did and did not purchase wood in the past 30 days"
# 		x = "Expense category",
# 		y = "Monthly expenditure (BDT)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(0, 22000)) # removes one pre-intervention hh data point for debt of 50,000
# 
# 
# 
# monthly_spent_diffs_pc_lpg_insufficient <- 
# 	survey_data_base_wide %>%
# 	filter(study_arm.endline == "post-intervention") %>%
# 	select(fcn_id, paste0(c("study_arm", all_of(spent_monthly_vars)), ".baseline"), paste0(c("study_arm", all_of(spent_monthly_vars)), ".endline"), 
# 				 fuel_30_gather_scraps.endline, fuel_30_collect_wood.endline, fuel_30_buy_wood.endline, fuel_30_buy_lpg.endline, fuel_30_buy_crh.endline
# 	) %>%
# 	rowwise() %>%
# 	mutate(
# 		use_non_lpg_30_days = sum(fuel_30_gather_scraps.endline, fuel_30_collect_wood.endline, fuel_30_buy_wood.endline, fuel_30_buy_lpg.endline, fuel_30_buy_crh.endline, na.rm = TRUE),
# 		lpg_sufficiency = ifelse(use_non_lpg_30_days > 0, "LPG insufficient", "LPG sufficent"),
# 		
# 		
# 		spent_total_month_diff_pc = spent_total_month_with_6mo_monthly.endline / spent_total_month_with_6mo_monthly.baseline,
# 		spent_food_diff_pc = (spent_food.endline / spent_total_month_with_6mo_monthly.endline) - (spent_food.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_wood_diff_pc = (buy_wood_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_wood_cost.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_tobacco_pan_diff_pc = (spent_tobacco_pan.endline / spent_total_month_with_6mo_monthly.endline) - (spent_tobacco_pan.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_clothing_diff_pc = (spent_clothing_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_clothing_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_debt_diff_pc = (spent_debt_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_debt_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_medical_diff_pc = (spent_medical_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_medical_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_transport_diff_pc = (spent_transport.endline / spent_total_month_with_6mo_monthly.endline) - (spent_transport.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_hygiene_diff_pc = (spent_hygiene.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hygiene.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_phone_diff_pc = (spent_phone.endline / spent_total_month_with_6mo_monthly.endline) - (spent_phone.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_shelter_diff_pc = (spent_shelter_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_shelter_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_other_diff_pc = (spent_other.endline / spent_total_month_with_6mo_monthly.endline) - (spent_other.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_celebrations_diff_pc = (spent_celebrations_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_celebrations_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_education_diff_pc = (spent_education_month.endline / spent_total_month_with_6mo_monthly.endline) - (spent_education_month.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		spent_hh_items_diff_pc = (spent_hh_items.endline / spent_total_month_with_6mo_monthly.endline) - (spent_hh_items.baseline / spent_total_month_with_6mo_monthly.baseline),
# 		
# 		spent_lpg_diff_pc = (buy_lpg_cost.endline / spent_total_month_with_6mo_monthly.endline) - (buy_lpg_cost.baseline / spent_total_month_with_6mo_monthly.baseline)
# 	) %>%
# 	select(-c(fuel_30_gather_scraps.endline, fuel_30_collect_wood.endline, fuel_30_buy_wood.endline, fuel_30_buy_lpg.endline, fuel_30_buy_crh.endline)) %>%
# 	select(fcn_id, study_arm.baseline, lpg_sufficiency, contains("_diff")) %>%
# 	gather(-fcn_id, -study_arm.baseline, -lpg_sufficiency, key = category, value = "expenditure_diff")
# 
# fig_monthly_spent_diffs_pc_lpg_insufficient <-
# 	monthly_spent_diffs_pc_lpg_insufficient %>%
# 	mutate(
# 		category = as.factor(category),
# 		category = 
# 			ordered(
# 				category,
# 				levels = c(
# 					"spent_total_month_diff_pc",
# 					"spent_food_diff_pc",  "spent_tobacco_pan_diff_pc", 
# 					"spent_clothing_diff_pc", "spent_medical_diff_pc", 
# 					"spent_transport_diff_pc", "spent_shelter_diff_pc", 
# 					"spent_celebrations_diff_pc",   "spent_education_diff_pc", 
# 					"spent_phone_diff_pc",  "spent_hygiene_diff_pc",
# 					"spent_hh_items_diff_pc", "spent_other_diff_pc", 
# 					
# 					"spent_wood_diff_pc", "spent_lpg_diff_pc", "spent_debt_diff_pc"
# 				),
# 				labels = 
# 					c(
# 						"all expenditures",
# 						"  food  ",  "pan/\ntobacco", 
# 						"clothing",  "medical", 
# 						"transport", "shelter",
# 						"other", "other",
# 						"other", "other", 
# 						"other", "other", 
# 						
# 						"  wood  ", "   lpg   ", " debt "
# 					)
# 			)
# 	) %>%
# 	ggplot(aes(x = category, y = expenditure_diff, fill = lpg_sufficiency)) +
# 	geom_boxplot(position = "dodge") +
# 	ggpubr::stat_compare_means(
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "wilcox.test",
# 		paired = TRUE,
# 		
# 		label.y = 1.5 #,
# 		# hide.ns = TRUE#,
# 		# ref.group = ".all."
# 		# show.legend = TRUE # doesn't show the legend of stas?
# 	) +
# 	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		axis.text.x = element_text(angle = 0, hjust = 1),
# 		# axis.text.x = element_blank(), # angle = 90
# 		axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) + 
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Monthly expenditures among intervention households that did and did run out of LPG at endline", #"Monthly expenditures among households that did and did not purchase wood in the past 30 days"
# 		x = "Expense category",
# 		y = "Monthly expenditure (BDT)" # we expect that pre-intervention group had a bigger change in spending
# 	) + 
# 	# facet_wrap(~ study_arm.baseline, nrow = 1) # + # fuel_30_buy_wood
# 	coord_cartesian(ylim = c(-3, 5)) # does not show one intervention hh data point for wage labor of -50,000
# 
