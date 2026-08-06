################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

## Chris's
# source(here::here("1_config.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################


# # LPG use and safety
# Average duration between when enrolled in LPG and when receive LPG and training
# Separate for UNHCR and IOM areas




# The following dates should already be in date format but for some reason they are not so use dmy to make them into dates
lpg_wait_duration <- 
	survey_data %>%
	rowwise() %>%
	mutate(lpg_wait_duration = first_receive_lpg - as.character(first_enrolled_lpg))

# difftime(ymd(as.character(first_receive_lpg)), ymd(as.character(first_enrolled_lpg)), units = "weeks") 

lpg_use_without_training <-
	survey_data %>%
	difftime(dmy(date_safety_training), dmy(first_receive_lpg), "weeks") %>%
	mean(na.rm = TRUE)


### Prior familiarity with LPG

survey_data %>%
	summarise_at(
		vars(lpg_prior_use),
		list(mean),
		na.rm = TRUE
	)


#######################################################################
### Safety
#######################################################################

# fire_cause for fire_why
# 1	Carelessness
# 2	I was busy with other work
# 3	While children were playing with the fire
# 4	Due to the low height of the roof and/or the stove was close to the fence
# 5	Don't know
# 66	Other
# 
# fire_consequence
# 1	Entire household burned out
# 2	Part of the household burned out
# 3	Mild burn of the children
# 4	Severe burn of the children
# 5	Severe burn of the adult 
# 6	Mild burn of the adult
# 7	People die
# 8	Don't know
# 66	Other


## Vector of variables to summarize

table13Vars <- 
	c(
		"stove_training",
		"lpg_no_training_learn",
		"lpg_safety_visit", 
		
		# 1	plastic tarp (with or without woven bamboo)
		# 2	tin / metal
		# 3	bamboo
		# 4	earth/clay
		# 5	earth and a thin layer of cement
		# 6	cement/ concrete/ brick
		# 7	thatch/leaf
		# 8	wood
		# 9	tile
		# 66	other
		"flooring_below_stove",
		
		# 1	Earthen fireguard
		# 4	tin / metal
		# 2	Gap of 1+ handspan
		# 3	No space
		"space_between_wall_stove", 
		
		"lpg_afraid",
		"lpg_gas_leak",# Have you ever smelled gas coming out of your stove
		"lpg_child_burn", 
		"fire_number",
		"fire_why",
		"fire_consequence"
	)

## Vector of categorical variables that need transformation
table13FactorVars <- table13Vars

# Create a TableOne object
tab13 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table13Vars, 
		factorVars = table13FactorVars# , 
		# strata = "study_arm_overall"
	)

tab13

survey_data %>%
	summarise_at(
		vars(starts_with("training_")),
		list(mean),
		na.rm = TRUE
	) %>%
	gather(key = "training_type", value = "pc_hh")

survey_data %>%
	filter(!is.na(lpg_afraid)) %>%
	select(lpg_afraid_why)



############################################################################
## LPG refills and sufficiency
##############################################################################

# refill_time_lpg_missed
# 0	No
# 1	Yes,  I couldn't go on the exact refill date and they wouldn't let me go on a different day
# 2	Yes, other
# 3	Don't know


survey_data %>%
	select(refill_time_lpg_missed) %>%
	table()


# times_bf_refill
# "How long before the next LPG refill does your household run out of LPG for cooking food. (Answer in meals or days – select the answer that best matches the response)"
# 1	1-3 days before my refill
# 2	4-6 days before my refill
# 3	7-9 days before my refill
# 4	10+ days before my refill
# 5	1-5 meals before my refill 
# 6	6-10 meals before my refill 
# 7	10-15 meals before my refill 
# 8	15-20 meals before my refill 
# 9	20+ meals before my refill
# 10	1-3 meals per week
# 11	4-6 meals per week
# 12	7-9 meals per week
# 13	10+ meals per week


survey_data %>%
	select(fuel_use_non_lpg_freq_cook) %>%
	table()


# ## Use of extra fuel
# lpg_use
# 1	Prepare snacks/ food that I sell
# 2	Give the extra LPG to others for free
# 3	Sell the extra LPG to another household
# 4	Sell the extra LPG to a restaurant
# 5	Keep on using it myself
# 6	We never have more than we need to cook for just my family


survey_data %>%
	select(lpg_extra_use) %>%
	table()


# Receive LPG but use other fuel
##  
# "For cooking food or boiling water, do you ever have to use fuel other than the LPG that you receive?""

survey_data %>%
	select(fuel_use_non_lpg_ever) %>%
	table()

survey_data %>%
	summarise_at(
		vars(starts_with("fuel_use_non_lpg")),
		list(mean),
		na.rm = TRUE
	) %>%
	gather(key = "alternative_fuel", value = "pc_hh")



################################################################################
# Number of days cylinder lasts
################################################################################

# lpg_days_possible
# hh_size

# survey_data %>%
# 	select(lpg_days_possible, hh_size) %>%
# 	filter(lpg_days_possible < 5) %>%
# 	View()

fig_lpg_cyliner_last_hh_size <-
	survey_data %>%
	# remove unlikely data
	filter(lpg_days_possible > 5) %>%
	
	ggplot(aes(x = hh_size, y = lpg_days_possible)) +
	# geom_jitter() + 
	geom_hex() +
	geom_smooth(method = "loess") +
	geom_segment(aes(x = 1, xend = 2, y = 45, yend = 45), colour = "red", size = 2) +
	geom_segment(aes(x = 3, xend = 6, y = 35, yend = 35), colour = "red", size = 2) +
	geom_segment(aes(x = 7, xend = 10, y = 29, yend = 29), colour = "red", size = 2) +
	geom_segment(aes(x = 11, xend = 13, y = 22, yend = 22), colour = "red", size = 2) +
	scale_x_continuous(breaks = seq(0, 20, 1), minor_breaks = FALSE) +
	scale_y_continuous(breaks = seq(0, 50, 4)) +
	# theme_classic() +
	# theme(
	# 	axis.text.x = element_text(angle = 45, hjust = 1)
	# ) +
	guides(
		fill = guide_colorbar(title = "Number of households"),
		color = guide_legend(element_blank()),
		size = guide_legend(element_blank())
	) +
	labs(
		title = "Usable Duration of 12 kg LPG Cylinder by Household Size ",
		x = "Household size",
		y = "Number of days that LPG typically lasts"
	) + 
	theme_bw()

fig_lpg_cyliner_last_hh_size

ggsave(
	here::here("6_figures", "fig_lpg_cyliner_last_hh_size.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "tiff"
)

###############################################################################33
#### LPG cylinder, stove required repair
##############################################################################

# contains("_repair"): lpg_stove_repair, lpg_stove_repair_inspect, lpg_cylinder_repair, lpg_cylinder_repair_inspect, lpg_repair_details, lpg_repair costs
# 
# 
# [lpg_stove_part]	0	NA - nothing broken
# [lpg_stove_part]	1	Key (ignition dial)
# [lpg_stove_part]	2	Burner
# [lpg_stove_part]	3	Superstructure
# [lpg_stove_part]	4	Rust
# [lpg_stove_part]	5	Stove body
# [lpg_stove_part]	66	Other
# 
# [lpg_tank_part]	0	NA - nothing broken
# [lpg_tank_part]	1	Pin
# [lpg_tank_part]	2	Pipe (hose)
# [lpg_tank_part]	3	Cylinder
# [lpg_tank_part]	4	Rust
# [lpg_tank_part]	5	Hose clamp
# [lpg_tank_part]	6	Top of cylinder
# [lpg_tank_part]	66	Other
# 
# 
# [lpg_repair_details]	1	Repaired at LPG depot	
# [lpg_repair_details]	2	Replaced by LPG depot
# [lpg_repair_details]	3	Repaired at private store
# [lpg_repair_details]	4	Purchased a new part privately	
# [lpg_repair_details]	5	Have not repaired or replaced



# survey_data %>%
# 	select(lpg_days_possible, hh_size) %>%
# 	filter(lpg_days_possible < 5) %>%
# 	View()

fig_lpg_stove_repair <-
	survey_data %>%
	filter(timepoint == "endline") %>% # endline only
	select(fcn_id, starts_with("lpg_stove_repair")) %>%
	select(-c(contains("inspect"), lpg_stove_repair, lpg_stove_repair_inspect, lpg_stove_repair_image, lpg_stove_repair_image_2)) %>%
	gather(-fcn_id, key = "repair_issue", value = "need_repair") %>%
	mutate(
		# repair_issue_new = str_remove(repair_issue, "_inspect"),
		# reported_inspected = str_extract(repair_issue, "inspect"),
		# reported_inspected = ifelse(is.na(.), 0, reported_inspected),
		# need_repair = as.numeric(need_repair),
		repair_issue = 
			factor(
				repair_issue,
				levels = 
					c(
						"lpg_stove_repair_0",
						"lpg_stove_repair_1", "lpg_stove_repair_2",
						"lpg_stove_repair_3", "lpg_stove_repair_4",
						"lpg_stove_repair_5", 
						"lpg_stove_repair_66"
					),
				labels = 
					c(
						"nothing broken", 
						"ignition dial", "burner", 
						"superstructure", "rust", 
						"stove body", 
						"other"
					)
				# c("nothing broken", "pin", "hose", "cylinder")
			)
	) %>%
	# filter(reported_inspected == "inspect") %>% # keep only the data from the inspection
	ggplot(aes(x = repair_issue, y = need_repair, fill = repair_issue)) +
	stat_summary(fun = "mean", geom = "bar") +
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
	# ggplot(aes(x = repair_issue_new, y = need_repair), group = reported_inspected) +
	# stat_summary(fun = "mean", geom = "bar", color = "black", aes(group = reported_inspected)) +
	# stat_summary(fun.data = "mean_cl_boot", geom = "linerange", aes(group = reported_inspected)) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 7/8,
		name = "Demographic"
	) +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) + # Percentage labels rounded to the nearest integer
	theme_bw() +
	theme(
		# axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "none"
	) +
	labs(
		title = "LPG stove repairs required as of November 2020",
		x = "Part of stove that requires repair",
		y = "Percentage of households"
	) #+ 
# facet_wrap(~ reported_inspected)


fig_lpg_cylinder_repair <-
	survey_data %>%
	filter(timepoint == "endline") %>% # endline only
	select(fcn_id, starts_with("lpg_cylinder_repair")) %>%
	select(-c(contains("inspect"), lpg_cylinder_repair, lpg_cylinder_repair_inspect, lpg_cylinder_repair_image, lpg_cylinder_repair_image_2)) %>%
	gather(-fcn_id, key = "repair_issue", value = "need_repair") %>%
	mutate(
		# repair_issue_new = str_remove(repair_issue, "_inspect"),
		# reported_inspected = str_extract(repair_issue, "inspect"),
		# reported_inspected = ifelse(is.na(.), 0, reported_inspected),
		# need_repair = as.numeric(need_repair),
		repair_issue = 
			factor(
				repair_issue,
				levels = 
					c("lpg_cylinder_repair_0",
						"lpg_cylinder_repair_1", "lpg_cylinder_repair_2",
						"lpg_cylinder_repair_3", "lpg_cylinder_repair_4",
						"lpg_cylinder_repair_5", "lpg_cylinder_repair_6",
						"lpg_cylinder_repair_66"
					),
				labels = 
					c("nothing broken", 
						"pin", "hose", 
						"cylinder", "rust", 
						"hose clamp", "top of \ncylinder", 
						"other"
					)
			)
	) %>%
	# filter(reported_inspected == "inspect") %>% # keep only the data from the inspection
	ggplot(aes(x = repair_issue, y = need_repair, fill = repair_issue)) +
	stat_summary(fun = "mean", geom = "bar") +
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
	# ggplot(aes(x = repair_issue_new, y = need_repair), group = reported_inspected) +
	# stat_summary(fun = "mean", geom = "bar", color = "black", aes(group = reported_inspected)) +
	# stat_summary(fun.data = "mean_cl_boot", geom = "linerange", aes(group = reported_inspected)) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 7/8
	) +
	scale_y_continuous(
		breaks = c(0, 0.5, 1, 1.5, 2),
		labels = scales::percent_format()
	) + # Percentage labels rounded to the nearest integer
	theme_bw() +
	theme(
		# axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "none"
	) +
	labs(
		title = "LPG cylinder repairs required as of November 2020",
		x = "Part of cylinder that requires repair",
		y = "Percentage of households"
	) #+ 
# facet_wrap(~ reported_inspected)

ggarrange(
	fig_lpg_stove_repair,                                                 # First row with scatter plot
	fig_lpg_cylinder_repair,
	nrow = 2
	# label = "A"                                     # Labels of the scatter plot
)

ggsave(
	here::here("6_figures", "fig_lpg_stove_cylinder_repair.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "tiff"
)

###############################################################################
#### Willingness-to-pay #####
###############################################################################

# lpg_willingness_to_pay
# buy_lpg_cost


survey_data %>%
	select(lpg_willingness_to_pay) %>%
	summary()

fig_lpg_willingness_to_pay <-
	survey_data %>%
	filter(timepoint == "endline") %>% 
	ggplot(aes(x = lpg_willingness_to_pay)) +
	geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10) + 
	# Bangladesh LPG price as of 12 April 2021: https://www.dhakatribune.com/bangladesh/power-energy/2021/04/12/govt-lpg-price-reset-at-tk591-private-one-at-tk975
	geom_vline(xintercept = 591, lty = 2, color = "dark green") + 
	geom_vline(xintercept = 975, lty = 2, color = "red") + 
	scale_x_continuous(breaks = seq(0, 1300, 100)) +
	scale_y_continuous(labels = scales::percent) +
	theme_bw() + 
	labs(
		title = "Willingnes to Pay for 12 kg tank of LPG",
		x = "Amount willing to pay (BDT)",
		y  = "Number of households"
	)

# ggplot(aes(y = lpg_willingness_to_pay)) +
# stat_summary(
# 	fun = "mean",
# 	geom = "bar"
# ) +
# stat_summary(
# 	fun.data = "mean_cl_boot",
# 	geom = "linerange"
# )

fig_lpg_willingness_to_pay

ggsave(
	here::here("6_figures", "fig_lpg_willingness_to_pay.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "tiff"
)

##############################################################################
#### Efficiencient cooking methods
############################################################################
## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table11Vars <- 
	c("soak_rice", "soak_lentils", "cover_pot")

## Vector of categorical variables that need transformation
table11FactorVars <- table11Vars

# Create a TableOne object
tab11 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table11Vars, 
		factorVars = table11FactorVars, 
		strata = "study_arm"
	)

tab11