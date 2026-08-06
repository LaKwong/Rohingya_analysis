##################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: harassment analysis
# @Date: 220511
################################################################################
rm(list = ls())
source(here::here("0_config_RF111.R"))
## Chris
# source(here::here("1_config.R"))
# source(here::here("1.5_define_vector_columns_RF111.R")) Chris thinks this is 
# already ran pre use of the identified dataset

# Parameters
#### Input Files ####
#file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

file_survey_data_base <- "/Users/tasmiahkhan/My Drive/Rohingya Fuel/Rohingya_Fuel_Project/RohingyaFuel_survey_data_triple_RF111.rds"


#### Output Files ####

# file_out_1a <- here::here("4_data/harassment_analysis.rds")
# file_out_1b <- here::here("4_data/harassment_analysis.csv")
# file_out_2 <- here::here ("4_data/harassment_freq_prev.rds")
# file_out_3 <- here::here("6_figures/barplot_sexual_harassment.png")


#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)

# ============================================================================


# vars(harassment_vars)
# choices: [frequency_times]
# 0	Never
# 1	Once
# 3	Three times
# 4	More than three times

# for fuel_collection_who, the denominator is hh that EVER collected type X fuel
fuel_collection_who <- c()
for (fuel in c("gather_scraps", "collect_wood", "receive_wood", "buy_wood", "receive_lpg", "buy_lpg", "receive_crh", "buy_crh")){
	for (person in c("w", "g", "m", "b")){
		fuel_collection_who <- c(fuel_collection_who, paste(fuel, person, sep = "_"))
	}
}

fuel_collection_who_ever <- c()
for (fuel in c("gather_scraps", "collect_wood", "receive_wood", "buy_wood", "receive_lpg", "buy_lpg", "receive_crh", "buy_crh")){
	for (person in c("w", "g", "m", "b")){
		fuel_collection_who_ever <- c(fuel_collection_who_ever, paste(fuel, person, "ever", sep = "_"))
	}
}

# for any_harassment_hh, the denominator is ALL households
any_harassment_hh <- # did anyone in the hh experience this type of harassment FOR ANY REASON? Y/N
	c(
		"insult_hh", "belittle_hh", "scare_hh", "push_hh", 
		"hit_hh", "kick_hh", "choke_hh", "weapon_hh", 
		"sex_lang_hh", "sex_contact_hh", "sex_rumor_hh", 
		"clothing_pull_hh", "sex_corner_hh"
	)

# for fuel_harassment_hh, the denominator is households that said they EvER collected type X fuel 
fuel_harassment_hh <- # did anyone in the hh experience this type of harassment related to fuel collection? Y/N
	survey_data %>% 
	select(
		ends_with("insult_hh"), ends_with("belittle_hh"), ends_with("scare_hh"), ends_with("push_hh"), 
		ends_with("hit_hh"), ends_with("kick_hh"), ends_with("choke_hh"), ends_with("weapon_hh"), 
		ends_with("sex_lang_hh"), ends_with("sex_contact_hh"), ends_with("sex_rumor_hh"), 
		ends_with("clothing_pull_hh"), ends_with("sex_corner_hh")
	) %>%
	select(
		-all_of(any_harassment_hh)
	) %>%
	names()

# What was the frequency of harassment by the specific people? (Categorical number of times per week)
# Determine prev with harassment_vars_yn
# Need to make the gender-specific denominator the fuel_collection_who - fuel-and gender-specifc 
harassment_vars <- 
	survey_data %>%
	select( 
		starts_with("gather_scraps"), 
		starts_with("collect_wood"), 
		starts_with("receive_wood"), 
		starts_with("buy_wood"), 
		starts_with("receive_lpg"), 
		starts_with("buy_lpg"), 
		starts_with("receive_crh"), 
		starts_with("buy_crh")
	) %>%
	select(
		-c(
			all_of(fuel_collection_who), # remove the vars that only indicate of that type of person collects the type of fuel
			all_of(fuel_collection_who_ever), # remove the vars that only indicate of that type of person collects the type of fuel
			# any_harassment_hh, # These weren't selected by the section criteria
			all_of(fuel_harassment_hh), # remove the vars that are the hh level of harassment 
			gather_scraps_dead, # gather_scraps_dead.1, #gather_scraps_dead1,
			collect_wood_walk_hr, collect_wood_forest_start, collect_wood_forest_stop,
			receive_wood_wait,
			buy_wood_cost, buy_wood_reason, buy_wood_when_start,
			receive_lpg_walk, receive_lpg_wait,
			buy_lpg_cost, buy_lpg_walk, buy_lpg_wait,
			receive_crh_walk, receive_crh_wait
			
			#	# vars that don't exist yet	
			# time_child_gathering_nonwood_items = time_child_gathering_non.wood_items, 
			# respondent_resp_rate  = resp_rate_reported_respondant,       
			# respondent_weight_loss = weight_loss_reported_respondant,
			# reduce_meals_lack_food = reduce_meals,
			# not_eat_lack_food = not_eat,
			# reduce_meals_lack_fuel = reduce_meals1,
			# not_eat_lack_fuel = not_eat1,
			# fuel_ever_gather_scraps = fuel_ever_scraps,
			# fuel_30_gather_scraps = fuel_30_scraps,
			# gather_wood_dead = gather_scraps_dead1,
			# collect_wood_times_week = times_wood_day,
			# reason_forest_defecation = reason_forest_defacation,
			# cook_to_sell_percent = cook_to_sell,
			# cook_sell_days_week = cook_sell_yesterday
		)
	) %>%
	names()

# Prevalence of harassment of certain genders; denominator is hh with that gender that EVER collected type X fuel
harassment_vars_yn <-  
	map_chr(harassment_vars, ~ paste0(., "_yn"))

# harassment_vars_by_group_yn <-  
# 	map_chr(harassment_vars_by_group, ~ paste0(., "_yn"))




# Harassment

# Questions set: 
# 	
# 	Since arriving at the camp, has anyone insulted you or any of your family members or made them feel bad about him/herself?
# 	Who caused this negative experience?  (harassment_who_insult)
# Did this happen when the person was gathering scraps/leaves/twigs for fuel?
# 	
# 	How many times has this happened to….? (number)
# 	women
#   girls
#   men
#   boys

# sex_contact = "Has somebody ever brushed up against you or one of your family members in a sexual way on purpose?"

## Test examples
# The percentage of households that experienced specific types of harassment at any time since they entered the camp

# Prevalence of harassment of certain genders; denominator is hh with that gender that EVER collected type X fuel 
# scraps_gather_insult_men_yn == 1 / (scraps_gather_m == 1)

# survey_data %>%
# 	select(harassment_vars_yn) %>% # any_harassment_hh, fuel_collection_who, 
# 	View()

# fuel_collection_who_alt_order {fuel_type}_{w/m/g/b}_ever
# fuel_collection_who_alt_order_summary_names  {fuel_type}_{w/m/g/b}



#####################################################################################################################
#####################################################################################################################

# We now have individual data for each hh, so make a long data frame that has one row for each demographic of each hh


df_harassment_base <-
	survey_data %>%
	filter(timepoint == "endline") %>% # endline vars
	# select(ends_with("_w_yn"), ends_with("_m_yn"), ends_with("_g_yn"), ends_with("_b_yn")) %>% # baseline vars
	select(
		study_arm_overall, fcn_id, 
		# fuel_ever_ is hh ever collected that type of fuel
		starts_with("fuel_ever_"), 
		# {fuel_type}_{w/g/m/b}_ever and {fuel_type}_{harassment_type}_{w/g/m/b}_ever
		ends_with("_w_ever"), ends_with("_m_ever"), ends_with("_g_ever"), ends_with("_b_ever"),
		# hh_ever was anyone in the hh ever harassed?
		ends_with("_hh_ever"),
		# harassment_who_{harassment_type} who perpetrated the harassment
		starts_with("harassment_who")
	)


# # For prevalence, need to make the number of times into a yn
# mutate_at(
# 	vars(ends_with("_w_ever"), ends_with("_m_ever"), ends_with("_g_ever"), ends_with("_b_ever")), # these variables are the number of times that harassment occurred
# 	list(yn = ~ifelse(. > 0, TRUE, FALSE))
# )
# vars that end with _ever are the number of times the harassment has occurred
# vars the end with _yn are about whether the harassment has ever happened

# select(collect_wood_choke_w_ever_yn) %>% glimpse()

# # This tells us if the hh ever gathered this type of fuel but not who in the hh
# df_fuel_ever_fcn_id <-
# 	df_harassment_base %>%
# 	select(study_arm_overall:fuel_ever_other) %>% # includes fuel_ever_other
# 	pivot_longer(cols = starts_with("fuel_ever"), names_to = "fuel_type", names_prefix = "fuel_ever_", values_to = "fuel_ever") 


harassment_types <- c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon", "sex_lang", "sex_contact", "sex_rumor", "clothing_pull", "sex_corner")

df_harassment_demographics <- 
	df_harassment_base %>% 
	select(study_arm_overall, fcn_id, contains("_w_")) %>%
	mutate(demographic = "w") %>%
	rename_at(vars(contains("_w_")), function(x) gsub("_w_", "_", x)) %>%
	bind_rows(
		df_harassment_base %>%
			select(study_arm_overall, fcn_id, contains("_m_")) %>%
			mutate(demographic = "m") %>%
			rename_at(vars(contains("_m_")), function(x) gsub("_m_", "_", x))
	) %>%
	bind_rows(
		df_harassment_base %>%
			select(study_arm_overall, fcn_id, contains("_b_")) %>%
			mutate(demographic = "b") %>%
			rename_at(vars(contains("_b_")), function(x) gsub("_b_", "_", x))
	) %>%
	bind_rows(
		df_harassment_base %>%
			select(study_arm_overall, fcn_id, contains("_g_")) %>%
			mutate(demographic = "g") %>%
			rename_at(vars(contains("_g_")), function(x) gsub("_g_", "_", x))
	) %>%
	select(study_arm_overall, fcn_id, demographic, everything())

# select(study_arm_overall, fcn_id, demographic, gather_scraps_insult_ever:buy_crh_sex_corner_ever) 
# rename_at(vars(gather_scraps_insult_ever:buy_crh_sex_corner_ever_yn), function(x) sub('(^[^_]+_[^_]+)_(.*)$', '\\1.\\2', x)) #%>%


# the number of times was only asked if the person ever reported collecting that type of fuel so tm


df_harassment_who_long <-
	df_harassment_base %>%
	select(fcn_id, contains("harassment_who")) %>%
	pivot_longer(
		cols = harassment_who_insult:harassment_who_clothing_pull, 
		names_to = c("harassment_who", "harassment_type"), 
		names_pattern = "(.{10}_.{3})_(.*_*.*)", 
		# names_pattern = "(.*_.*)_([^_]+$)", # The parentheses are used to indicate group 1 and group 2
		values_to = "perpetrator"
	) %>%
	select(-harassment_who)


df_harassment_analysis <-
	df_harassment_demographics %>%
	# select(-c(gather_scraps_ever:buy_crh_ever)) %>% # Calling columns this way is risky, but it works
	# This way is better
	select(!(contains("ever") & !(contains(harassment_types)))) %>% # , contains("harassment_who")
	pivot_longer(
		cols = gather_scraps_insult_ever:buy_crh_sex_corner_ever, 
		names_to = c("fuel_type", "harassment_type", "ever"), 
		# regex are hard!
		# ^         # beginning of string
		# 	(?:       # non-capturing group
		# 	 	[^_]+ # not _, at least once
		# 	 	_     # _
		# 	){1}      # repeat the group once
		# ([^_ ]+)  # capture characters not _ or spaces to group 1
		
		names_pattern = "(^(?:[^_]+_){1}[^_ ]+)_(.+_*.*)_(.{4})$", 
		values_to = "times_occurred"
	) %>%
	select(-ever) %>%
	mutate(
		# harassment_type = str_extract(harassment_type, pattern = "^[a-z]+_[a-z]+"),
		# the number of times was only asked if the person ever reported collecting that type of fuel so 
		# if the number of times was NA it means that the person did not every collect that type of fuel so could not be subject to harassment 
		# if the number of times was 0 it means that the person collected the fuel but was not harassed
		yn = 
			case_when(
				is.na(times_occurred) ~ NA,
				times_occurred == 0 ~ FALSE, 
				times_occurred > 0 ~ TRUE
			)
	) %>%
	left_join(df_harassment_who_long, by = c("fcn_id", "harassment_type")) %>%
	# If times_occurred == NA or 0, then perpetrator = NA
	mutate(perpetrator = ifelse(is.na(times_occurred) | times_occurred == 0, NA, perpetrator)) %>%
	mutate(
		harassment_category =
			ordered(
				harassment_type,
				levels =
					c(
						"insult", "belittle", "scare", 
						"push", "hit", "kick", 
						"choke", "weapon", "sex_lang", 
						"sex_contact", "sex_rumor", 
						"clothing_pull", "sex_corner"
					),
				labels = 
					c(
						"verbal (insulted, belittled, scared)", 
						"verbal (insulted, belittled, scared)", 
						"verbal (insulted, belittled, scared)", 
						"physical (pushed, hit, kicked, choked, faced a \nweapon)", "physical (pushed, hit, kicked, choked, faced a \nweapon)",
						"physical (pushed, hit, kicked, choked, faced a \nweapon)", "physical (pushed, hit, kicked, choked, faced a \nweapon)",
						"physical (pushed, hit, kicked, choked, faced a \nweapon)",
						"sexual (language, rumors, brush past, clothing pulled, cornered)", "sexual (language, rumors, brush past, clothing pulled, cornered)", 
						"sexual (language, rumors, brush past, clothing pulled, cornered)", 
						"sexual (language, rumors, brush past, clothing pulled, cornered)", "sexual (language, rumors, brush past, clothing pulled, cornered)"
					)
			),
		harassment_type_label =
			ordered(
				harassment_type,
				levels = 
					c(
						"insult", "belittle", "scare", 
						"push", "hit", "kick", 
						"choke", "weapon", "sex_lang", 
						"sex_contact", "sex_rumor", 
						"clothing_pull", "sex_corner"
						
						# "insulted", "belittled", "scared", 
						# "pushed", "hit", "kicked", "choked", "faced a weapon", 
						# "subject to sexual language",  "brushed by sexually", "subject to sexual rumors", 
						# "clothing pulled", "cornered in a sexual way"
					),
				labels = 
					c(
						"insulted", "belittled", "scared", 
						"pushed", "hit/kicked", "hit/kicked", "choked/\nfaced a \nweapon", "choked/\nfaced a \nweapon", 
						"subject to \nsexual \nlanguage/ \nrumors", "brushed by \nsexually /\nclothing \npulled", "subject to \nsexual \nlanguage/ \nrumors", 
						"brushed by \nsexually /\nclothing \npulled", "cornered in a \nsexual way"
					)
			),
		demographic_label = 
			ordered(
				demographic,
				levels = c("m", "w", "g", "b"),
				labels = c("Men", "Women", "Girls", "Boys")
			)
	) %>%
	rename(
		fuel_type_collected = fuel_type
	) %>%
	select(study_arm_overall, fcn_id, fuel_type_collected, demographic, demographic_label, harassment_type, harassment_type_label, times_occurred, yn) %>%
	arrange(desc(times_occurred))


saveRDS(df_harassment_analysis, file_out_1a)
write.csv(df_harassment_analysis, file_out_1b)


# select(sort(current_vars())) %>% # sort() puts in alaphabetical order; current_vars() calls all column names



## Harassment frequency, by gender
# 13 Harassment types:
# 	c("insult_hh", "belittle_hh", "scare_hh", "push_hh", "hit_hh", "kick_hh", "choke_hh", "weapon_hh", "sex_lang_hh", "sex_contact_hh", "sex_rumor_hh", "clothing_pull_hh", "sex_corner_hh")
# 
# At baseline, the frequency was selected from frequency_times:
# 	0	Never
# 1	Once
# 2	Twice
# 3	Three times
# 4	More than three times
# 
# At endline, we asked for an integer number of times of harassment





# Compare experience of harassment in physical composite, verbal composite, sexual composite for each fuel type

# fuel_type    physical 0/1

# There are some discreptancies yn_new and "timepoint" column label does not exist.
df_harassment_category_analysis <-
	df_harassment_analysis %>%
  group_by(study_arm_overall, fcn_id, fuel_type_collected) %>%
  # group_by(timepoint, study_arm_overall, fcn_id, fuel_type) %>% # harassment_category collapse the harassment categories because so few physical and sexual
	summarise(index_value = sum(as.numeric(yn_new), na.rm = TRUE)) %>%
	mutate(
		index_yn = 
			ifelse(
				is.na(index_value), NA, 
				ifelse(index_value > 0, 1, 0)
			)
	) # %>%
# 	group_by(fuel_type, harassment_category, index_yn) %>%
#  summarise(index_value_2 = sum(as.numeric(index_yn), na.rm = TRUE))


glm_fuel_type_harassment <-
	glm(formula = index_yn ~ fuel_type, family = binomial, data = df_harassment_category_analysis %>% filter(fuel_type %in% c("buy_wood", "collect_wood", "receive_lpg"))) # + fuel_type * harassment_category (don't expect that fuel type will change harassment - most verbal harassment in collect_wood, most physical harassment in collect_wood)
#  + harassment_category 

summary(glm_fuel_type_harassment)






df_harassment_analysis %>% # only has endline data
	filter(person_obtained == 1) %>%
	select(fcn_id, demographic, fuel_type) %>%
	unique() %>%
	arrange(fcn_id, demographic, fuel_type) %>%
	count(fuel_type, demographic) %>%
	filter(fuel_type %in% c("gather_scraps", "collect_wood", "receive_lpg"))














#########################################################################################################################
#########################################################################################################################



## Harassment prevalence plot
# fig.width=18, fig.height=10

# harassment_prev %>%
# 	kable(digits = 2) %>%
# 	kable_styling(
# 		bootstrap_options = c("striped", "hover", "condensed"),
# 		fixed_thead = TRUE
# 	)

ann_text_gather_scraps <- data.frame(
	fuel_type = "Gather scraps", demographic_label = "Women", harassment_type = "insulted", number_hh_harassed = 0,  
	number_hh_collecting_fuel = 0, pc_harassed = 0.25, mean_freq_harassed_of_those_harassed = 0,   
	label = "(m = 840, w = 198, g = 108, b = 195)"
)

ann_text_collect_wood <- data.frame(
	fuel_type = "collect wood", demographic_label = "Women", harassment_type = "insulted", number_hh_harassed = 0,  
	number_hh_collecting_fuel = 0, pc_harassed = 0.25, mean_freq_harassed_of_those_harassed = 0,   
	label = "(m = 920, w = 51, g = 57, b = 178)"
)

ann_text_receive_lpg <- data.frame(
	fuel_type = "Receive LPG", demographic_label = "Women", harassment_type = "insulted", number_hh_harassed = 0,  
	number_hh_collecting_fuel = 0, pc_harassed = 0.25, mean_freq_harassed_of_those_harassed = 0,   
	label = "(m = 977, w = 585, g = 8, b = 78)"
)

fig_harassment_prev <-
	df_harassment_analysis %>%
	mutate(yn_new = as.numeric(yn_new)) %>%
	# filter(harassment_category == "physical (pushed, hit, kicked, choked, faced a \nweapon)") %>%
	filter(str_detect(fuel_type, pattern = 'gather_scrap|collect_wood|receive_lpg')) %>% # Can't have a " " around "|"
	mutate(
		fuel_type =
			ordered(
				fuel_type,
				levels = c("gather_scraps", "collect_wood", "receive_lpg"),
				labels = c("Gather scraps", "Collect wood", "Receive LPG")
			)
	) %>%
	# group_by(harassment_category) %>%
	ggplot(aes(x = harassment_type, y = yn_new, fill = demographic_label)) + # 
	stat_summary(
		fun = "mean",
		geom = "bar", 
		# color = "black",
		position = position_dodge(0.9)
	) +
	stat_summary(
		fun.data = "mean_cl_boot",
		geom = "linerange",
		position = position_dodge(0.9)
	) +
	# geom_text(data = ann_text_gather_scraps, aes(label = label)) +
	# 	geom_text(data = ann_text_collect_wood, aes(label = label)) +
	# 	geom_text(data = ann_text_receive_lpg, aes(label = label)) +
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
		plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		# axis.text.x = element_text(angle = 60, hjust = 1),
		strip.background = element_blank(),
		strip.text.x = element_blank(),
		panel.spacing = unit(3, "lines") # increase space between faceted plots
	) + 
	labs(
		title = "Harassment experienced while collecting fuel",
		subtitle = "Gathering scraps                                          Collecting firewood                                          Receiving LPG",
		x = "Type of harassment", 
		y = "Percentage of individuals who experienced harassment \nwhile collecting fuel"
	) +
	coord_cartesian(expand = F, clip = "off") +
	# facet_wrap(~ fuel_type, ncol = 3)
	
	facet_wrap(harassment_category ~ fuel_type, ncol = 3, scales = "free_x") # facet_grid doesn't work - puts all the harassment types on the bottom, not sure why; facet_grid can use a space agrument, but facet_wrap cannot; space = "free". Facet_wrap can use ,   strip.position = "right"

fig_harassment_prev

ggsave(
	here::here("6_figures", "harassment.png"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "png"
)


#########################################################################################################################
#########################################################################################################################



#########################################################################################################################
#########################################################################################################################



# # Who was the perpetrator of the harassment ?
# We only allowed one response (not multiple responses) ** this was a big mistake!! 
# [harassment_who]	1	Someone I didn't know
# [harassment_who]	2	Someone I did know but isn't my family or relative
# [harassment_who]	3	Someone who is my relative but isn't part of my household
# [harassment_who]	4	Someone who is part of my household


harassment_who_vars <-
	survey_data %>%
	select(contains("harassment_who")) %>%
	names()

harassment_who_details <-
	survey_data %>%
	select(hh_id, contains("harassment_who")) %>%
	pivot_longer(col = contains("harassment_who"), names_to = "harassment_type", values_to = "perpetrator") %>%
	group_by(harassment_type, perpetrator) %>%
	summarise(count = n())

harassment_type_freq <-
	harassment_who_details %>%
	mutate(occurred = ifelse(!is.na(perpetrator), "harassed", NA)) %>%
	group_by(harassment_type, occurred) %>%
	summarise(count = sum(count)) %>%
	mutate(freq = count / sum(count) * 100)

write_csv(harassment_type_freq, here::here("C:/Users/admin/Desktop/harassment_type_freq.csv"))

harassment_type_freq_by_who <-
	harassment_who_details %>%
	filter(!is.na(perpetrator)) %>%
	mutate(freq = count / sum(count) * 100)


write_csv(harassment_type_freq_by_who, here::here("C:/Users/admin/Desktop/harassment_type_freq_by_who.csv"))




#########################################################################################################################
#########################################################################################################################


# Identify individuals to include in IDIs
## Vector of variables to summarize

harassment_physical_sexual_vars <-
	survey_data %>%
	select(ends_with("_w"), ends_with("_m"), ends_with("_g"), ends_with("_b")) %>%
	select(
		-c(
			cook_who_w, gather_scraps_w, collect_wood_w, buy_wood_w, receive_wood_w, receive_lpg_w, buy_lpg_w, receive_crh_w, buy_crh_w,
			cook_who_m, gather_scraps_m, collect_wood_m, buy_wood_m, receive_wood_m, receive_lpg_m, buy_lpg_m, receive_crh_m, buy_crh_m,
			cook_who_g, gather_scraps_g, collect_wood_g, buy_wood_g, receive_wood_g, receive_lpg_g, buy_lpg_g, receive_crh_g, buy_crh_g,
			cook_who_b, gather_scraps_b, collect_wood_b, buy_wood_b, receive_wood_b, receive_lpg_b, buy_lpg_b, receive_crh_b, buy_crh_b
		)
	) %>%
	names()

harassment_physical_sexual <- 
	survey_data %>%
	filter(consent == "OK") %>%
	select(hh_id, ends_with("_w"), ends_with("_m"), ends_with("_g"), ends_with("_b")) %>%
	select(
		-c(
			cook_who_w, gather_scraps_w, collect_wood_w, buy_wood_w, receive_wood_w, receive_lpg_w, buy_lpg_w, receive_crh_w, buy_crh_w,
			cook_who_m, gather_scraps_m, collect_wood_m, buy_wood_m, receive_wood_m, receive_lpg_m, buy_lpg_m, receive_crh_m, buy_crh_m,
			cook_who_g, gather_scraps_g, collect_wood_g, buy_wood_g, receive_wood_g, receive_lpg_g, buy_lpg_g, receive_crh_g, buy_crh_g,
			cook_who_b, gather_scraps_b, collect_wood_b, buy_wood_b, receive_wood_b, receive_lpg_b, buy_lpg_b, receive_crh_b, buy_crh_b
		)
	) %>%
	# replace all instances of "Never" with NA
	# Turned everything back into integers without labels
	naniar::replace_with_na_at(.vars = harassment_physical_sexual_vars, condition = ~.x == "Never") %>% # Replace "Never" (which was entered if a hh reported X happened but it didn't happen to the particular group w, m, g, b) with NA
	# when I use replace_with_na_all it influences the hh_id column and screws it up.
	select(-c(contains("insult"), contains("belittle"), contains("scare"), contains("push"))) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols")

# 14 women report being hit or worse  or sexual anything
harassment_physical_sexual_w <-
	harassment_physical_sexual %>%
	select(hh_id, ends_with("_w")) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols")



# 110 respondent reported men have been hit or worse or experience anything sexual
harassment_physical_sexual_m <-
	harassment_physical_sexual %>%
	select(hh_id, ends_with("_m")) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols")

# 2 respondent reported adolesent girls have been hit or worse or experience anything sexual
harassment_physical_sexual_g <-
	harassment_physical_sexual %>%
	select(hh_id, ends_with("_g")) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols")

# 21 respondent reported adolescent boys have been hit or worse or experience anything sexual
harassment_physical_sexual_b <-
	harassment_physical_sexual %>%
	select(hh_id, ends_with("_b")) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols")

# tidyr::replace_na(): Missing values turns into a value (NA –> -99)
# naniar::replace_with_na(): Value becomes a missing value (-99 –> NA)

harassment_physical_sexual_g_list <-
	harassment_physical_sexual_g %>%
	sample_n(size = 2)  %>%
	select(hh_id)
# There are only two girls
# 10G115604G26_3478, 10G115614G26_3517



## Identify more adolescent girls that faced harassment by including "push" - this only adds one girl (10D109224D11_2750) so then also add also allow for verbal abuse of "scare" -> this results in  10 girls

harassment_physical_sexual_g_extra_list <-
	survey_data %>%
	filter(consent == "OK") %>%
	select(hh_id, ends_with("_w"), ends_with("_m"), ends_with("_g"), ends_with("_b")) %>%
	select(
		-c(
			cook_who_w, gather_scraps_w, collect_wood_w, buy_wood_w, receive_wood_w, receive_lpg_w, buy_lpg_w, receive_crh_w, buy_crh_w,
			cook_who_m, gather_scraps_m, collect_wood_m, buy_wood_m, receive_wood_m, receive_lpg_m, buy_lpg_m, receive_crh_m, buy_crh_m,
			cook_who_g, gather_scraps_g, collect_wood_g, buy_wood_g, receive_wood_g, receive_lpg_g, buy_lpg_g, receive_crh_g, buy_crh_g,
			cook_who_b, gather_scraps_b, collect_wood_b, buy_wood_b, receive_wood_b, receive_lpg_b, buy_lpg_b, receive_crh_b, buy_crh_b
		)
	) %>%
	# replace all instances of "Never" with NA
	# Turned everything back into integers without labels
	naniar::replace_with_na_at(.vars = harassment_physical_sexual_vars, condition = ~.x == "Never") %>% # Replace "Never" (which was entered if a hh reported X happened but it didn't happen to the particular group w, m, g, b) with NA
	# when I use replace_with_na_all it influences the hh_id column and screws it up.
	select(-c(contains("insult"), contains("belittle"))) %>%
	# filter(hh_id %notin% c("10D109224D11_2750", "10G115604G26_3478", "10G115614G26_3517")) %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_list %>% pull(hh_id))) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols") %>%
	
	select(hh_id, ends_with("_g")) %>%
	filter_at(vars(-hh_id), any_vars(!is.na(.))) %>%
	janitor::remove_empty(which = "cols") %>%
	
	sample_n(size = 8)  %>%
	select(hh_id)


# There were only three adolescent girls who's mother reported that they were hit or worse. These were hh  10G115604G26_3478, 10G115614G26_3517. This girl was reported to be pushed 10D109224D11_2750. The two of seven ramdonly selected girls reported being scared are 8wB122150_1733, 3G186539Dd8_1975



## Identify adolescent boys for IDIs

harassment_physical_sexual_b_list <-
	harassment_physical_sexual_b %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_extra_list %>% pull(hh_id))) %>%
	sample_n(size = 10) %>%
	select(hh_id)
# 3E185415DD22_1424, 8wBI15102369_1104, 10G115604G26_3478, 9G123670G29_6347, 10D109334D11_2814


harassment_physical_sexual_w_list <-
	harassment_physical_sexual_w %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_extra_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_b_list %>% pull(hh_id))) %>%
	sample_n(size = 10) %>%
	select(hh_id)
#hh: 8wA117722A19_2130, 8wA115397_1528, 8wDI21101459, 8wBA20117719_, 9G600019G1_5663

harassment_physical_sexual_m_list <-
	harassment_physical_sexual_m %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_g_extra_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_b_list %>% pull(hh_id))) %>%
	filter(hh_id %notin% (harassment_physical_sexual_w_list %>% pull(hh_id))) %>%
	sample_n(size = 10) %>%
	select(hh_id)
# 8wB102745i14_2610, 10G110695G26_3480, 8wA117770A30_, 8wB115467A16_2421, 10D109334D11_2814


## All individuals for IDIs

harassment_physical_sexual_list <-
	bind_rows(
		harassment_physical_sexual_w_list,
		harassment_physical_sexual_m_list,
		harassment_physical_sexual_g_list,
		harassment_physical_sexual_g_extra_list,
		harassment_physical_sexual_b_list
	) %>%
	pull(hh_id)

harassment_physical_sexual_list_hh_detail <-
	survey_data %>%
	filter(hh_id %in% harassment_physical_sexual_list) %>%
	mutate(
		hh_id = 
			factor(hh_id, levels = harassment_physical_sexual_list)
	) %>%
	select(SubmissionDate, hh_id, camp_id, block_id, subblock_id, name_respondent, name_hh_head, name_mahji, fcn_id, hh_id, target_child_name) %>%
	arrange(hh_id) %>%
	mutate(
		target_respondent = c(rep("woman", 10), rep("man", 10), rep("girl", 10), rep("boy", 10))
	) %>%
	select(SubmissionDate,target_respondent, hh_id, everything())

write.csv(harassment_physical_sexual_list_hh_detail, "C:/Users/lenovo/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/HH selection/harassment_physical_sexual_list_hh_detail.csv")
