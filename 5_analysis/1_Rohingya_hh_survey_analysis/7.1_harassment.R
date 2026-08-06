##################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: harassment analysis
# @Date: 220511
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
#### Input Files ####
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

# Number each demographic that collected each fuel at midline, when we also asked about a perpetrator
# I will use for the denominator in the  harassment analysis
file_in_1 <-here::here("4_data/RohingyaFuel_fuel_procurement_who_midline.csv") 


#### Output Files ####

file_out_1a <- here::here("4_data/harassment_analysis.rds")
file_out_1b <- here::here("4_data/harassment_analysis.csv")
file_out_1c <- here::here("7_tables/harassment_table.csv")

# file_out_2 <- here::here ("4_data/harassment_freq_prev.rds")
# file_out_3 <- here::here("6_figures/barplot_sexual_harassment.png")


#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)
# Number of each demographic that collected fuel type X
fuel_procurement_who_baseline <- read_csv(file_in_1) %>% select(-"...1")

# ============================================================================

# Asked at baseline - DO NOT USE
# Has anyone insulted you or any of your family members or made them feel bad about him/herself? insult_hh
# Since we didn't specify ****Since arriving at camp *** we decided to drop the results from baseline
# Did this happen when the person was gathering scraps/leaves/twigs for fuel? gather_scraps_insult_hh
# How many times has this happened to….? gather_scraps_insult_w
# At baseline, the frequency was selected from frequency_times:
# 	0	Never
# 1	Once
# 2	Twice
# 3	Three times
# 4	More than three times
# 

# Asked at endline
# Since arriving at the camp, has anyone insulted you or any of your family members or made them feel bad about him/herself? insult_hh_ever
# Who caused this negative experience? harassment_insult_who
# Did this happen when the person was gathering scraps/leaves/twigs for fuel? gather_scraps_insult_hh_ever
# How many times has this happened to….? gather_scraps_insult_w_ever
# At endline, we asked for an integer number of times of harassment

# ============================================================================

# We want to know how many of each demographic was harassed every when collecting fuel type X
# We can also get the distribution for the number of times harassed when harassed




###############################################################################
## Get the names of the variables associated with harassment
###############################################################################

# These are the variables from the midline survey related to harassment
harassment_vars <-
  survey_data %>%
  select(
    contains(c("w_ever", "g_ever", "m_ever", "b_ever")) & 
      (contains(c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon", "sex_lang", "sex_contact", "sex_rumor", "clothing_pull", "sex_corner")))
  ) %>% 
  names()

harassment_base <- 
  survey_data %>%
  select(study_arm_overall, timepoint, fcn_id, all_of(harassment_vars)) %>%
  pivot_longer( 
    cols = c(
      all_of(harassment_vars)
    ),
    names_to = "fuel_type_harassment_demographic_ever",
    values_to = "times_harassed"
  ) %>%
  # some harassment have multiple words so will need to use a regex to extract fuel type, w, and ever and the remaining is the harassment
  # separate(col = "fuel_type_harassment_sex_ever", into = c("fuel_how", "fuel", "harassment", "sex", "ever"), sep = "_") %>%
  mutate(
    fuel_type = str_extract(fuel_type_harassment_demographic_ever, pattern = "^[^_]*_[^_]*"), # has an extra und
    demographic_ever = str_extract(fuel_type_harassment_demographic_ever, pattern = "([^_]*_[^_]*)$"),
    # there is definitely an easier regexp to get what comes after the second underscore and before the penultimate underscore but the below works
    harassment_type = 
      str_remove(
        str_remove(
          str_remove(
            str_remove(fuel_type_harassment_demographic_ever, pattern = "^[^_]*_[^_]*"), 
            pattern = "([^_]*_[^_]*)$"), 
          pattern = "^[._]+"),
        pattern = "[._]$"
      )
  ) %>%
  separate("demographic_ever", into = c("demographic", "ever")) %>%
  select(-study_arm_overall, -timepoint, -fuel_type_harassment_demographic_ever, -ever) %>%
  mutate(harassed_yn = ifelse(times_harassed > 0, 1, 0))

## Get summary table
harassment <-
  harassment_base %>%
  group_by(fuel_type, harassment_type, demographic) %>%
  summarise(
    num_people_harassed = sum(harassed_yn, na.rm = TRUE),
    mean_times_harassed = mean(times_harassed, na.rm = TRUE)
  ) %>%
  left_join(fuel_procurement_who_baseline %>% select(-pc), by = c("fuel_type", "demographic")) %>%
  rename(num_people_collect_fuel = n) %>%
  rowwise() %>% # use rowwise instead of lapply
  mutate(
    pc_harassed = num_people_harassed/num_people_collect_fuel * 100,
    # # prop.test uses wilson score metho, which is more accurate than wald score
    # 1-sample proportions test without continuity correction
    # # Confidence interval for a single proportion
    # prop.test(x=120, n=180, correct=FALSE)$conf.int
    # 
    # # Two-sided test that the true proportion is different from 120/180 = 0.6
    # prop.test(x=120, n=180, p=0.6, correct=FALSE)
    lower = ifelse(num_people_harassed > 0, prop.test(num_people_harassed, num_people_collect_fuel, correct = FALSE)$conf.int[1] * 100, NA),
    upper = ifelse(num_people_harassed > 0, prop.test(num_people_harassed, num_people_collect_fuel, correct = FALSE)$conf.int[2] * 100, NA)
  ) %>%
  ungroup() %>%
  mutate(    
    harassment_type =
      ordered(
        harassment_type,
        levels = 
          c(
            "insult", "belittle", "scare", 
            "push", "hit", "kick", 
            "choke", "weapon", "sex_lang", 
            "sex_contact", "sex_rumor", 
            "clothing_pull", "sex_corner")
      ),
    
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
            "verbal", 
            "verbal", 
            "verbal",  
            "physical",
            "physical",
            "physical",
            "physical",
            "physical",
            "sexual", 
            "sexual", 
            "sexual",
            "sexual", 
            "sexual"
          )
      )
  ) %>%
  select(fuel_type, harassment_category, harassment_type, demographic, num_people_harassed, num_people_collect_fuel, pc_harassed, lower, upper, mean_times_harassed) %>%
  arrange(desc(num_people_harassed)) 

saveRDS(harassment, file_out_1a)
write.csv(harassment, file_out_1b)


################################################################################
# Creat a table of the harassment results
################################################################################
# Percent of people that received verbal, physical, or sexual harassment
# Recall that each person could report one or more forms of harassment, 
# so adding up the number of people who reported X harassment does not tell us the unique number
# The denominator of num_people_harassed is also only the number of people and doesn't represent the number of trips, etc. 
# (perhaps a boy takes a trip once a month but the hh still reports as "collecting" fuel whereas the man goes twice a week)

# So what is a better way to report this?  The highest prevalence of harassment by type, not category?

harassment_table <-
  harassment %>%
  mutate(
    pc_harassed_rounded = round(pc_harassed, 2),
    lower_rounded = round(lower, 2),
    upper_rounded = round(upper, 2)
  ) %>%
  pivot_wider(
    id_cols = c("fuel_type", "harassment_category", "harassment_type"),
    names_from = "demographic",
    values_from = c("num_people_harassed", "num_people_collect_fuel", "pc_harassed_rounded", "lower_rounded", "upper_rounded")
  ) %>% 
  mutate(
    fuel_type_label =
      ordered(
        fuel_type,
        levels = c(
          "gather_scraps", "collect_wood", "buy_wood", "receive_wood",
          "buy_crh", "receive_crh", 
          "buy_lpg", "receive_lpg"
        ),
        labels = c(
          "Scraps, gathered", "Wood, harvested", "Wood, purchased", "Wood, provided as aid",
          "Compressed rice husks, purchased", "Compressed rice husks, provided as aid", 
          "LPG, purchased", "LPG, provided as aid"
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
            "clothing_pull", "sex_corner"),
        labels = 
          c(
            "insulted", "belittled", "scared", 
            "pushed", "hit", "kicked", "choked", "threatened with a weapon", 
            "subject to sexual language", 
            "brushed by sexually", 
            "subject to sexual rumors", 
            "clothing pulled in a sexual way", 
            "cornered in a sexual way"
          )
      )
  ) %>%
  select(fuel_type_label, harassment_category, harassment_type_label, contains("_m"), contains("_w"), contains("_b"), contains("_g")) %>%
  arrange(fuel_type_label, harassment_category, harassment_type_label)

write.csv(harassment_table, file_out_1c)

# 494+437 # total 931 hh
# 49/931 #5.2% of hh had a girl collect wood



################################################################################
# Make figure
################################################################################

ann_text_gather_scraps <-
  data.frame(
    fuel_type_label = "Gather scraps", demographic_label = "Girls", harassment_category_label = "verbal (insulted, belittled, scared)", 
    harassment_type_label = "belittled", pc_harassed = 0.46, lower = 0, upper = 0,   
    label = "Gathering scraps\n(m = 687, w = 152, b = 163, g = 92)"
  ) %>%
  mutate(
    harassment_category_label =
      ordered(
        harassment_category_label,
        levels =
          c(
            "verbal (insulted, belittled, scared)",  
            "physical (pushed, hit, kicked, choked, faced a \nweapon)", 
            "sexual (language, rumors, brush past, clothing pulled, cornered)" 
          )
      )
  )

ann_text_collect_wood <- 
  data.frame(
    fuel_type_label = "Collect wood", demographic_label = "Boys", harassment_category_label = "verbal (insulted, belittled, scared)",
    harassment_type_label = "belittled", pc_harassed = 0.46, lower = 0, upper = 0,    
    label = "Collecting wood\n(m = 760, w = 36, b = 152, g = 49)"
  ) %>%
  mutate(
    harassment_category_label =
      ordered(
        harassment_category_label,
        levels =
          c(
            "verbal (insulted, belittled, scared)",  
            "physical (pushed, hit, kicked, choked, faced a \nweapon)", 
            "sexual (language, rumors, brush past, clothing pulled, cornered)" 
          )
      )
  )

ann_text_receive_lpg <- 
  data.frame(
    fuel_type_label = "Receive LPG", demographic_label = "Women", harassment_category_label = "verbal (insulted, belittled, scared)",
    harassment_type_label = "belittled", pc_harassed = 0.46, lower = 0, upper = 0,    
    label = "Receiving LPG\n(m = 797, w = 477, b = 65, g = 6)"
  ) %>%
  mutate(
    harassment_category_label =
      ordered(
        harassment_category_label,
        levels =
          c(
            "verbal (insulted, belittled, scared)",  
            "physical (pushed, hit, kicked, choked, faced a \nweapon)", 
            "sexual (language, rumors, brush past, clothing pulled, cornered)" 
          )
      )
  )


fig_harassment <- 
  harassment %>%
  # Remove harassment related to sex becuase there were so few reports.
  filter(
    harassment_type %in% 
      c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon")
  ) %>%
  
  filter(str_detect(fuel_type, pattern = 'gather_scrap|collect_wood|receive_lpg')) %>% # Can't have a " " around "|"
  
  # beautify names
  mutate(
    fuel_type_label =
      ordered(
        fuel_type,
        levels = c("gather_scraps", "collect_wood", "receive_lpg"),
        labels = c("Gather scraps", "Collect wood", "Receive LPG")
      ),
    harassment_category_label =
      ordered(
        harassment_type,
        levels =
          c("verbal", "physical", "sexual"),
        labels = 
          c(
            "verbal (insulted, belittled, scared)",
            "physical (pushed, hit, kicked, choked, faced a \nweapon)",
            "sexual (language, rumors, brush past, clothing pulled, cornered)"
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
          ),
        labels = 
          c(
            "insulted", "belittled", "scared", 
            "pushed", "hit/kicked", "hit/kicked", "choked/\nfaced a \nweapon", "choked/\nfaced a \nweapon", 
            "subject to \nsexual \nlanguage/ \nrumors", 
            "brushed by \nsexually /\nclothing \npulled", 
            "subject to \nsexual \nlanguage/ \nrumors", 
            "brushed by \nsexually /\nclothing \npulled", 
            "cornered in a \nsexual way"
          )
      ),
    demographic_label = 
      ordered(
        demographic,
        levels = c("m", "w", "b", "g"),
        labels = c("Men", "Women", "Boys", "Girls")
      )
  ) %>%
  select(fuel_type_label, demographic_label, harassment_category_label, harassment_type_label, pc_harassed, lower, upper) %>%
  # arrange(desc(times_occurred))
  group_by(fuel_type_label, harassment_type_label, demographic_label) %>%
  ggplot(aes(x = harassment_type_label, y = pc_harassed, fill = demographic_label)) + # 
  geom_col(position = position_dodge(0.9)) + 
  geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(0.9)) +
  
  # # to make sure the annotations didn't mess up the order of the plot, its required at add the order of the factors
  # # https://stackoverflow.com/questions/66693267/re-ordering-ordered-plots-after-adding-annotation-in-ggplot
  geom_text(data = ann_text_gather_scraps, aes(label = label), size = 3.5) +
  geom_text(data = ann_text_collect_wood, aes(label = label), size = 3.5) +
  geom_text(data = ann_text_receive_lpg, aes(label = label), size = 3.5) +
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
    # # Need to create space at the top for the annotations
    title = "",
    subtitle = "",
    # title = "Harassment experienced while collecting fuel",
    # subtitle = "Gathering scraps                                          Collecting firewood                                          Receiving LPG",
    x = "Type of harassment", 
    y = "Percentage of individuals who experienced harassment \nwhile collecting fuel"
  ) +
  # Needed so the annotations can be outside the plotting area
  coord_cartesian(ylim = c(0, 0.43), expand = F, clip = "off") +
  facet_wrap(harassment_category_label ~ fuel_type_label, ncol = 3, scales = "free") # facet_grid doesn't work - puts all the harassment types on the bottom, not sure why; facet_grid can use a space agrument, but facet_wrap cannot; space = "free". Facet_wrap can use ,   strip.position = "right"

fig_harassment


ggsave(
  here::here("6_figures", "fig_harassment.png"),
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


df_harassment_who_long <-
  df_harassment_base %>%
  select(timepoint, study_arm_overall, fcn_id, contains("harassment_who")) %>%
  pivot_longer(
    cols = harassment_who_insult:harassment_who_clothing_pull, 
    names_to = c("harassment_who", "harassment_type"), 
    names_pattern = "(.{10}_.{3})_(.*_*.*)", 
    # names_pattern = "(.*_.*)_([^_]+$)", # The parentheses are used to indicate group 1 and group 2
    values_to = "perpetrator"
  ) # %>%
# select(-harassment_who)


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
####### Identify individuals to include in IDIs ###################
#########################################################################################################################

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

# write.csv(harassment_physical_sexual_list_hh_detail, "C:/Users/lenovo/Google Drive (lakwong@stanford.edu)/Rohingya/Rohingya research - Fuel/HH selection/harassment_physical_sexual_list_hh_detail.csv")



