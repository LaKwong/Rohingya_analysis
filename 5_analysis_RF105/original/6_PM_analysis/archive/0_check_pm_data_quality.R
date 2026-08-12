################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: visualize PM quality control
# @Version: xxx
# @Date: 220703
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

################################################################################

#Input Files

# the pm_data_base file has been censored at the top and has values inserted for <10 but still includes the insufficient data (which is necessary because some QC files had insufficient data)
# qc files with insufficient data: 4FUU14296441, 10FF31201201

file_in_pm_data_base_rds <- here::here("4_data/pm_data_base.rds") 
file_in_pm_data_qc_rds <- here::here("4_data/pm_data_qc.rds")

### Output Files RDS 

# XXXXX

################################################################################


pm_data <- read_rds(file_in_pm_data_base_rds)
pm_data_qc <- read_rds(file_in_pm_data_qc_rds)
<- 
pm_data_qc %>%
	filter(timepoint == "endline")

# baseline
# timepoint hh_id       
# <ord>     <chr>       
# 	1 baseline  8wDI18224646 qc2
# 2 baseline  8wDI20101777   qc2
# 3 baseline  8wDI20274944   qc2
# 4 baseline  10D193579   qc1
# 5 baseline  10G105368   qc1
# 6 baseline  10G105460   qc1
# 7 baseline  10G108169   qc1
# 8 baseline  10G116129   qc1
# 9 baseline  10F113975   qc1
# 10 baseline  9G600021    qc2
# 11 baseline  10H286043   qc2
# 12 baseline  4E280792    qc3
# 13 baseline  4E277017    qc3
# 14 baseline  4E179247    qc3
# 15 baseline  4F283027    qc3
# 16 baseline  4F180644    qc3
# 17 baseline  4F296441    qc3
# 18 baseline  4E179624    qc2
################################ 

# midline
# timepoint hh_id       
# <ord>     <chr>       
# 	1 midline   8WDI18224646
# 2 midline   8WDH21290495
# 3 midline   8WDI21101595
# 4 midline   10D12193591 
# 5 midline   10F40201619 
# 6 midline   4EPP10188920
# 7 midline   4EPP11277068
# 8 midline   4EPP12283062
# 9 midline   4EPP15179029
################################ 



# endline
# timepoint hh_id       
# <ord>     <chr>       
# 	1 endline   8WDI18224646
# 2 endline   8WDH21291234
# 3 endline   8WDI20101574
# 4 endline   10DD12193579
# 5 endline   10GG38105460
# 6 endline   10GG09111931
# 7 endline   10FF34105441
# 8 endline   10FF31201201
# 9 endline   10FF40201613
# 10 endline   10FF16106528
# 11 endline   10HH55285946
# 12 endline   9GG29123670 
# 13 endline   9GG29123845 
# 14 endline   4EPP15152794
# 15 endline   4EPP12188379
# 16 endline   4FUU14296441
# 17 endline   4EPP13184510
# 18 endline   4FUU16283055
# 19 endline   4EPP13179599









# need to do the QC files for midline and endline - there are 39 files total

####################################



qc1_baseline_files <-
	c(
		"10D193579_normal", "10D193579_qc",
		"10F113975_normal", "10F113975_qc",
		"10G105368_normal", "10G105368_qc",
		"10G105460_normal", "10G105460_qc",
		"10G108169_normal", "10G108169_qc",
		"10G116129_normal", "10G116129_qc"
	)

qc2_baseline_files <-
c(				"10H286043_normal", "10H286043_qc",
			"9G600021_normal", "9G600021_qc",
			"8wDI18224646_normal", "8wDI18224646_qc",
			"8wDI20101777_normal", "8wDI20101777_qc",
			"8wDI20274944_normal", "8wDI20274944_qc",
			"4E179624_normal", "4E179624_qc"
			)	


qc3_baseline_files <-
	c(
		"4E280792_normal", "4E280792_qc",
		"4E277017_normal", "4E277017_qc",
		"4E179247_normal", "4E179247_qc",
		"4F283027_normal", "4F283027_qc",
		"4F180644_normal", "4F180644_qc",
		"4F296441_normal", "4F296441_qc"
	)

# was previously in baseline, but not on 220703
# "4E280792_normal", "4E280792_qc",


qc4_midline_files <-
	c(
		"8WDI18224646_normal", "8WDI18224646_qc",
		"8WDH21290495_normal", "8WDH21290495_qc",
		"8WDI21101595_normal", "8WDI21101595_qc",
		"10D12193591_normal", "10D12193591_qc",
		"10F40201619_normal", "10F40201619_qc",
		"4EPP10188920_normal", "4EPP10188920_qc"
	)

qc5_midline_files <-
	c(
		"4EPP11277068_normal", "4EPP11277068_qc",
		"4EPP12283062_normal", "4EPP12283062_qc",
		"4EPP15179029_normal", "4EPP15179029_qc"
	)

qc6_endline_files <-
	c(
		"8WDI18224646_normal", "8WDI18224646_qc",
		"8WDH21291234_normal", "8WDH21291234_qc",
		"8WDI20101574_normal", "8WDI20101574_qc",
		"9GG29123670_normal", "9GG29123670_qc",
		"9GG29123845_normal", "9GG29123845_qc"
	)

qc7_endline_files <-
	c(
		"10DD12193579_normal", "10DD12193579_qc",
		"10GG38105460_normal", "10GG38105460_qc",
		"10GG09111931_normal", "10GG09111931_qc",
		"10FF34105441_normal", "10FF34105441_qc",
		"10FF31201201_normal", "10FF31201201_qc",
		"10FF40201613_normal", "10FF40201613_qc"
	)

qc8_endline_files <-
	c(
		"10FF16106528_normal", "10FF16106528_qc",
		"10HH55285946_normal", "10HH55285946_qc"
	)

qc9_endline_files <-
	c(
		"4EPP15152794_normal", "4EPP15152794_qc",
		"4EPP12188379_normal", "4EPP12188379_qc",
		"4FUU14296441_normal", "4FUU14296441_qc",
		"4EPP13184510_normal", "4EPP13184510_qc",
		"4FUU16283055_normal", "4FUU16283055_qc",
		"4EPP13179599_normal", "4EPP13179599_qc"
	)
# Compare quality control and normal files
qc_check_fcn <- function(qc_files, study_timepoint){
	pm_data %>%
		filter(
			hh_id_note %in% qc_files,
			timepoint == study_timepoint
		) %>% 
		ggplot(aes(dateTime_min, PM_Estimate, color = PM_monitor)) + # hh_id_note
		geom_line(na.rm = TRUE) +
		## shows the values overlapping
		# geom_text(aes(as_datetime(2020-01-01), 20000, label = paste("ID: ", PM_monitor))) +
		# geom_hline(yintercept = 100, color = "grey", lty = 2) + 
		# geom_hline(yintercept = 500, color = "black", lty = 2) +
		scale_x_datetime(labels = scales::date_format("%Y-%m-%d %H:%M")) + # labels = scales::date_format("%H:%M")
		scale_color_brewer(palette = "Paired") + # Unsuprisingly, palette = "Paired" is probably the best, max size is 12
		# Max size of palette = Accent is 8, Set1 is 9, of Set2 is 8, of Set3 is 10?
		# The palette = "Accent" or "Set[x]" worked better then the type = "divergent"
		# scale_color_brewer(type = "div", palette = "Spectral") + 
		theme_classic() +
		# theme(
		# 	legend.position = "none"
		# ) +
		labs(
			title = "Quality control comparison",
			subtitle = study_timepoint,
			y = "PM 2.5 (ug/m3)",
			x = "Time"
		) +
		facet_wrap(. ~ hh_id, ncol = 3, scales = "free") 
}
# facet_grid splits ONLY on the horizontal or vertical direction while facet_wrap only has a horizontal dimension
# facet_grid(vertical ~ horizontal) - plots on the same row cannot have different y-axes, similarly, only one x-axis per column
# facet_wrap only stacks plots horizontally - each plot is indep so can each can have diff x and y axes


qc_check_fcn(qc1_baseline_files, "baseline")
qc_check_fcn(qc2_baseline_files, "baseline")
qc_check_fcn(qc3_baseline_files, "baseline") #PM_monitor PM07781I in 4F296441 seems high
qc_check_fcn(qc4_midline_files, "midline")
qc_check_fcn(qc5_midline_files, "midline") # PM97768X in 4EPP12283062 high peaks are much to high (2200 instead of 100; 3000 instead of 600) PM_monitor PM07767U in 4EPP15179029 are slightly higher (eg 400 vs 250 is max diff) low
qc_check_fcn(qc6_endline_files, "endline") # 9GG29123670_normal (PM_monitors == PM07750M and PM07751N do not appear to overlap in time at all) - really qc file? ; # 8WDI20101574_normal PM_monitor PM07762G spikes to 4000 instead of 2000
qc_check_fcn(qc7_endline_files, "endline") # 10FF31201201_normal doesn't appear to have a pair; PM07765K is consistently low
qc_check_fcn(qc8_endline_files, "endline") 
qc_check_fcn(qc9_endline_files, "endline") # 4FUU14296441_normal doesn't appear to have a pair because the pair had 

pm_data %>% 
	filter(hh_id == "9GG29123670") %>%
	distinct(note, date)

pm_data %>% 
	filter(study_arm == "outdoor") %>%
	select(timepoint, hh_id, hh_id_note) %>%
	unique()

# 	timepoint hh_id  hh_id_note   
# <ord>     <chr>  <chr>        
# 	1 baseline  8WI18  8WI18_mosque 
# 2 baseline  10GG9  10GG9_mosque 
# 3 baseline  4EPP11 4EPP11_school
# 4 midline   8WI18  8WI18_mosque 
# 5 midline   10GG9  10GG9_mosque 
# 6 midline   4EPP11 4EPP11_school
# 7 endline   8WI18  8WI18_mosque 
# 8 endline   10DD12 10DD12_mosque # new location for endline
# 9 endline   10GG09 10GG09_mosque  # should be 10GG9
# 10 endline   10GG9  10GG9_mosque 
# 11 endline   4EPP11 4EPP11_school









# ggsave(
# 	here::here("docs", "qc_check_3.png"),
# 	plot = last_plot(),
# 	scale = 0.8,
# 	height = 6,
# 	width = 10,
# 	units = "in",
# 	device = "png"
# )

