source(here::here("0_config.R"))

file_in_pm_data_clean_rds <- here::here("4_data/pm_data_clean.rds")

pm_data_full <- read_rds(file_in_pm_data_clean_rds) %>%
	mutate(date = as_date(date))

pm_data_full %>%
	filter(timepoint == "endline") %>%
	filter(hh_id == "10FF33107012") %>% 
	ggplot(aes(dateTime, PM_Estimate, color = hh_id)) +
	geom_line() + 
	scale_x_datetime(labels = scales::date_format("%Y-%m-%d")) + 
	theme(
		legend.position = "none"
	)
pm_data_full %>%
	filter(timepoint == "endline") %>%
	filter(hh_id == "4EPP10280794") %>%
	# filter(dateTime > as_datetime("2022-04-20"), dateTime < as_datetime("2022-04-21")) %>%
	ggplot(aes(dateTime, PM_Estimate, color = hh_id)) +
	geom_line() + 
	scale_x_datetime(labels = scales::date_format("%y-%m-%d %H:%M")) + 
	theme(
		legend.position = "none"
	)



