fig_monthly_expenditures_food_fuel <-	
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
          "spent_food",
          "buy_wood_cost"
          ),
        labels =
          c(
# all expenditures
            "  food  ",
            "  fuel  "
          )
      )
  ) %>%
  ungroup() %>%
  drop_na(category_label) %>% 
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
   # axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    legend.position = "bottom"
  ) + 
  guides(
    color = guide_legend(title = "Study arm"),
    shape = FALSE
  ) +
  labs(
  #  title = "Monthly household expenditure",
    x = "Expenditure category",
    y = "Monthly expenditure (USD)"
  ) + 
  # facet_wrap(~ category_label, ncol = 3) #, scales = "free_y"
  facet_grid(. ~ category_label, scales = "free_y") 
# facet_grid( ~ category, labeller = labeller(category = supp.labs)) # + # these only work with facet_wrap: , strip.position = "bottom", scales = "free_y"
# facet_grid(lpg_sufficiency_endline ~ category) + # fuel_30_purchased_fuel_endline
# coord_cartesian(ylim = c(0, 40))
