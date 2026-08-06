# coalesce function  with var names that have slashes.


# Function to coalese columns
survey_data_coalesce_fcn <- function(df){
	df %>%
		select(
			-starts_with("generated_note"),
			-starts_with("reserved_name")
		) %>%
		mutate_at(
			vars(camp_id, block_id, subblock_id, fcn_id),
			funs(str_to_lower(str_replace_all(., fixed(" "), "")))
		) %>%
		# select(camp_id, block_id, subblock_id, fcn_id, Rand_nu, hh_id, hh_id_original, name_respondent, consent) %>%
		# arrange(fcn_id) %>%
		# View()
		mutate(
			org =
				case_when(
					camp_id %in% c("3", "4", "5") ~ "UNHCR",
					camp_id %in% c("8w", "8e", "9", "10", "18") ~ "IOM"
				)
		) %>%
		
		mutate_at(
			vars(
				starts_with("rice_source"),
				starts_with("bread_source"),
				starts_with("corn_source"),
				starts_with("potatoes_source"),
				starts_with("lentils_source"),
				starts_with("eggs_source"),
				starts_with("dairy_source"),
				starts_with("veggies_source"),
				starts_with("fruit_source"),
				starts_with("fish_source"),
				starts_with("poultry_source"),
				starts_with("goat_sheep_source"),
				starts_with("oil_source"),
				starts_with("sugar_source"),
				starts_with("food_cant_afford_action"),
				starts_with("fuel_cant_afford_action"),
				starts_with("reduce_meals"),
				starts_with("not_eat"),
				starts_with("gather_scraps_dead")
			),
			list(as.numeric)
		) %>%
		mutate(
			# reduce_meals1 = coalesce(reduce_meals1, reduce_meals.1, reduce_meals_1),
			
			# not_eat1 = coalesce(as.numeric(not_eat1), as.numeric(not_eat.1), as.numeric(not_eat_1)),
			
			
			
			
			## "Are the scraps/leaves/twigs you collect all dead?" has the variable gather_scraps_dead 
			## "Is all the wood you collect all dead?" accidentally has the variable gather_scraps_dead1
			gather_wood_dead = coalesce(gather_scraps_dead1, gather_scraps_dead.1), # this is actually gather_wood_dead
			
			time_child_gathering_nonwood_items = coalesce(time_child_gathering_nonwood_ite, time_child_gathering_non.wood_items), # `time_child_gathering_non-wood_items` (this was removed when importing with read_csv(name_repair = "universal") )
			# # "time_child_gathering_nonwood_ite" was misspelled - missng the "ms" at the end
			# # coalesce isn't working becuase everything in time_child_gathering_nonwood_ite is NA
			# instead just rename the variable that does have the data (see survey_data_2)
			
			lpg_changes_lifestyle1 = coalesce(lpg_changes_lifestyle1, lpg_changes_lifestyle.1, `lpg_changes_lifestyle/1`),
			lpg_changes_lifestyle2 = coalesce(lpg_changes_lifestyle2, lpg_changes_lifestyle.2, `lpg_changes_lifestyle/2`),
			lpg_changes_lifestyle3 = coalesce(lpg_changes_lifestyle3, lpg_changes_lifestyle.3, `lpg_changes_lifestyle/3`),
			lpg_changes_lifestyle4 = coalesce(lpg_changes_lifestyle4, lpg_changes_lifestyle.4, `lpg_changes_lifestyle/4`),
			lpg_changes_lifestyle5 = coalesce(lpg_changes_lifestyle5, lpg_changes_lifestyle.5, `lpg_changes_lifestyle/5`),
			lpg_changes_lifestyle6 = coalesce(lpg_changes_lifestyle6, lpg_changes_lifestyle.6, `lpg_changes_lifestyle/6`),
			
			food_cant_afford_action1 = coalesce(food_cant_afford_action1, food_cant_afford_action.1, `food_cant_afford_action/1`),
			food_cant_afford_action2 = coalesce(food_cant_afford_action2, food_cant_afford_action.2, `food_cant_afford_action/2`),
			food_cant_afford_action3 = coalesce(food_cant_afford_action3, food_cant_afford_action.3, `food_cant_afford_action/3`),
			food_cant_afford_action4 = coalesce(food_cant_afford_action4, food_cant_afford_action.4, `food_cant_afford_action/4`),
			food_cant_afford_action5 = coalesce(food_cant_afford_action5, food_cant_afford_action.5, `food_cant_afford_action/5`),
			food_cant_afford_action6 = coalesce(food_cant_afford_action6, food_cant_afford_action.6, `food_cant_afford_action/6`),
			food_cant_afford_action7 = coalesce(food_cant_afford_action7, food_cant_afford_action.7, `food_cant_afford_action/7`),
			food_cant_afford_action8 = coalesce(food_cant_afford_action8, food_cant_afford_action.8, `food_cant_afford_action/8`),
			food_cant_afford_action9 = coalesce(food_cant_afford_action9, food_cant_afford_action.9, `food_cant_afford_action/9`),
			food_cant_afford_action10 = coalesce(food_cant_afford_action10, food_cant_afford_action.10, `food_cant_afford_action/10`),
			food_cant_afford_action11 = coalesce(food_cant_afford_action11, food_cant_afford_action.11, `food_cant_afford_action/11`),
			food_cant_afford_action12 = coalesce(food_cant_afford_action12, food_cant_afford_action.12, `food_cant_afford_action/12`),
			food_cant_afford_action13 = coalesce(food_cant_afford_action13, food_cant_afford_action.13, `food_cant_afford_action/13`),
			food_cant_afford_action66 = coalesce(food_cant_afford_action66, food_cant_afford_action.66, `food_cant_afford_action/66`),
			food_cant_afford_action88 = coalesce(food_cant_afford_action88, food_cant_afford_action.88, `food_cant_afford_action/88`),
			
			fuel_cant_afford_action1 = coalesce(fuel_cant_afford_action1, fuel_cant_afford_action.1, `fuel_cant_afford_action/1`),
			fuel_cant_afford_action2 = coalesce(fuel_cant_afford_action2, fuel_cant_afford_action.2, `fuel_cant_afford_action/2`),
			fuel_cant_afford_action3 = coalesce(fuel_cant_afford_action3, fuel_cant_afford_action.3, `fuel_cant_afford_action/3`),
			fuel_cant_afford_action4 = coalesce(fuel_cant_afford_action4, fuel_cant_afford_action.4, `fuel_cant_afford_action/4`),
			fuel_cant_afford_action5 = coalesce(fuel_cant_afford_action5, fuel_cant_afford_action.5, `fuel_cant_afford_action/5`),
			fuel_cant_afford_action6 = coalesce(fuel_cant_afford_action6, fuel_cant_afford_action.6, `fuel_cant_afford_action/6`),
			fuel_cant_afford_action7 = coalesce(fuel_cant_afford_action7, fuel_cant_afford_action.7, `fuel_cant_afford_action/7`),
			fuel_cant_afford_action8 = coalesce(fuel_cant_afford_action8, fuel_cant_afford_action.8, `fuel_cant_afford_action/8`),
			fuel_cant_afford_action9 = coalesce(fuel_cant_afford_action9, fuel_cant_afford_action.9, `fuel_cant_afford_action/9`),
			fuel_cant_afford_action10 = coalesce(fuel_cant_afford_action10, fuel_cant_afford_action.10, `fuel_cant_afford_action/10`),
			fuel_cant_afford_action11 = coalesce(fuel_cant_afford_action11, fuel_cant_afford_action.11, `fuel_cant_afford_action/11`),
			fuel_cant_afford_action12 = coalesce(fuel_cant_afford_action12, fuel_cant_afford_action.12, `fuel_cant_afford_action/12`),
			fuel_cant_afford_action13 = coalesce(fuel_cant_afford_action13, fuel_cant_afford_action.13, `fuel_cant_afford_action/13`),
			fuel_cant_afford_action66 = coalesce(fuel_cant_afford_action66, fuel_cant_afford_action.66, `fuel_cant_afford_action/66`),
			fuel_cant_afford_action88 = coalesce(fuel_cant_afford_action88, fuel_cant_afford_action.88, `fuel_cant_afford_action/88`),
			
			# baseline had rice_source.0 but endline has rice_source/0
			
			rice_source0 = coalesce(rice_source0, rice_source.0, `rice_source/0`),
			rice_source1 = coalesce(rice_source1, rice_source.1, `rice_source/1`),
			rice_source2 = coalesce(rice_source2, rice_source.2, `rice_source/2`),
			rice_source3 = coalesce(rice_source3, rice_source.3, `rice_source/3`),
			rice_source4 = coalesce(rice_source4, rice_source.4, `rice_source/4`),
			rice_source5 = coalesce(rice_source5, rice_source.5, `rice_source/5`),
			rice_source6 = coalesce(rice_source6, rice_source.6, `rice_source/6`),
			rice_source7 = coalesce(rice_source7, rice_source.7, `rice_source/7`),
			rice_source8 = coalesce(rice_source8, rice_source.8, `rice_source/8`),
			rice_source9 = coalesce(rice_source9, rice_source.9, `rice_source/9`),
			rice_source66 = coalesce(rice_source66, rice_source.66, `rice_source/66`),
			
			bread_source0 = coalesce(bread_source0, bread_source.0, `bread_source/0`),
			bread_source1 = coalesce(bread_source1, bread_source.1, `bread_source/1`),
			bread_source2 = coalesce(bread_source2, bread_source.2, `bread_source/2`),
			bread_source3 = coalesce(bread_source3, bread_source.3, `bread_source/3`),
			bread_source4 = coalesce(bread_source4, bread_source.4, `bread_source/4`),
			bread_source5 = coalesce(bread_source5, bread_source.5, `bread_source/5`),
			bread_source6 = coalesce(bread_source6, bread_source.6, `bread_source/6`),
			bread_source7 = coalesce(bread_source7, bread_source.7, `bread_source/7`),
			bread_source8 = coalesce(bread_source8, bread_source.8, `bread_source/8`),
			bread_source9 = coalesce(bread_source9, bread_source.9, `bread_source/9`),
			bread_source66 = coalesce(bread_source66, bread_source.66, `bread_source/66`),
			
			corn_source0 = coalesce(corn_source0, corn_source.0, `corn_source/0`),
			corn_source1 = coalesce(corn_source1, corn_source.1, `corn_source/1`),
			corn_source2 = coalesce(corn_source2, corn_source.2, `corn_source/2`),
			corn_source3 = coalesce(corn_source3, corn_source.3, `corn_source/3`),
			corn_source4 = coalesce(corn_source4, corn_source.4, `corn_source/4`),
			corn_source5 = coalesce(corn_source5, corn_source.5, `corn_source/5`),
			corn_source6 = coalesce(corn_source6, corn_source.6, `corn_source/6`),
			corn_source7 = coalesce(corn_source7, corn_source.7, `corn_source/7`),
			corn_source8 = coalesce(corn_source8, corn_source.8, `corn_source/8`),
			corn_source9 = coalesce(corn_source9, corn_source.9, `corn_source/9`),
			corn_source66 = coalesce(corn_source66, corn_source.66, `corn_source/66`),
			
			potatoes_source0 = coalesce(potatoes_source0, potatoes_source.0, `potatoes_source/0`),
			potatoes_source1 = coalesce(potatoes_source1, potatoes_source.1, `potatoes_source/1`),
			potatoes_source2 = coalesce(potatoes_source2, potatoes_source.2, `potatoes_source/2`),
			potatoes_source3 = coalesce(potatoes_source3, potatoes_source.3, `potatoes_source/3`),
			potatoes_source4 = coalesce(potatoes_source4, potatoes_source.4, `potatoes_source/4`),
			potatoes_source5 = coalesce(potatoes_source5, potatoes_source.5, `potatoes_source/5`),
			potatoes_source6 = coalesce(potatoes_source6, potatoes_source.6, `potatoes_source/6`),
			potatoes_source7 = coalesce(potatoes_source7, potatoes_source.7, `potatoes_source/7`),
			potatoes_source8 = coalesce(potatoes_source8, potatoes_source.8, `potatoes_source/8`),
			potatoes_source9 = coalesce(potatoes_source9, potatoes_source.9, `potatoes_source/9`),
			potatoes_source66 = coalesce(potatoes_source66, potatoes_source.66, `potatoes_source/66`),
			
			lentils_source0 = coalesce(lentils_source0, lentils_source.0, `lentils_source/0`),
			lentils_source1 = coalesce(lentils_source1, lentils_source.1, `lentils_source/1`),
			lentils_source2 = coalesce(lentils_source2, lentils_source.2, `lentils_source/2`),
			lentils_source3 = coalesce(lentils_source3, lentils_source.3, `lentils_source/3`),
			lentils_source4 = coalesce(lentils_source4, lentils_source.4, `lentils_source/4`),
			lentils_source5 = coalesce(lentils_source5, lentils_source.5, `lentils_source/5`),
			lentils_source6 = coalesce(lentils_source6, lentils_source.6, `lentils_source/6`),
			lentils_source7 = coalesce(lentils_source7, lentils_source.7, `lentils_source/7`),
			lentils_source8 = coalesce(lentils_source8, lentils_source.8, `lentils_source/8`),
			lentils_source9 = coalesce(lentils_source9, lentils_source.9, `lentils_source/9`),
			lentils_source66 = coalesce(lentils_source66, lentils_source.66, `lentils_source/66`),
			
			eggs_source0 = coalesce(eggs_source0, eggs_source.0, `eggs_source/0`),
			eggs_source1 = coalesce(eggs_source1, eggs_source.1, `eggs_source/1`),
			eggs_source2 = coalesce(eggs_source2, eggs_source.2, `eggs_source/2`),
			eggs_source3 = coalesce(eggs_source3, eggs_source.3, `eggs_source/3`),
			eggs_source4 = coalesce(eggs_source4, eggs_source.4, `eggs_source/4`),
			eggs_source5 = coalesce(eggs_source5, eggs_source.5, `eggs_source/5`),
			eggs_source6 = coalesce(eggs_source6, eggs_source.6, `eggs_source/6`),
			eggs_source7 = coalesce(eggs_source7, eggs_source.7, `eggs_source/7`),
			eggs_source8 = coalesce(eggs_source8, eggs_source.8, `eggs_source/8`),
			eggs_source9 = coalesce(eggs_source9, eggs_source.9, `eggs_source/9`),
			eggs_source66 = coalesce(eggs_source66, eggs_source.66, `eggs_source/66`),
			
			dairy_source0 = coalesce(dairy_source0, dairy_source.0, `dairy_source/0`),
			dairy_source1 = coalesce(dairy_source1, dairy_source.1, `dairy_source/1`),
			dairy_source2 = coalesce(dairy_source2, dairy_source.2, `dairy_source/2`),
			dairy_source3 = coalesce(dairy_source3, dairy_source.3, `dairy_source/3`),
			dairy_source4 = coalesce(dairy_source4, dairy_source.4, `dairy_source/4`),
			dairy_source5 = coalesce(dairy_source5, dairy_source.5, `dairy_source/5`),
			dairy_source6 = coalesce(dairy_source6, dairy_source.6, `dairy_source/6`),
			dairy_source7 = coalesce(dairy_source7, dairy_source.7, `dairy_source/7`),
			dairy_source8 = coalesce(dairy_source8, dairy_source.8, `dairy_source/8`),
			dairy_source9 = coalesce(dairy_source9, dairy_source.9, `dairy_source/9`),
			dairy_source66 = coalesce(dairy_source66, dairy_source.66, `dairy_source/66`),
			
			veggies_source0 = coalesce(veggies_source0, veggies_source.0, `veggies_source/0`),
			veggies_source1 = coalesce(veggies_source1, veggies_source.1, `veggies_source/1`),
			veggies_source2 = coalesce(veggies_source2, veggies_source.2, `veggies_source/2`),
			veggies_source3 = coalesce(veggies_source3, veggies_source.3, `veggies_source/3`),
			veggies_source4 = coalesce(veggies_source4, veggies_source.4, `veggies_source/4`),
			veggies_source5 = coalesce(veggies_source5, veggies_source.5, `veggies_source/5`),
			veggies_source6 = coalesce(veggies_source6, veggies_source.6, `veggies_source/6`),
			veggies_source7 = coalesce(veggies_source7, veggies_source.7, `veggies_source/7`),
			veggies_source8 = coalesce(veggies_source8, veggies_source.8, `veggies_source/8`),
			veggies_source9 = coalesce(veggies_source9, veggies_source.9, `veggies_source/9`),
			veggies_source66 = coalesce(veggies_source66, veggies_source.66, `veggies_source/66`),
			
			fruit_source0 = coalesce(fruit_source0, fruit_source.0, `fruit_source/0`),
			fruit_source1 = coalesce(fruit_source1, fruit_source.1, `fruit_source/1`),
			fruit_source2 = coalesce(fruit_source2, fruit_source.2, `fruit_source/2`),
			fruit_source3 = coalesce(fruit_source3, fruit_source.3, `fruit_source/3`),
			fruit_source4 = coalesce(fruit_source4, fruit_source.4, `fruit_source/4`),
			fruit_source5 = coalesce(fruit_source5, fruit_source.5, `fruit_source/5`),
			fruit_source6 = coalesce(fruit_source6, fruit_source.6, `fruit_source/6`),
			fruit_source7 = coalesce(fruit_source7, fruit_source.7, `fruit_source/7`),
			fruit_source8 = coalesce(fruit_source8, fruit_source.8, `fruit_source/8`),
			fruit_source9 = coalesce(fruit_source9, fruit_source.9, `fruit_source/9`),
			fruit_source66 = coalesce(fruit_source66, fruit_source.66, `fruit_source/66`),
			
			fish_source0 = coalesce(fish_source0, fish_source.0, `fish_source/0`),
			fish_source1 = coalesce(fish_source1, fish_source.1, `fish_source/1`),
			fish_source2 = coalesce(fish_source2, fish_source.2, `fish_source/2`),
			fish_source3 = coalesce(fish_source3, fish_source.3, `fish_source/3`),
			fish_source4 = coalesce(fish_source4, fish_source.4, `fish_source/4`),
			fish_source5 = coalesce(fish_source5, fish_source.5, `fish_source/5`),
			fish_source6 = coalesce(fish_source6, fish_source.6, `fish_source/6`),
			fish_source7 = coalesce(fish_source7, fish_source.7, `fish_source/7`),
			fish_source8 = coalesce(fish_source8, fish_source.8, `fish_source/8`),
			fish_source9 = coalesce(fish_source9, fish_source.9, `fish_source/9`),
			fish_source66 = coalesce(fish_source66, fish_source.66, `fish_source/66`),
			
			poultry_source0 = coalesce(poultry_source0, poultry_source.0, `poultry_source/0`),
			poultry_source1 = coalesce(poultry_source1, poultry_source.1, `poultry_source/1`),
			poultry_source2 = coalesce(poultry_source2, poultry_source.2, `poultry_source/2`),
			poultry_source3 = coalesce(poultry_source3, poultry_source.3, `poultry_source/3`),
			poultry_source4 = coalesce(poultry_source4, poultry_source.4, `poultry_source/4`),
			poultry_source5 = coalesce(poultry_source5, poultry_source.5, `poultry_source/5`),
			poultry_source6 = coalesce(poultry_source6, poultry_source.6, `poultry_source/6`),
			poultry_source7 = coalesce(poultry_source7, poultry_source.7, `poultry_source/7`),
			poultry_source8 = coalesce(poultry_source8, poultry_source.8, `poultry_source/8`),
			poultry_source9 = coalesce(poultry_source9, poultry_source.9, `poultry_source/9`),
			poultry_source66 = coalesce(poultry_source66, poultry_source.66, `poultry_source/66`),
			
			goat_sheep_source0 = coalesce(goat_sheep_source0, goat_sheep_source.0, `goat_sheep_source/0`),
			goat_sheep_source1 = coalesce(goat_sheep_source1, goat_sheep_source.1, `goat_sheep_source/1`),
			goat_sheep_source2 = coalesce(goat_sheep_source2, goat_sheep_source.2, `goat_sheep_source/2`),
			goat_sheep_source3 = coalesce(goat_sheep_source3, goat_sheep_source.3, `goat_sheep_source/3`),
			goat_sheep_source4 = coalesce(goat_sheep_source4, goat_sheep_source.4, `goat_sheep_source/4`),
			goat_sheep_source5 = coalesce(goat_sheep_source5, goat_sheep_source.5, `goat_sheep_source/5`),
			goat_sheep_source6 = coalesce(goat_sheep_source6, goat_sheep_source.6, `goat_sheep_source/6`),
			goat_sheep_source7 = coalesce(goat_sheep_source7, goat_sheep_source.7, `goat_sheep_source/7`),
			goat_sheep_source8 = coalesce(goat_sheep_source8, goat_sheep_source.8, `goat_sheep_source/8`),
			goat_sheep_source9 = coalesce(goat_sheep_source9, goat_sheep_source.9, `goat_sheep_source/9`),
			goat_sheep_source66 = coalesce(goat_sheep_source66, goat_sheep_source.66, `goat_sheep_source/66`),
			
			beef_source0 = coalesce(beef_source0, beef_source.0, `beef_source/0`),
			beef_source1 = coalesce(beef_source1, beef_source.1, `beef_source/1`),
			beef_source2 = coalesce(beef_source2, beef_source.2, `beef_source/2`),
			beef_source3 = coalesce(beef_source3, beef_source.3, `beef_source/3`),
			beef_source4 = coalesce(beef_source4, beef_source.4, `beef_source/4`),
			beef_source5 = coalesce(beef_source5, beef_source.5, `beef_source/5`),
			beef_source6 = coalesce(beef_source6, beef_source.6, `beef_source/6`),
			beef_source7 = coalesce(beef_source7, beef_source.7, `beef_source/7`),
			beef_source8 = coalesce(beef_source8, beef_source.8, `beef_source/8`),
			beef_source9 = coalesce(beef_source9, beef_source.9, `beef_source/9`),
			beef_source66 = coalesce(beef_source66, beef_source.66, `beef_source/66`),
			
			oil_source0 = coalesce(oil_source0, oil_source.0, `oil_source/0`),
			oil_source1 = coalesce(oil_source1, oil_source.1, `oil_source/1`),
			oil_source2 = coalesce(oil_source2, oil_source.2, `oil_source/2`),
			oil_source3 = coalesce(oil_source3, oil_source.3, `oil_source/3`),
			oil_source4 = coalesce(oil_source4, oil_source.4, `oil_source/4`),
			oil_source5 = coalesce(oil_source5, oil_source.5, `oil_source/5`),
			oil_source6 = coalesce(oil_source6, oil_source.6, `oil_source/6`),
			oil_source7 = coalesce(oil_source7, oil_source.7, `oil_source/7`),
			oil_source8 = coalesce(oil_source8, oil_source.8, `oil_source/8`),
			oil_source9 = coalesce(oil_source9, oil_source.9, `oil_source/9`),
			oil_source66 = coalesce(oil_source66, oil_source.66, `oil_source/66`),
			
			sugar_source0 = coalesce(sugar_source0, sugar_source.0, `sugar_source/0`),
			sugar_source1 = coalesce(sugar_source1, sugar_source.1, `sugar_source/1`),
			sugar_source2 = coalesce(sugar_source2, sugar_source.2, `sugar_source/2`),
			sugar_source3 = coalesce(sugar_source3, sugar_source.3, `sugar_source/3`),
			sugar_source4 = coalesce(sugar_source4, sugar_source.4, `sugar_source/4`),
			sugar_source5 = coalesce(sugar_source5, sugar_source.5, `sugar_source/5`),
			sugar_source6 = coalesce(sugar_source6, sugar_source.6, `sugar_source/6`),
			sugar_source7 = coalesce(sugar_source7, sugar_source.7, `sugar_source/7`),
			sugar_source8 = coalesce(sugar_source8, sugar_source.8, `sugar_source/8`),
			sugar_source9 = coalesce(sugar_source9, sugar_source.9, `sugar_source/9`),
			sugar_source66 = coalesce(sugar_source66, sugar_source.66, `sugar_source/66`)
		) %>%
		select(
			-c(
				# reduce_meals.1, reduce_meals_1,
				# not_eat.1, not_eat_1,
				
				
				gather_scraps_dead1, gather_scraps_dead.1, # this is actually gather_wood_dead
				
				time_child_gathering_nonwood_ite, time_child_gathering_non.wood_items, `time_child_gathering_non-wood_items`,
				contains("_lifestyle.1"),
				contains("_lifestyle.2"),
				contains("_lifestyle.3"),
				contains("_lifestyle.4"),
				contains("_lifestyle.5"),
				contains("_lifestyle.6"),
				contains("_lifestyle/1"),
				contains("_lifestyle/2"),
				contains("_lifestyle/3"),
				contains("_lifestyle/4"),
				contains("_lifestyle/5"),
				contains("_lifestyle/6"),
				
				contains("_action.1"),
				contains("_action.2"),
				contains("_action.3"),
				contains("_action.4"),
				contains("_action.5"),
				contains("_action.6"),
				contains("_action.7"),
				contains("_action.8"),
				contains("_action.9"),
				contains("_action.10"),
				contains("_action.11"),
				contains("_action.12"),
				contains("_action.13"),
				contains("_action.66"),
				contains("_action.88"),
				contains("_action/1"),
				contains("_action/2"),
				contains("_action/3"),
				contains("_action/4"),
				contains("_action/5"),
				contains("_action/6"),
				contains("_action/7"),
				contains("_action/8"),
				contains("_action/9"),
				contains("_action/10"),
				contains("_action/11"),
				contains("_action/12"),
				contains("_action/13"),
				contains("_action/66"),
				contains("_action/88"),
				
				
				contains("_source.0"),
				contains("_source.1"),
				contains("_source.2"),
				contains("_source.3"),
				contains("_source.4"),
				contains("_source.5"),
				contains("_source.6"),
				contains("_source.7"),
				contains("_source.8"),
				contains("_source.9"),
				contains("_source.66"),
				contains("_source/0"),
				contains("_source/1"),
				contains("_source/2"),
				contains("_source/3"),
				contains("_source/4"),
				contains("_source/5"),
				contains("_source/6"),
				contains("_source/7"),
				contains("_source/8"),
				contains("_source/9"),
				contains("_source/66")
			)
		) %>%
		
		# clean up values
		mutate(
			camp_id = 
				str_to_upper(str_trim(camp_id)), # could use stringr in case there are other mis-matches
			subblock_id = 
				str_to_upper(str_trim(subblock_id)), # could use stringr in case there are other mis-matches
			name_mahji = 
				str_to_title(str_trim(name_mahji)),
			name_respondent = 
				str_to_title(str_trim(name_respondent)),
			name_hh_head = 
				str_to_title(str_trim(name_hh_head)),
			target_child_name = 
				str_to_title(str_trim(target_child_name)),
		) %>%
		
		# rename vars
		# rename(newname, oldname) renames (not copy and rename new column)
		rename( 
			
			
			# time_child_gathering_nonwood_items = time_child_gathering_non.wood_items, # corrected above
			respondent_resp_rate  = resp_rate_reported_respondant,
			respondent_weight_loss = weight_loss_reported_respondant,
			
			# # coping strategies food and fuel sections both have "reduce_meals" and "not_eat" vars
			reduce_meals_lack_food = reduce_meals,
			not_eat_lack_food = not_eat,
			reduce_meals_lack_fuel = reduce_meals1,
			not_eat_lack_fuel = not_eat1,
			
			fuel_ever_gather_scraps = fuel_ever_scraps,
			fuel_30_gather_scraps = fuel_30_scraps,
			
			
			## When you depend on wood for cooking fuel, then how many times in one week do you go to the forest to collect wood?
			collect_wood_times_week = times_wood_day,
			reason_forest_defecation = reason_forest_defacation,
			cook_to_sell_percent = cook_to_sell,
			cook_sell_days_week = cook_sell_yesterday
		) # %>%
	# select(-time_child_gathering_nonwood_ite)
}
