clear 
import excel "C:\Users\nuhu.amin\Desktop\Fuel data review\RohingyaFuelMaster_today.xlsx", sheet("RohingyaFuelMaster_today") firstrow

*September 23, 2019
*replace food_wrong_meat= 1 if food_wrong_meat==3 and hh_id==8wDI18100999 ("food_wrong_meat" not found)
replace floor_material= "5" if floor_material=="1" & hh_id=="8wDI20101395"

replace hh_per_structure= "7" if hh_per_structure=="9" & hh_id=="8wDI20101239" 
replace rice_adults_week= "7" if rice_adults_week=="6" & hh_id=="8wDI20101392"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wDI21101493"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wDI21101220"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wDI21101214"
replace rice_adults_week= "7" if rice_adults_week=="2" & hh_id=="8wDI21101492"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wDI21101788_227"
replace income_wage_labor="3500" if income_wage_labor =="35000" & hh_id=="8wAA17122929_888"

replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wBI15102330_1117"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wBI15102312_1120"
replace rice_adults_week= "7" if rice_adults_week=="3" & hh_id=="8wBI15102369_1104"

replace hh_size="6" if hh_size=="10" &  name_respondant=="Janowara Begum"
replace floor_material="5" if floor_material=="3" &  name_respondant=="Asiya"

replace hh_per_structure= "7" if hh_per_structure=="9" & hh_id=="8wDI20101239" 
replace income_wage_labor="3500" if income_wage_labor =="35000" & hh_id=="8wAA17122929_888"
replace floor_material="5" if floor_material=="3" & hh_id=="8wAA41123859_2182"
replace floor_material="5" if floor_material=="3" & hh_id=="8wAA41123859_2182"
replace floor_material="5" if floor_material=="7" & &  name_respondant=="Ferdus"

*floor_material (*****ID not matched WHY*******)
*replace floor_material="5" if floor_material=="3" & hh_id=="8wAA41123859_2182"
*replace floor_material="5" if floor_material=="7" & hh_id=="8wBA15124088_1778"
*replace floor_material="5" if floor_material=="7" & hh_id=="8wBA15119364_1768"
*replace floor_material="5" if floor_material=="7" & hh_id=="8wBA15116103_1766"

replace rice_adults_week= "7" if rice_adults_week=="0" & hh_id=="8wB122637A16_2453"
replace hh_size_5_18="5" if hh_size_5_18=="6" & hh_id=="8wDI18100999"
replace hh_size="7" if hh_size=="8" & hh_id=="8wDH21290455"
replace hh_size_o18="1" if hh_size_o18=="2" & hh_id=="8wBI19101048_516"
replace hh_size_o40="1" if hh_size_o40=="2" & hh_id=="8wBI19101048_516"
replace hh_size_o40="1" if hh_size_o40=="0" & hh_id=="8wBI12101667_"
replace hh_size="1" if hh_size=="7" & hh_id=="8wBA17124230_912"
replace hh_size_2mo_u5="2" if hh_size_2mo_u5=="1" & hh_id=="8wBA17124230_912"
replace hh_size="8" if hh_size=="9" & hh_id=="8wAA27113811_1294"
replace hh_size_5_18="3" if hh_size_5_18=="5" & hh_id=="8wA112648_1636"
replace hh_size="0" if hh_size=="3" & hh_id=="8wA123504_1652"
replace rice_adults_week= "7" if rice_adults_week=="0" & hh_id=="8wB102959I14_2594"
replace hh_size_5_18="1" if hh_size_5_18=="2" & hh_id=="8wB102722i14_2614"
replace hh_size_5_18="2" if hh_size_5_18=="0" & hh_id=="10D109224D11_2750"
replace rice_adults_week= "7" if rice_adults_week=="4" & hh_id=="8wA115741A30_"
replace spent_total_month="6000" if spent_total_month=="60002" & hh_id=="8wB102734I14_2593"
replace camp_id="10" if camp_id=="Camp10" & hh_id=="Camp10D193582D12_2869"
replace camp_id="10" if camp_id=="Camp 10" & hh_id=="Camp 10D193591D12_2896"
replace subblock_id="D12" if subblock_id=="12" & hh_id=="10D19273012_2868"
replace hh_size="4" if hh_size=="8" & hh_id=="10G112100G9_"

