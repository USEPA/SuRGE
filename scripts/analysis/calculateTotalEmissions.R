#1.  Merge diffusive and ebullitive emissions
# OUT2 created in "calculateDiffusion.R"


dim(OUT2) #1835 observations
unique(OUT2$lake_id) # 
OUT2 <- OUT2 %>% filter(!is.na(lake_id)) # 120 present, cuts this down to 1715
dim(eb_results) #1797 observation
emissions <- full_join(eb_results, OUT2)
dim(emissions) #1867




# CALCULATE TOTAL EMISSION RATES------------------
# Only calculate if both diff and ebul were quantified
# tot = NA if is_na(ebul) or is_na(diff)_
emissions <- mutate(emissions, 
                     co2_trate_mg_h = co2_drate_mg_h_best + co2_erate_mg_h,
                     ch4_trate_mg_h = ch4_drate_mg_h_best + ch4_erate_mg_h)

# FORMAT TO BE CONSISTENT WITH CHEM DATA
emissions <- emissions %>%
  # adopt clearer names.  Remove units
  rename_with(~gsub("erate_mg_h", "ebullition", .),
              contains("erate_mg_h")) %>%
  rename_with(~gsub("drate_mg_h_best", "diffusion_best", .),
              contains("drate_mg_h_best")) %>%
  rename_with(~gsub("trate_mg_h", "total", .),
              contains("trate_mg_h")) %>%
  rename(volumetric_ebullition = eb_ml_hr_m2) %>%
  # define units
  mutate(ch4_ebullition_units = "mg_ch4_m2_h",
         co2_ebullition_units = "mg_co2_m2_h",
         n2o_ebullition_units = "mg_n2o_m2_h",
         volumetric_ebullition_units = "ml_m2_h",
         ch4_diffusion_units = "mg_ch4_m2_h",
         co2_diffusion_units = "mg_co2_m2_h",
         ch4_total_units = "mg_ch4_m2_h",
         co2_total_units = "mg_co2_m2_h",
         co2_deployment_length_units = "s",
         ch4_deployment_length_units = "s",
         air_temp_units = "degrees celcius") %>%
  # arrange columns
  select(lake_id, visit, site_id, air_temp, air_temp_units, # these first
         sort(tidyselect::peek_vars())) # all others alphabetical

# # CO2 EQUIVALENTS------------------
# source("ohio2016/scriptsAndRmd/co2Equiv.R")
# eqAreaData <- mutate(eqAreaData,
#                      ch4Co2eq = co2Equiv(eqAreaData$ch4.trate.mg.h, 
#                                          choice1 = "CH4"),
#                      co2Co2eq = eqAreaData$co2.trate.mg.h,
#                      totCo2eq = ch4Co2eq + co2Co2eq)