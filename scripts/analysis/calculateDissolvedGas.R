# SCRIPT FOR CALCULATING OBSERVED AND SATURATED DISSOLVED GAS CONCENTRATIONS
# USES FUNCTIONS FROM FORK OF NEONScience NEON-dissolved-gas PACKAGE.  

# Load function
source("scripts/analysis/def.calc.sdg.R")


# def.calc.sdg.R
# Get GC data from gc_lakeid_agg
gc_lakeid_agg %>% 
  filter(type %in% c("dg", "air")) %>%
  select(lake_id, site_id, visit, type, n2o_ppm, co2_ppm, ch4_ppm) %>% 
  # exetainer codes for air samples are arbitratirly written in first
  # row of spreadsheet and may not correspond to correct site_id. Set 
  # to NA here, then inherit site_id from dissolved gas sampling location
  mutate(site_id = case_when(type == "air" ~ NA_real_,
                             type == "dg" ~ site_id,
                             TRUE ~ 999999999)) %>%
  # assign dissolved gas sampling site to air sampling site
  group_by(lake_id, visit) %>%
  fill(site_id, .direction = "downup") %>%
  #ungroup %>%
  pivot_longer(!c(lake_id, site_id, visit, type)) %>% filter(lake_id == "239")
  summarize(n=n())
  
  #filter(lake_id == "1") %>% # development
  pivot_wider(names_from = c(type, name), values_from = value)
  


dissolved_gas <- with(eqAreaData, def.calc.sdg(inputFile = eqAreaData, 
                                            volGas = air_vol, volH2O = water_vol, 
                                            baro = atm_pressure, # units? 
                                            waterTemp = Tmp_C_S, # lake temp     ###### update
                                            headspaceTemp = dg_extn_temp, # use lake temp
                                            eqCO2 = dissolved_co2.ppm,           ###### update
                                            sourceCO2 = 0, # measured air CO2
                                            airCO2 = 405, # measured air CO2
                                            eqCH4 = dissolved_ch4.ppm,          ###### update
                                            sourceCH4 = 0, # measured air ch4 
                                            airCH4 = 1.85, # measured air ch4
                                            eqN2O = dissolved_n2o.ppm, 
                                            sourceN2O = 0, # measured air n2o 
                                            airN2O = 0.33)) %>% # measured air n2o
  mutate(co2.sat.ratio = dissolvedCO2 / satCO2,
         ch4.sat.ratio = dissolvedCH4 / satCH4,
         n2o.sat.ratio = dissolvedN2O / satN2O) %>%
  rename(dissolved.co2 = dissolvedCO2,
         sat.co2 = satCO2,
         dissolved.ch4 = dissolvedCH4,
         sat.ch4 = satCH4,
         dissolved.n2o = dissolvedN2O,
         sat.n2o = satN2O)
